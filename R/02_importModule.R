#' Import JSON Module UI
#'
#' @param id Module ID
#' @return Shiny UI elements
importModuleUI <- function(id) {
  ns <- NS(id)

  tagList(
    fileInput(ns("file"), "Import JSON",
      accept = c("application/json", "text/json", ".json")
    ),
    importDataUI(ns("session_import"), label = "Import Session file"),
  )
}

#' Import JSON Module Server
#'
#' @param id Module ID
#' @return uploaded data
importModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      data <- reactiveValues(graph = NULL,
                             input = NULL)

      # Load session
      uploaded_session <- importDataServer(
        "session_import",
        importType = "model",
        defaultSource = config()[["defaultSource"]],
        ckanFileTypes = config()[["ckanFileTypes"]],
        fileExtension = config()[["fileExtension"]],
        onlySettings = TRUE,
        options = importOptions(rPackageName = config()[["rPackageName"]])
      )

      observe({
        req(length(uploaded_session()) > 0)
        logDebug("importModuleServer: Update graph and inputs")

        # Update data
        req(!is.null(uploaded_session()[[1]][["data"]]))
        data$graph <- uploaded_session()[[1]][["data"]]

        # Update input if data is available
        req(!is.null(uploaded_session()[[1]][["inputs"]]))
        data$input <- uploaded_session()[[1]][["inputs"]]
      }) %>%
        bindEvent(uploaded_session())

      # import data from JSON file
      observe({
        req(input$file)
        logDebug("importModuleServer: Update graph from JSON file")

        infile <- input$file
        res <- tryCatch(
          {
            fromJSON(infile$datapath)
          },
          error = function(e) {
            message <- paste("An error occurred while reading the file:", e$message, sep = "\n")
            shinyalert(
              title = "Error",
              text = message,
              type = "error"
            )
            return(NULL)
          }
        )

        data$graph <- res
      }) %>%
        bindEvent(input$file)

      return(data)
    }
  )
}
