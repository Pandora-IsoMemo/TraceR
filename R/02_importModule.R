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

      data <- reactive({
        req(input$file)
        infile <- input$file
        tryCatch(
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
      })
    }
  )
}
