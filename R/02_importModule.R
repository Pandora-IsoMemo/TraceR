#' Import JSON Module UI
#'
#' @param id Module ID
#' @return Shiny UI elements
#'
#' @export
importModuleUI <- function(id) {
  ns <- NS(id)

  tagList(
    importDataUI(ns("file_import"), label = "Import JSON"),
    tags$br(), tags$br(),
    importDataUI(ns("session_import"), label = "Import Session")
  )
}

#' Import JSON Module Server
#'
#' @param id Module ID
#' @return uploaded data
#'
#' @export
importModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      data <- reactiveValues(graph = NULL,
                             input = NULL)

      # import data from JSON file from Pandora, url, file
      uploaded_file <- importDataServer(
        "file_import",
        importType = "list",
        defaultSource = config()[["defaultSource"]][["file"]],
        ckanFileTypes = config()[["ckanFileTypes"]][["file"]],
        fileExtension = config()[["fileExtension"]][["file"]]
      )

      observe({
        req(length(uploaded_file()) > 0)
        logDebug("importModuleServer: Update graph")

        # update data
        req(!is.null(uploaded_file()[[1]]))
        data$graph <- uploaded_file()[[1]]
      }) %>%
        bindEvent(uploaded_file())

      # load session
      uploaded_session <- importDataServer(
        "session_import",
        title = "Session import",
        importType = "model",
        defaultSource = config()[["defaultSource"]][["session"]],
        ckanFileTypes = config()[["ckanFileTypes"]][["session"]],
        fileExtension = config()[["fileExtension"]][["session"]],
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

      return(data)
    }
  )
}
