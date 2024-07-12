#' Import JSON Module UI
#'
#' @param id Module ID
#' @return Shiny UI elements
#'
#' @export
importModuleUI <- function(id) {
  ns <- NS(id)
  fileInput(ns("file"), "Import JSON",
    accept = c("application/json", "text/json", ".json")
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
