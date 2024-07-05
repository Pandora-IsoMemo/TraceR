#' Download JSON Module UI
#'
#' @param id Module ID
#' @return Shiny UI elements
#'
#' @export
downloadModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download"), "Download JSON"),
    tags$br(),
    checkboxInput(
      inputId = ns("download_inputs"),
      label = "Download user inputs and graph",
      value = FALSE
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.download_inputs == true",
      downloadModelUI(
        id = ns("session_download"),
        label = "Download Session"
      ),
      tags$hr()
    )
  )
}

#' Download JSON Module Server
#'
#' @param id Module ID
#' @param graph reactive graph object to be converted to JSON
#' @param upload_description reactive description of the upload
#' @return None
#'
#' @export
downloadModuleServer <- function(id, graph, upload_description) {
  moduleServer(
    id,
    function(input, output, session) {
      output$download <- downloadHandler(
        filename = function() {
          paste("traceR-graph-", Sys.Date(), ".json", sep = "")
        },
        content = function(file) {
          graph() %>%
            asGraphList() %>%
            write_json(path = file, pretty = TRUE)
        }
      )

      # export inputs and graph
      downloadModelServer("session_download",
                          dat = reactive(asGraphList(graph())),
                          inputs = input,
                          model = reactive(NULL),
                          rPackageName = config()[["rPackageName"]],
                          defaultFileName = config()[["defaultFileName"]],
                          fileExtension = config()[["fileExtension"]],
                          modelNotes = upload_description,
                          triggerUpdate = reactive(TRUE),
                          onlySettings = TRUE)
    }
  )
}

#' Convert Graph to List
#'
#' @param graph Graph object
#' @return List
asGraphList <- function(graph) {
  graph_list <- sapply(graph, function(x) x)
  graph_list
}

### EXAMPLE
# ui <- fluidPage(
#   titlePanel("Download JSON Example"),
#   sidebarLayout(
#     sidebarPanel(
#       downloadModuleUI("jsonDownload")
#     )
#   )
# )
#
# server <- function(input, output) {
#   graph <- reactiveVal()
#   graph(list(
#     name = "John Doe",
#     age = 30,
#     email = "johndoe@example.com",
#     interests = c("Reading", "Traveling", "Swimming")
#   ))
#   downloadModuleServer("jsonDownload", graph=graph)
# }
#
# shinyApp(ui = ui, server = server)
