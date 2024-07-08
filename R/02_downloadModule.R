#' Download JSON Module UI
#'
#' @param id Module ID
#' @return Shiny UI elements
#'
#' @export
downloadModuleUI <- function(id) {
  ns <- NS(id)
  downloadButton(ns("download"), "Download JSON")
}

#' Download JSON Module Server
#'
#' @param id Module ID
#' @param graph reactive graph object to be converted to JSON
#' @return None
#'
#' @export
downloadModuleServer <- function(id, graph) {
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
