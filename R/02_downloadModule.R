#' Download JSON Module UI
#'
#' @param id Module ID
#' @return Shiny UI elements
downloadModuleUI <- function(id) {
  ns <- NS(id)
  downloadButton(ns("download"), "Download JSON")
}

#' Download JSON Module Server
#'
#' @param id Module ID
#' @param graph reactive graph object to be converted to JSON
#' @return None
downloadModuleServer <- function(id, graph) {
  moduleServer(
    id,
    function(input, output, session) {
      output$download <- downloadHandler(
        filename = function() {
          paste("traceR-graph-", Sys.Date(), ".json", sep = "")
        },
        content = function(file) {
          graph_list <- sapply(graph(), function(x) x)
          write_json(graph_list, path = file, pretty = TRUE)
        }
      )
    }
  )
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
