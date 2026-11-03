#' Download JSON Module UI
#'
#' @param id Module ID
#' @param label Button label
#' @return Shiny UI elements
#'
#' @export
downloadModuleUI <- function(id, label) {
  deprecate_warn(
    "26.11.1",
    "TraceR::downloadModuleUI()",
    details = "Graph JSON downloads are no longer provided by the app."
  )
  .download_module_ui(id, label)
}

.download_module_ui <- function(id, label) {
  ns <- NS(id)
  downloadButton(ns("download"), label)
}

#' Dynamically add ui elements for downloadButton
#'
#' @param output shiny session output object
#' @param private_key Private key used for the conditional logic to add a signed JSON
#' download button or not
#'
#' @export
createDownloadModuleUI <- function(output, private_key) {
  deprecate_warn(
    "26.11.1",
    "TraceR::createDownloadModuleUI()",
    details = "Graph JSON download controls are no longer provided by the app."
  )
  output$conditional_download_buttons <- renderUI({
    if (
      !is.na(Sys.getenv("SHINYPROXY_USERID", unset = NA)) &&
        !is.null(private_key)
    ) {
      tagList(
        .download_module_ui("download_unsigned", label = "Download JSON"),
        tags$br(),
        tags$br(),
        .download_module_ui("download_signed", label = "Download Signed JSON")
      )
    } else {
      .download_module_ui("download_unsigned", label = "Download JSON")
    }
  })
}

#' Download JSON Module Server
#'
#' @param id Module ID
#' @param graph reactive graph object to be converted to JSON
#' @param private_key private key
#' @param signed logical indicating if a signed version should be exported
#' @return None
#'
#' @export
downloadModuleServer <- function(id, graph, private_key, signed) {
  deprecate_warn(
    "26.11.1",
    "TraceR::downloadModuleServer()",
    details = "This module exports the legacy DiagrammeR graph JSON format."
  )
  moduleServer(
    id,
    function(input, output, session) {
      output$download <- downloadHandler(
        filename = function() {
          paste("traceR-graph-", Sys.Date(), ".json", sep = "")
        },
        content = function(file) {
          graph_list <- .as_graph_list(graph())
          if (signed) {
            # Convert to json
            json_data <- graph_list %>%
              toJSON(pretty = TRUE, auto_unbox = TRUE)

            # Create a signature and encode in base64 for easier handling
            signature <- signature_create(data = charToRaw(json_data), key = private_key)
            encoded_signature <- base64_encode(signature)

            # Append the signature to the original data
            signed_data <- c(graph_list, c(signature = encoded_signature))

            # Write to json file
            signed_data %>% write_json(path = file, pretty = TRUE)
          } else {
            # Write to json file
            graph_list %>%
              write_json(path = file, pretty = TRUE)
          }
        }
      )
    }
  )
}

#' Convert Graph to List
#'
#' @param graph Graph object
#' @return List
#'
#' @export
asGraphList <- function(graph) {
  deprecate_warn(
    "26.11.1",
    "TraceR::asGraphList()",
    details = "Use tracer_table_to_json() to serialize network table data."
  )
  .as_graph_list(graph)
}

.as_graph_list <- function(graph) {
  sapply(graph, function(x) x)
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
