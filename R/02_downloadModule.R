#' Download JSON Module UI
#'
#' @param id Module ID
#' @return Shiny UI elements
#'
#' @export
downloadModuleUI <- function(id, label) {
  ns <- NS(id)
  downloadButton(ns("download"), label)
}

createDownloadModuleUI <- function(output, user_id) {
  output$conditional_download_buttons <- renderUI({
    if (!is.na(user_id)) {
      tagList(
        downloadModuleUI("download_unsigned", label = "Download JSON"),
        tags$br(),
        tags$br(),
        downloadModuleUI("download_signed", label = "Download Signed JSON")
      )
    } else {
      downloadModuleUI("download_unsigned", label = "Download JSON")
    }
  })
}

#' Download JSON Module Server
#'
#' @param id Module ID
#' @param graph reactive graph object to be converted to JSON
#' @return None
#'
#' @export
downloadModuleServer <- function(id, graph, private_key, signed) {
  moduleServer(
    id,
    function(input, output, session) {
      output$download <- downloadHandler(
        filename = function() {
          paste("traceR-graph-", Sys.Date(), ".json", sep = "")
        },
        content = function(file) {
          graph_list <- graph() %>%
            asGraphList()
          if (signed) {
            # Convert to json
            json_data <- graph_list %>%
              jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

            # Create a signature and encode in base64 for easier handling
            signature <- openssl::signature_create(data = charToRaw(json_data), key = private_key)
            encoded_signature <- openssl::base64_encode(signature)

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
