#' Network module UI
#'
#' @param id Module ID
#' @return Network graph UI
#' @export
mod_network_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    uiOutput(ns("graph_status")),
    uiOutput(ns("flowchart_container")),
    textOutput(ns("clickMessage")),
    tags$hr()
  )
}

#' Network sidebar UI
#'
#' Deprecated compatibility wrapper for [mod_sidebar_ui()].
#'
#' @param id Module ID
#' @return Shared sidebar controls UI
#' @seealso [mod_sidebar_ui()]
#' @export
mod_network_sidebar_ui <- function(id) {
  mod_sidebar_ui(id)
}

#' Network module server
#'
#' @param id Module ID
#' @param private_key Deprecated; graph JSON downloads are provided by the sidebar
#' @param public_key Deprecated; retained for compatibility
#' @param current_table Reactive value containing the shared `tracer_table`
#' @return Reactive DiagrammeR graph derived from `current_table`
#' @export
mod_network_server <- function(id, private_key = NULL, public_key = NULL, current_table = NULL) {
  moduleServer(id, function(input, output, session) {
    if (is.null(current_table)) {
      current_table <- reactive(new_tracer_table())
    }

    graph <- reactive({
      table <- current_table()
      validation <- validate_tracer_table(table)
      if (nrow(validation$errors) > 0L || nrow(validation$warnings) > 0L) {
        return(NULL)
      }

      graph_tables <- as.graph_tables(table)
      result <- DiagrammeR::create_graph(directed = TRUE) |>
        DiagrammeR::add_global_graph_attrs(attr = "layout", value = "dot", attr_type = "graph") |>
        DiagrammeR::add_global_graph_attrs(attr = "rankdir", value = "TB", attr_type = "graph") |>
        DiagrammeR::add_node_df(graph_tables$nodes)
      if (nrow(graph_tables$edges) > 0L) {
        result <- DiagrammeR::add_edge_df(result, graph_tables$edges)
      }
      result
    })

    output$graph_status <- renderUI({
      validation <- validate_tracer_table(current_table())
      issues <- c(validation$errors$message, validation$warnings$message)
      if (length(issues) == 0L) return(NULL)

      tags$div(
        class = if (nrow(validation$errors) > 0L) "alert alert-danger" else "alert alert-warning",
        role = "alert",
        tags$strong("Network not rendered because the table is invalid"),
        tags$ul(lapply(issues, tags$li))
      )
    })

    output$flowchart_container <- renderUI({
      if (is.null(graph())) {
        return(tags$div(
          class = "network-placeholder",
          "Graph unavailable until the table has no validation errors or warnings."
        ))
      }

      DiagrammeR::grVizOutput(session$ns("flowchart"), width = "800px", height = "800px")
    })

    output$flowchart <- DiagrammeR::renderGrViz({
      withProgress(renderFlowchart(graph),
        message = "Rendering graph...",
        value = 0.75
      )
    })

    displayNodeId(input, output, outputId = "clickMessage", graph = graph)
    initialize_graph_zooming(session$ns("flowchart_container"))

    graph
  })
}