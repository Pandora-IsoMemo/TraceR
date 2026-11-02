#' Displays the node Id that was clicked
#'
#' @param input server input object
#' @param output server output object
#' @param outputId outputId of element for which the clicked ID is shown
#' @param graph Optional reactive DiagrammeR graph used to map graph IDs to
#'   original TraceR IDs
#'
#' @export
displayNodeId <- function(input, output, outputId, graph = NULL) {
  output[[outputId]] <- renderText({
    req(input$flowchart_click)
    clicked_id <- input$flowchart_click$id[[1]]
    if (!is.null(graph) && !is.null(graph())) {
      nodes <- DiagrammeR::get_node_df(graph())
      node_index <- match(as.character(clicked_id), as.character(nodes$id))
      if (!is.na(node_index) && "tracer_id" %in% names(nodes)) {
        clicked_id <- nodes$tracer_id[[node_index]]
      }
    }
    paste0("You clicked the node, which has the ID ", clicked_id, ".")
  })
}
