#' Creates a graph from a (uploaded) list
#'
#' @param graph_list list, containing all elements required for the graph
createGraphFromUpload <- function(graph_list) {
  graph <-
    create_graph() %>%
    add_node_df(
      node_df = graph_list$nodes_df
    ) %>%
    add_edge_df(
      edge_df = graph_list$edges_df
    )

  graph$graph_log <- graph_list$graph_log
}
