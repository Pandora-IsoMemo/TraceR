#' Creates a graph from a list
#'
#' @param graph_list list, containing all elements required for the graph
create_graph_from_list <- function(graph_list){
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


# graph_list <- sapply(example_graph, function(x) x)
# graph_json <- toJSON(graph_list,na = "string", pretty = TRUE)
# loaded_graph <- fromJSON(paste(graph_json, collapse = "\n"))
