#' Creates a graph from a (uploaded) list
#'
#' @param graph_list list, containing all elements required for the graph
createGraphFromUpload <- function(graph_list) {
  deprecate_warn(
    "26.11.1",
    "TraceR::createGraphFromUpload()",
    details = "Graph-to-table migration is not supported; import tracer_table JSON instead."
  )
  .create_graph_from_upload(graph_list)
}

.create_graph_from_upload <- function(graph_list) {
  tryCatch(
    {
      graph <-
        create_graph() %>%
        add_node_df(
          node_df = graph_list$nodes_df
        ) %>%
        add_edge_df(
          edge_df = graph_list$edges_df
        ) %>%
        set_node_attr_to_display(attr = NULL, nodes = NULL, default = "label")

      graph$graph_log <- graph_list$graph_log
      graph$graph_log$time_modified <- as.POSIXct(graph$graph_log$time_modified)
      graph$graph_info$graph_time <- as.POSIXct(graph$graph_info$graph_time)

      graph
    },
    error = function(e) {
      message <- paste("An error occurred while converting file to graph:", e$message, sep = "\n")
      shinyalert(
        title = "Error",
        text = message,
        type = "error"
      )
      return(NULL)
    }
  )
}

#' Updates the reactive graph element with the uploaded graph
#'
#' @param graph reactive, main graph of the app
#' @param uploadedGraph reactive, uploaded graph
#'
#' @export
updateGraph <- function(graph, uploadedGraph) {
  deprecate_warn(
    "26.11.1",
    "TraceR::updateGraph()",
    details = "The Network graph is now derived from the shared tracer_table."
  )
  observe({
    req(uploadedGraph())
    logDebug("Update graph")
    new_graph <- .create_graph_from_upload(graph_list = uploadedGraph())
    graph(new_graph)
  })
}

#' Update input with uploaded input
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param uploaded_inputs reactive, uploaded inputs
#'
#' @export
updateInput <- function(input, output, session, uploaded_inputs) {
  observe({
    logDebug("updateInput: Send uploaded_inputs")

    ## update inputs ----
    input_ids <- names(uploaded_inputs())
    input_ids <- input_ids[input_ids %in% names(input)]

    for (input_id in input_ids) {
      session$sendInputMessage(input_id, list(value = uploaded_inputs()[[input_id]]))
    }
  }) %>% bindEvent(uploaded_inputs())
}
