#' Creates a graph from a (uploaded) list
#'
#' @param graph_list list, containing all elements required for the graph
createGraphFromUpload <- function(graph_list) {
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
updateGraph <- function(graph, uploadedGraph) {
  observe({
    req(uploadedGraph())
    logDebug("Update graph")
    new_graph <- createGraphFromUpload(graph_list = uploadedGraph())
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
    inputIDs <- names(uploaded_inputs())
    inputIDs <- inputIDs[inputIDs %in% names(input)]

    for (i in 1:length(inputIDs)) {
      session$sendInputMessage(inputIDs[i],  list(value = uploaded_inputs()[[inputIDs[i]]]) )
    }
  }) %>% bindEvent(uploaded_inputs())
}
