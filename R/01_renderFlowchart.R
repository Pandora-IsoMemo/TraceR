#' Render flowchart
#'
#' @param graph reactive graph object
#' @return rendered flowchart
#' @export
renderFlowchart <- function(graph) {
  req(graph())
  tryCatch(
    {
      render_graph(graph(), as_svg = TRUE)
    },
    error = function(e) {
      message <- paste("An error occurred while creating the graph:", e$message, sep = "\n")
      shinyalert(
        title = "Error",
        text = message,
        type = "error"
      )
      return(NULL)
    }
  )
}
