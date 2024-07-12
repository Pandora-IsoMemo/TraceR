#' Displays the node Id that was clicked
#'
#' @param input server input object
#' @param output server output object
#' @param outputId outputId of element for which the clicked Id is shown
#'
#' @export
displayNodeId <- function(input, output, outputId) {
  output[[outputId]] <- renderText({
    req(input$flowchart_click)
    paste0("You clicked the node, which has the ID ", input$flowchart_click$id[[1]], ".")
  })
}
