#' @rawNamespace import(shiny)
#' @rawNamespace import(DiagrammeR)

#' @importFrom jsonlite write_json fromJSON
#' @importFrom shinyalert shinyalert

utils::globalVariables(
  c(
    ".",
    "total_degree",
    "fillcolor",
    "betweenness",
    "height",
    "peripheries",
    "penwidth"
  )
)

NULL
