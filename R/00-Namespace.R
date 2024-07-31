#' @rawNamespace import(shiny)
#' @rawNamespace import(DiagrammeR)

#' @importFrom DataTools downloadModelServer downloadModelUI extractNotes extractObjectFromFile
#'  importDataUI importDataServer importOptions
#' @importFrom magrittr "%>%"
#' @importFrom openssl signature_create
#' @importFrom jsonlite write_json fromJSON
#' @importFrom shinyalert shinyalert
#' @importFrom yaml read_yaml

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
