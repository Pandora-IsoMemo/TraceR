#' @rawNamespace import(shiny)
#' @rawNamespace import(DiagrammeR)

#' @importFrom DataTools downloadModelServer downloadModelUI extractNotes extractObjectFromFile
#'  importOptions importServer importUI
#' @importFrom magrittr "%>%"
#' @importFrom openssl signature_create signature_verify base64_encode base64_decode
#' @importFrom jsonlite write_json fromJSON toJSON
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
