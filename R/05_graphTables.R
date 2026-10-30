#' Convert objects to DiagrammeR graph tables
#'
#' Converts supported TraceR objects to a list containing DiagrammeR node and
#' edge data frames.
#'
#' @param x Object to convert.
#' @param ... Additional arguments passed to methods.
#' @return A list with `nodes` and `edges` data frames.
#' @export
#' @examples
#' table <- new_tracer_table()
#' as.graph_tables(table)
as.graph_tables <- function(x, ...) {
  UseMethod("as.graph_tables")
}

#' Convert a `tracer_table` to DiagrammeR graph tables
#'
#' Node and capsule rows become graph nodes; edge rows become directed graph
#' edges. Graph node IDs are assigned sequentially in table order, with the
#' original IDs retained in `tracer_id`. Node and capsule types are retained in
#' `type` and `tracer_type`; original unique identifiers are retained in
#' `tracer_unique_identifier`.
#'
#' Nodes use circles and a blue palette, while capsules use rounded boxes and a
#' teal palette. Missing node labels are represented by empty strings. Missing
#' edge labels are represented by a single space because DiagrammeR's current
#' renderer errors on empty or `NA` edge labels; the space is visually blank.
#'
#' Conversion fails if validation reports errors or unresolved references.
#' Completely blank editing rows are ignored. References are resolved only
#' within the current table; hierarchical capsule paths are not supported.
#'
#' @param x A structurally valid `tracer_table`.
#' @param ... Unused.
#' @return A list with DiagrammeR-compatible `nodes` and `edges` data frames.
#' @export
#' @examples
#' table <- new_tracer_table()
#' graph_tables <- as.graph_tables(table)
as.graph_tables.tracer_table <- function(x, ...) {
  validation <- validate_tracer_table(x)
  if (nrow(validation$errors) > 0L) {
    stop(
      paste(
        c("Cannot convert tracer_table with validation errors:", validation$errors$message),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }
  if (nrow(validation$warnings) > 0L) {
    stop(
      paste(
        c("Cannot convert tracer_table with unresolved edge references:", validation$warnings$message),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  data <- x$data
  if (nrow(data) == 0L) {
    blank_rows <- logical()
  } else {
    blank_rows <- vapply(seq_len(nrow(data)), function(row) {
      all(vapply(data[row, , drop = FALSE], function(column) .tracer_table_is_missing(column[[1L]]), logical(1)))
    }, logical(1))
  }

  element_rows <- which(!blank_rows & data$Type %in% c("node", "capsule"))
  edge_rows <- which(!blank_rows & data$Type == "edge")
  element_ids <- data$ID[element_rows]
  graph_ids <- seq_along(element_rows)
  id_map <- stats::setNames(as.integer(graph_ids), element_ids)

  node_type <- data$Type[element_rows]
  node_labels <- data$Label[element_rows]
  node_labels[.tracer_table_is_missing(node_labels)] <- ""
  is_capsule <- node_type == "capsule"
  node_shapes <- rep("circle", length(element_rows))
  node_shapes[is_capsule] <- "box"
  node_styles <- rep("filled", length(element_rows))
  node_styles[is_capsule] <- "rounded,filled"
  node_fillcolors <- rep("#E3F0FB", length(element_rows))
  node_fillcolors[is_capsule] <- "#D9F0ED"
  node_colors <- rep("#386FA4", length(element_rows))
  node_colors[is_capsule] <- "#176B66"

  nodes <- DiagrammeR::create_node_df(
    n = length(element_rows),
    type = node_type,
    label = node_labels,
    shape = node_shapes,
    style = node_styles,
    fillcolor = node_fillcolors,
    color = node_colors,
    fontcolor = "#1F2937",
    tracer_id = data$ID[element_rows],
    tracer_type = node_type,
    tracer_unique_identifier = data[["Unique identifier"]][element_rows]
  )

  edge_labels <- data$Label[edge_rows]
  edge_labels[.tracer_table_is_missing(edge_labels)] <- " "
  edges <- DiagrammeR::create_edge_df(
    from = as.integer(unname(id_map[data$From[edge_rows]])),
    to = as.integer(unname(id_map[data$To[edge_rows]])),
    rel = rep("edge", length(edge_rows)),
    label = edge_labels,
    tracer_id = data$ID[edge_rows],
    tracer_type = data$Type[edge_rows],
    tracer_unique_identifier = data[["Unique identifier"]][edge_rows]
  )

  list(nodes = nodes, edges = edges)
}