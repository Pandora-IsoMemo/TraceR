make_graph_table <- function(ids, labels, types, from = rep(NA_character_, length(ids)), to = rep(NA_character_, length(ids))) {
  data <- new_tracer_table()$data
  if (length(ids) > 0L) {
    data[seq_along(ids), ] <- NA_character_
  }
  data$ID <- ids
  data$Label <- labels
  data$Type <- types
  data$From <- from
  data$To <- to
  new_tracer_table(data)
}

make_diagrammer_graph <- function(graph_tables) {
  graph <- DiagrammeR::create_graph() |>
    DiagrammeR::add_node_df(graph_tables$nodes)
  if (nrow(graph_tables$nodes) > 0L) {
    graph <- DiagrammeR::add_edge_df(graph, graph_tables$edges)
  }
  graph
}

test_that("empty tracer_tables convert to typed empty graph tables and render", {
  graph_tables <- as.graph_tables(new_tracer_table())

  expect_type(graph_tables, "list")
  expect_named(graph_tables, c("nodes", "edges"))
  expect_s3_class(graph_tables$nodes, "data.frame")
  expect_s3_class(graph_tables$edges, "data.frame")
  expect_equal(nrow(graph_tables$nodes), 0L)
  expect_equal(nrow(graph_tables$edges), 0L)
  expect_identical(names(graph_tables$nodes)[1:3], c("id", "type", "label"))
  expect_identical(names(graph_tables$edges)[1:4], c("id", "from", "to", "rel"))
  expect_type(graph_tables$nodes$id, "integer")
  expect_type(graph_tables$edges$from, "integer")

  svg <- DiagrammeR::render_graph(make_diagrammer_graph(graph_tables), as_svg = TRUE)
  expect_match(svg, "<svg")
})

test_that("nodes and capsules map to styled nodes and edges use mapped graph IDs", {
  table <- make_graph_table(
    ids = c("edge-loop", "capsule-x", "edge-link", "node-A", "edge-parallel", "node-B"),
    labels = c(NA_character_, NA_character_, "Contains", "Same label", "Same label", "Same label"),
    types = c("edge", "capsule", "edge", "node", "edge", "node"),
    from = c("node-A", NA_character_, "node-A", NA_character_, "node-A", NA_character_),
    to = c("node-A", NA_character_, "capsule-x", NA_character_, "node-B", NA_character_)
  )
  table$data[["Unique identifier"]] <- c("edge-uid", "capsule-uid", "edge-uid-2", "node-uid", "edge-uid-3", "node-uid-2")
  original <- table

  graph_tables <- as.graph_tables(table)

  expect_equal(graph_tables$nodes$id, c(1L, 2L, 3L))
  expect_equal(graph_tables$nodes$tracer_id, c("capsule-x", "node-A", "node-B"))
  expect_equal(graph_tables$nodes$type, c("capsule", "node", "node"))
  expect_equal(graph_tables$nodes$tracer_type, graph_tables$nodes$type)
  expect_equal(graph_tables$nodes$tracer_unique_identifier, c("capsule-uid", "node-uid", "node-uid-2"))
  expect_equal(graph_tables$nodes$label, c("", "Same label", "Same label"))
  expect_equal(graph_tables$nodes$shape, c("box", "circle", "circle"))
  expect_equal(graph_tables$nodes$style, c("rounded,filled", "filled", "filled"))
  expect_equal(graph_tables$nodes$fillcolor[1], "#D9F0ED")
  expect_equal(graph_tables$nodes$fillcolor[2], "#E3F0FB")
  expect_equal(graph_tables$nodes$color[1], "#176B66")
  expect_equal(graph_tables$nodes$color[2], "#386FA4")

  expect_equal(graph_tables$edges$id, 1:3)
  expect_equal(graph_tables$edges$from, c(2L, 2L, 2L))
  expect_equal(graph_tables$edges$to, c(2L, 1L, 3L))
  expect_equal(graph_tables$edges$tracer_id, c("edge-loop", "edge-link", "edge-parallel"))
  expect_equal(graph_tables$edges$tracer_type, rep("edge", 3))
  expect_equal(graph_tables$edges$tracer_unique_identifier, c("edge-uid", "edge-uid-2", "edge-uid-3"))
  expect_equal(graph_tables$edges$label, c(" ", "Contains", "Same label"))
  expect_identical(table, original)
})

test_that("conversion ignores blank editing rows and supports reordered and renamed optional columns", {
  data <- make_graph_table("source-id", "Source", "node")$data
  blank <- data
  blank[1, ] <- NA_character_
  data <- rbind(data, blank)
  names(data)[7:11] <- c("Author", "Year", "Description", "Source", "Notes")
  data <- data[c("Notes", "Type", "ID", "Label", "From", "To", "Unique identifier", "Author", "Year", "Description", "Source")]

  converted <- as.graph_tables(new_tracer_table(data))

  expect_equal(nrow(converted$nodes), 1L)
  expect_equal(nrow(converted$edges), 0L)
  expect_equal(converted$nodes$tracer_id, "source-id")
})

test_that("validation errors and unresolved references prevent conversion", {
  duplicate_ids <- make_graph_table(
    ids = c("duplicate", "duplicate"),
    labels = c("A", "B"),
    types = c("node", "capsule")
  )
  expect_error(as.graph_tables(duplicate_ids), "validation errors")

  unknown_reference <- make_graph_table(
    ids = c("edge", "node"),
    labels = c("", "Node"),
    types = c("edge", "node"),
    from = c("missing", NA_character_),
    to = c("node", NA_character_)
  )
  expect_error(as.graph_tables(unknown_reference), "unresolved edge references")

  edge_reference <- make_graph_table(
    ids = c("edge-1", "edge-2", "node"),
    labels = c("one", "two", "Node"),
    types = c("edge", "edge", "node"),
    from = c("node", "node", NA_character_),
    to = c("node", "edge-1", NA_character_)
  )
  expect_error(as.graph_tables(edge_reference), "validation errors")
})

test_that("converted tables build and render in DiagrammeR", {
  table <- make_graph_table(
    ids = c("capsule", "node-a", "node-b", "edge-a", "edge-b"),
    labels = c("Container", "A", NA_character_, "Connects", NA_character_),
    types = c("capsule", "node", "node", "edge", "edge"),
    from = c(NA_character_, NA_character_, NA_character_, "node-a", "node-b"),
    to = c(NA_character_, NA_character_, NA_character_, "node-b", "capsule")
  )

  graph_tables <- as.graph_tables(table)
  graph <- make_diagrammer_graph(graph_tables)
  svg <- DiagrammeRsvg::export_svg(DiagrammeR::render_graph(graph, as_svg = TRUE))

  expect_match(svg, "<svg")
  expect_equal(nrow(DiagrammeR::get_node_df(graph)), 3L)
  expect_equal(nrow(DiagrammeR::get_edge_df(graph)), 2L)
  expect_true(all(c("tracer_id", "tracer_type") %in% names(DiagrammeR::get_node_df(graph))))
  expect_true(all(c("tracer_id", "tracer_type") %in% names(DiagrammeR::get_edge_df(graph))))
  expect_equal(sum(graph_tables$nodes$shape == "circle"), 2L)
  expect_equal(sum(graph_tables$nodes$shape == "box"), 1L)
})