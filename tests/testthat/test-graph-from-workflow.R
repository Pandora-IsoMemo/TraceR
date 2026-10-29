test_that("display of graph from a workflow", {
  wf_as_graph_table <- list(
    nodes = structure(
      list(
        id = c("1", "2", "3", "4"),
        name = c("A", "B", "C", "D"),
        label = c("A", "B", "C", "D"),
        command = c("identity", "identity", "identity", "identity"),
        entry = structure(1:4, names = c("A", "B", "C", "D")),
        order = 1:4),
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(NA, -4L)),
    edges = structure(
      list(
        from = c("1", "1", "2", "3"),
        to = c("2", "3", "4", "4"),
        rel = c("required_steps", "required_steps", "required_steps", "required_steps")
      ),
      row.names = c(NA, -4L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )

  graph <- create_graph(directed = TRUE) |>
    add_nodes_from_table(
      wf_as_graph_table$nodes,
      label_col = label,
      type_col = command
    ) |>
    add_edges_from_table(
      wf_as_graph_table$edges,
      from_col = from,
      to_col = to,
      from_to_map = id_external,
      rel_col = rel
    )

  expect_equal(nrow(graph$nodes_df), 4L)
  expect_equal(graph$nodes_df$label, c("A", "B", "C", "D"))
  expect_equal(graph$nodes_df$type, rep("identity", 4))
  expect_equal(nrow(graph$edges_df), 4L)
  expect_true(all(graph$edges_df$rel == "required_steps"))

  svg <- graph |>
    render_graph(layout = "tree") |>
    DiagrammeRsvg::export_svg()

  expect_match(svg, "<svg")
  expect_true(all(vapply(
    c("A", "B", "C", "D"), grepl, logical(1), x = svg, fixed = TRUE
  )))
})

test_that("display of graph from the example workflow", {
  example_wf_as_graph_table <- PEITHO::as.graph_tables(
    PEITHO::workflow_example()
  )

  graph <- create_graph(directed = TRUE) |>
    add_nodes_from_table(
      example_wf_as_graph_table$nodes,
      label_col = label,
      type_col = type
    ) |>
    add_edges_from_table(
      example_wf_as_graph_table$edges,
      from_col = from,
      to_col = to,
      from_to_map = id_external,
      rel_col = rel
    )

  expect_equal(nrow(graph$nodes_df), 11L)
  expect_true(all(c("id", "label", "type") %in% colnames(graph$nodes_df)))
  expect_true(all(c("step", "input") %in% graph$nodes_df$type))
  expect_equal(nrow(graph$edges_df), 10L)
  expect_true(all(c("required_steps", "required_inputs") %in% graph$edges_df$rel))

  step_ids <- example_wf_as_graph_table$nodes$id[example_wf_as_graph_table$nodes$type == "step"]
  input_ids <- example_wf_as_graph_table$nodes$id[example_wf_as_graph_table$nodes$type == "input"]

  set_node_style <- function(
    graph, node_ids, fillcolor, shape, style = "filled"
  ) {
    graph |>
      DiagrammeR::set_node_attrs(
        node_attr = "fillcolor", values = fillcolor, nodes = node_ids
      ) |>
      DiagrammeR::set_node_attrs(
        node_attr = "style", values = style, nodes = node_ids
      ) |>
      DiagrammeR::set_node_attrs(
        node_attr = "shape", values = shape, nodes = node_ids
      )
  }

  svg <- graph |>
    DiagrammeR::set_node_attrs("fontcolor", "black") |>
    set_node_style(step_ids, "#AED6F1", "rectangle") |>
    set_node_style(input_ids, "#A9DFBF", "ellipse") |>
    render_graph(layout = "tree") |>
    DiagrammeRsvg::export_svg()

  expect_match(svg, "<svg")
  expect_match(svg, "<text")
})
