collapse_test_table <- function(
  ids = character(),
  types = character(),
  from = rep("", length(ids)),
  to = rep("", length(ids)),
  labels = rep("", length(ids)),
  unique_ids = rep("", length(ids))
) {
  data <- new_tracer_table()$data
  data <- data[FALSE, , drop = FALSE]
  if (length(ids) > 0L) {
    data <- as.data.frame(
      stats::setNames(
        rep(list(rep("", length(ids))), ncol(data)),
        names(data)
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    data$ID <- ids
    data$Type <- types
    data$From <- from
    data$To <- to
    data$Label <- labels
    data[["Unique identifier"]] <- unique_ids
  }
  new_tracer_table(data)
}

collapse_test_network <- function(parent_table, child_table, nested_tables = list()) {
  network <- new_tracer_network()
  parent_capsules <- parent_table$data$ID[
    !is.na(parent_table$data$Type) & parent_table$data$Type == "capsule"
  ]
  for (capsule_id in parent_capsules) {
    network <- add_tracer_capsule(network, "", capsule_id)
  }
  network <- set_tracer_table(network, "", parent_table)

  child_capsules <- child_table$data$ID[
    !is.na(child_table$data$Type) & child_table$data$Type == "capsule"
  ]
  for (capsule_id in child_capsules) {
    network <- add_tracer_capsule(network, "C1", capsule_id)
  }
  network <- set_tracer_table(network, "C1", child_table)

  nested_order <- names(nested_tables)
  if (length(nested_order) > 0L) {
    nested_order <- nested_order[
      order(lengths(strsplit(nested_order, "/", fixed = TRUE)))
    ]
  }
  for (path in nested_order) {
    nested_table <- nested_tables[[path]]
    nested_capsules <- nested_table$data$ID[
      !is.na(nested_table$data$Type) & nested_table$data$Type == "capsule"
    ]
    for (capsule_id in nested_capsules) {
      network <- add_tracer_capsule(network, path, capsule_id)
    }
    network <- set_tracer_table(network, path, nested_table)
  }
  network
}

test_that("collapsing promotes rows, rewrites references, and preserves metadata", {
  parent <- collapse_test_table(
    c("A", "E1", "C1", "E2", "B"),
    c("node", "edge", "capsule", "edge", "node"),
    from = c("", "A", "", "C1", ""),
    to = c("", "C1", "", "B", ""),
    labels = c("", "incoming", "capsule label", "outgoing", ""),
    unique_ids = c("", "external-1", "capsule-uid", "external-2", "")
  )
  child <- collapse_test_table(
    c("N1", "N2", "E3"),
    c("node", "node", "edge"),
    from = c("", "", "N1"),
    to = c("", "", "N2"),
    labels = c("one", "two", "internal"),
    unique_ids = c("node-1", "node-2", "internal-uid")
  )
  network <- collapse_test_network(parent, child)

  result <- collapse_tracer_capsule(network, "C1")
  expect_true(result$success)
  expect_equal(nrow(result$issues), 0L)
  output <- result$network$root$data
  expect_false("C1" %in% output$ID)
  expect_true(all(c("C1_N1", "C1_N2", "C1_E3") %in% output$ID))
  expect_false("C1" %in% names(result$network$tables))
  expect_identical(output[output$ID == "C1_E3", "From"], "C1_N1")
  expect_identical(output[output$ID == "C1_E3", "To"], "C1_N2")
  expect_identical(output[output$ID == "C1_N1", "Unique identifier"], "node-1")
  expect_identical(output[output$ID == "E1", "From"], "A")
  expect_identical(output[output$ID == "E1", "To"], "C1_N1")
  expect_identical(output[output$ID == "E1", "Unique identifier"], "external-1")
  expect_identical(output[output$ID == "E2", "From"], "C1_N2")
  expect_identical(output[output$ID == "C1_N1", "Label"], "one")
  expect_identical(result$summary$promoted_nodes, 2L)
  expect_identical(result$summary$promoted_internal_edges, 1L)
  expect_identical(result$summary$updated_edges, 2L)
  expect_identical(validate_tracer_network(result$network)$errors$path, character())
})

test_that("multiple boundary elements generate deterministic edge IDs", {
  parent <- collapse_test_table(
    c("A", "E1", "C1", "E2", "B"),
    c("node", "edge", "capsule", "edge", "node"),
    from = c("", "A", "", "C1", ""),
    to = c("", "C1", "", "B", "")
  )
  child <- collapse_test_table(
    c("N1", "N2"),
    c("node", "node")
  )
  result <- collapse_tracer_capsule(collapse_test_network(parent, child), "C1")

  expect_true(result$success)
  expect_identical(result$generated_edges$id, c("E1_1", "E1_2", "E2_1", "E2_2"))
  expect_identical(result$generated_edges$from, c("A", "A", "C1_N1", "C1_N2"))
  expect_identical(result$generated_edges$to, c("C1_N1", "C1_N2", "B", "B"))
  expect_identical(result$summary$removed_edges, 2L)
  expect_identical(result$summary$generated_edges, 4L)
})

test_that("nested capsule paths move one level and reset restores the checkpoint", {
  parent <- collapse_test_table("C1", "capsule")
  child <- collapse_test_table("C2", "capsule")
  nested <- collapse_test_table("C3", "capsule")
  leaf <- collapse_test_table("N1", "node")
  network <- collapse_test_network(
    parent,
    child,
    nested_tables = list("C1/C2" = nested, "C1/C2/C3" = leaf)
  )
  path_map <- tracer_network_path_map(network)

  result <- collapse_tracer_capsule(network, "C1")
  updated_map <- tracer_network_collapse_path_map(
    path_map,
    "C1",
    result$network
  )

  expect_true(result$success)
  expect_true(all(c("C1_C2", "C1_C2/C3") %in% names(result$network$tables)))
  expect_identical(result$updated_paths$old_path, c("C1/C2", "C1/C2/C3"))
  expect_identical(result$updated_paths$new_path, c("C1_C2", "C1_C2/C3"))
  expect_identical(
    updated_map$saved_path[match(c("C1_C2", "C1_C2/C3"), updated_map$working_path)],
    c("C1/C2", "C1/C2/C3")
  )

  saved <- tracer_network_save_table(result$network, network, updated_map, "")
  expect_true(all(c("C1_C2", "C1_C2/C3") %in% names(saved$network$tables)))
  expect_identical(
    saved$path_map$saved_path[
      match(c("C1_C2", "C1_C2/C3"), saved$path_map$working_path)
    ],
    c("C1_C2", "C1_C2/C3")
  )

  reset <- tracer_network_reset_table(result$network, network, updated_map, "")
  expect_identical(reset$network, network)
  expect_identical(reset$path_map$working_path, tracer_network_paths(network))
})

test_that("empty capsules generate pass-through edges and report metadata conflicts", {
  parent <- collapse_test_table(
    c("A", "E1", "C1", "E2", "B"),
    c("node", "edge", "capsule", "edge", "node"),
    from = c("", "A", "", "C1", ""),
    to = c("", "C1", "", "B", ""),
    labels = c("", "Input label", "", "Output label", ""),
    unique_ids = c("", "incoming-uid", "", "", "")
  )
  child <- collapse_test_table()
  result <- collapse_tracer_capsule(collapse_test_network(parent, child), "C1")

  expect_true(result$success)
  expect_identical(result$generated_edges$id, "E1_E2")
  expect_identical(result$generated_edges$from, "A")
  expect_identical(result$generated_edges$to, "B")
  generated <- result$network$root$data[result$network$root$data$ID == "E1_E2", ]
  expect_identical(generated[["Unique identifier"]], "incoming-uid")
  expect_identical(generated$Label, "")
  expect_identical(result$metadata_conflicts$field, "Label")
  expect_identical(result$metadata_conflicts$original_value_1, "Input label")
  expect_identical(result$metadata_conflicts$original_value_2, "Output label")
  expect_identical(result$summary$removed_edges, 2L)
  expect_match(result$edge_changes$original_connections[1], "E1: A -> C1", fixed = TRUE)
  expect_match(
    result$edge_changes$resulting_connection[
      result$edge_changes$action == "Generated"
    ],
    "E1_E2: A -> B",
    fixed = TRUE
  )
})

test_that("empty capsule self-loops are removed rather than used as pass-through edges", {
  parent <- collapse_test_table(
    c("A", "E1", "C1", "Eloop", "E2", "B"),
    c("node", "edge", "capsule", "edge", "edge", "node"),
    from = c("", "A", "", "C1", "C1", ""),
    to = c("", "C1", "", "C1", "B", "")
  )
  result <- collapse_tracer_capsule(
    collapse_test_network(parent, collapse_test_table()),
    "C1"
  )

  expect_true(result$success)
  expect_identical(result$generated_edges$id, "E1_E2")
  expect_identical(result$summary$removed_edges, 3L)
  expect_false("Eloop" %in% result$network$root$data$ID)
})

test_that("large empty-capsule expansion preserves row and report ordering", {
  incoming <- paste0("I", seq_len(100L))
  outgoing <- paste0("O", seq_len(100L))
  parent <- collapse_test_table(
    c("A", incoming, "C1", outgoing, "B"),
    c("node", rep("edge", 100L), "capsule", rep("edge", 100L), "node"),
    from = c("", rep("A", 100L), "", rep("C1", 100L), ""),
    to = c("", rep("C1", 100L), "", rep("B", 100L), ""),
    labels = c("", rep("Input", 100L), "", rep("Output", 100L), "")
  )
  network <- collapse_test_network(parent, collapse_test_table())

  result <- collapse_tracer_capsule(network, "C1")

  expect_true(result$success)
  ids <- paste(rep(incoming, each = 100L), rep(outgoing, times = 100L), sep = "_")
  originals <- paste(rep(incoming, each = 100L), rep(outgoing, times = 100L), sep = ", ")
  output <- result$network$root$data
  expect_identical(output$ID, c("A", ids, "B"))
  expect_identical(output$From, c("", rep("A", 10000L), ""))
  expect_identical(output$To, c("", rep("B", 10000L), ""))
  expect_identical(output$Label, rep("", 10002L))
  expect_identical(result$generated_edges$id, ids)
  expect_identical(result$generated_edges$original_edges, originals)
  expect_identical(result$removed_edges$id, c(incoming, outgoing))
  expect_identical(result$metadata_conflicts$generated_edge, ids)
  expect_identical(result$metadata_conflicts$field, rep("Label", 10000L))
  expect_identical(result$metadata_conflicts$original_value_1, rep("Input", 10000L))
  expect_identical(result$metadata_conflicts$original_value_2, rep("Output", 10000L))
  expect_identical(
    result$edge_changes$action,
    c(rep(c("Removed", rep("Generated", 100L)), 100L), rep("Removed", 100L))
  )
  expect_identical(
    result$edge_changes$resulting_edge[result$edge_changes$action == "Generated"],
    ids
  )
  expect_identical(result$summary$generated_edges, 10000L)
  expect_identical(result$summary$metadata_conflicts, 10000L)
  expect_identical(network$root, parent)
})

test_that("collapsing an isolated empty capsule retains empty report schemas", {
  network <- collapse_test_network(
    collapse_test_table("C1", "capsule"),
    collapse_test_table()
  )

  result <- collapse_tracer_capsule(network, "C1")

  expect_true(result$success)
  expect_true(tracer_network_data_identical(result$network, new_tracer_network()))
  empty_report <- .tracer_collapse_empty_report()
  for (field in c("generated_edges", "updated_edges", "removed_edges", "metadata_conflicts", "edge_changes")) {
    expect_identical(result[[field]], empty_report[[field]])
  }
})

test_that("cycles are allowed unless external edges require missing boundaries", {
  cycle <- collapse_test_table(
    c("N1", "N2", "E1", "E2"),
    c("node", "node", "edge", "edge"),
    from = c("", "", "N1", "N2"),
    to = c("", "", "N2", "N1")
  )
  no_external_edges <- collapse_test_network(
    collapse_test_table("C1", "capsule"),
    cycle
  )
  expect_true(collapse_tracer_capsule(no_external_edges, "C1")$success)

  with_incoming <- collapse_test_network(
    collapse_test_table(
      c("A", "E3", "C1"),
      c("node", "edge", "capsule"),
      from = c("", "A", ""),
      to = c("", "C1", "")
    ),
    cycle
  )
  blocked <- collapse_tracer_capsule(with_incoming, "C1")
  expect_false(blocked$success)
  expect_true(any(grepl("starting element", blocked$issues$message)))
})

test_that("blocking issues leave the input unchanged", {
  parent <- collapse_test_table(
    c("A", "E1", "C1"),
    c("node", "edge", "capsule"),
    from = c("", "A", ""),
    to = c("", "C1", "")
  )
  child <- collapse_test_table(
    c("N1", "N2", "E1"),
    c("node", "node", "edge"),
    from = c("", "", "N1"),
    to = c("", "", "Missing")
  )
  network <- collapse_test_network(parent, child)
  original <- network

  result <- collapse_tracer_capsule(network, "C1")
  expect_false(result$success)
  expect_null(result$network)
  expect_true(any(grepl("unknown node or capsule", result$issues$message)))
  expect_identical(network, original)
})

test_that("collapse blocks optional-column mismatches and dangling-reference resolution", {
  parent <- collapse_test_table("C1", "capsule")
  child <- collapse_test_table("N1", "node")
  names(child$data)[7] <- "Different meaning"
  network <- collapse_test_network(parent, child)
  result <- collapse_tracer_capsule(network, "C1")
  expect_false(result$success)
  expect_true(any(grepl("different optional column names", result$issues$message)))

  parent <- collapse_test_table(
    c("A", "E1", "C1"),
    c("node", "edge", "capsule"),
    from = c("", "A", ""),
    to = c("", "C1_N1", "")
  )
  result <- collapse_tracer_capsule(
    collapse_test_network(parent, collapse_test_table("N1", "node")),
    "C1"
  )
  expect_false(result$success)
  expect_true(any(grepl("currently unresolved", result$issues$message)))
})

test_that("root collapse is rejected", {
  result <- collapse_tracer_capsule(new_tracer_network(), "")
  expect_false(result$success)
  expect_true(any(grepl("non-root", result$issues$message)))
})

test_that("app collapse updates working state, saves, and resets with shared feedback", {
  network <- collapse_test_network(
    collapse_test_table("C1", "capsule"),
    collapse_test_table("N1", "node")
  )
  app <- shiny::shinyAppDir(system.file("app", package = "TraceR"))

  shiny::testServer(app$serverFuncSource(), {
    working_network(network)
    saved_network(network)
    path_map(tracer_network_path_map(network))
    selected_path("C1")
    current_table(get_tracer_table(network, "C1"))
    session$flushReact()

    session$setInputs(`sidebar-collapse_capsule` = 1L)
    session$flushReact()
    session$setInputs(confirm_capsule_collapse = 1L)
    session$flushReact()

    expect_identical(selected_path(), "")
    expect_identical(working_network()$root$data$ID, "C1_N1")
    expect_identical(saved_network(), network)
    expect_true(operation_feedback()$result$success)
    expect_false(operation_feedback()$saved)
    expect_match(output$operation_feedback$html, "not yet saved", fixed = TRUE)

    session$setInputs(network_tabs = "Network Graph")
    session$flushReact()
    expect_match(output$operation_feedback$html, "collapsed", fixed = TRUE)

    session$setInputs(`sidebar-save_table` = 1L)
    session$flushReact()
    expect_true(operation_feedback()$saved)
    expect_identical(saved_network()$root$data$ID, "C1_N1")

    changed <- current_table()
    changed$data$Label[1] <- "Unsaved after collapse"
    current_table(new_tracer_table(changed$data))
    session$flushReact()
    session$setInputs(`sidebar-reset_table` = 1L)
    session$flushReact()
    session$setInputs(confirm_table_reset = 1L)
    session$flushReact()

    expect_null(operation_feedback())
    expect_identical(current_table()$data$Label[1], "")
  })
})

test_that("renaming a collapsed capsule parent rebases its feedback path", {
  network <- collapse_test_network(
    collapse_test_table("C1", "capsule"),
    collapse_test_table("C2", "capsule"),
    nested_tables = list("C1/C2" = collapse_test_table("N1", "node"))
  )
  app <- shiny::shinyAppDir(system.file("app", package = "TraceR"))

  shiny::testServer(app$serverFuncSource(), {
    working_network(network)
    saved_network(network)
    path_map(tracer_network_path_map(network))
    selected_path("C1/C2")
    current_table(get_tracer_table(network, "C1/C2"))
    session$flushReact()

    session$setInputs(`sidebar-collapse_capsule` = 1L)
    session$flushReact()
    session$setInputs(confirm_capsule_collapse = 1L)
    session$flushReact()
    expect_identical(operation_feedback()$parent_path, "C1")

    session$setInputs(`sidebar-rename_capsule_id` = "Renamed", `sidebar-rename_capsule` = 1L)
    session$flushReact()
    expect_identical(operation_feedback()$parent_path, "Renamed")

    expect_true(save_network_table("Renamed"))
    expect_true(operation_feedback()$saved)
    reset_network_table("Renamed")
    expect_null(operation_feedback())
  })
})
