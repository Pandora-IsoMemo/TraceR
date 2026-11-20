make_network_test_table <- function(ids = character(), types = character(), from = rep("", length(ids)), to = rep("", length(ids))) {
  columns <- c(
    "ID", "Label", "Type", "From", "To", "Unique identifier",
    paste0("Optional ", 1:5)
  )
  data <- as.data.frame(
    stats::setNames(rep(list(rep("", length(ids))), length(columns)), columns),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  data$ID <- ids
  data$Label <- ids
  data$Type <- types
  data$From <- from
  data$To <- to
  new_tracer_table(data)
}

test_that("a new tracer_network has a root table and no descendants", {
  network <- new_tracer_network()

  expect_s3_class(network, "tracer_network")
  expect_named(network, c("root", "tables"))
  expect_s3_class(get_tracer_table(network), "tracer_table")
  expect_length(network$tables, 0L)
  expect_identical(validate_tracer_network(network)$errors$path, character())
})

test_that("structural operations accept incomplete editable table content", {
  incomplete <- make_network_test_table("Incomplete", "")
  network <- new_tracer_network(incomplete)

  expect_true(nrow(validate_tracer_network(network)$errors) > 0L)
  expect_no_error(add_tracer_capsule(network, "", "C1"))
})

test_that("capsules create nested paths and allow repeated local IDs", {
  network <- new_tracer_network()
  network <- add_tracer_capsule(network, "", "C_1")
  network <- add_tracer_capsule(network, "C_1", "C2")

  root <- make_network_test_table(c("N1", "C_1"), c("node", "capsule"))
  network <- set_tracer_table(network, "", root)
  child <- make_network_test_table(c("N1", "C2"), c("node", "capsule"))
  network <- set_tracer_table(network, "C_1", child)

  expect_identical(names(network$tables), c("C_1", "C_1/C2"))
  expect_identical(list_tracer_capsules(network), "C_1")
  expect_identical(list_tracer_capsules(network, "C_1"), "C2")
  expect_identical(get_tracer_table(network, "C_1/C2"), new_tracer_table())
  expect_identical(validate_tracer_network(network)$errors$path, character())
})

test_that("set_tracer_table rejects capsule relationship changes", {
  network <- new_tracer_network()
  network <- add_tracer_capsule(network, "", "C1")
  original <- network

  missing_child <- make_network_test_table("C2", "capsule")
  expect_error(set_tracer_table(network, "", missing_child), "capsule IDs must remain unchanged")
  expect_identical(network, original)

  extra_child <- make_network_test_table(c("C1", "C2"), c("capsule", "capsule"))
  expect_error(set_tracer_table(network, "", extra_child), "capsule IDs must remain unchanged")
  expect_identical(network, original)
})

test_that("hierarchy validation reports missing and orphaned child tables", {
  root <- make_network_test_table("C1", "capsule")
  missing <- structure(list(root = root, tables = list()), class = "tracer_network")
  expect_true(any(grepl("has no child table", validate_tracer_network(missing)$errors$message)))

  orphan <- structure(
    list(root = new_tracer_table(), tables = stats::setNames(list(new_tracer_table()), "C1")),
    class = "tracer_network"
  )
  expect_true(any(grepl("must correspond to exactly one capsule row", validate_tracer_network(orphan)$errors$message)))
})

test_that("capsule renaming updates only its subtree and local edge references", {
  root <- make_network_test_table(
    c("N1", "E1"),
    c("node", "edge"),
    from = c("", "N1"),
    to = c("", "C1")
  )
  network <- new_tracer_network(root)
  network <- add_tracer_capsule(network, "", "C1")
  network <- add_tracer_capsule(network, "", "C10")
  network <- add_tracer_capsule(network, "C1", "C2")
  network <- add_tracer_capsule(network, "C1/C2", "C3")
  leaf <- make_network_test_table("N1", "node")
  network <- set_tracer_table(network, "C1/C2/C3", leaf)

  renamed <- rename_tracer_capsule(network, "C1", "Renamed")

  expect_true("Renamed" %in% names(renamed$tables))
  expect_true("Renamed/C2" %in% names(renamed$tables))
  expect_true("Renamed/C2/C3" %in% names(renamed$tables))
  expect_true("C10" %in% names(renamed$tables))
  expect_identical(get_tracer_table(renamed, "Renamed/C2/C3"), leaf)
  expect_identical(get_tracer_table(renamed)$data$To[2], "Renamed")
  expect_identical(get_tracer_table(network)$data$To[2], "C1")
  expect_true("C1/C2" %in% names(network$tables))
})

test_that("capsule deletion removes only the exact subtree", {
  root <- make_network_test_table(
    c("N1", "E1"),
    c("node", "edge"),
    from = c("", "N1"),
    to = c("", "C1")
  )
  network <- new_tracer_network(root)
  network <- add_tracer_capsule(network, "", "C1")
  network <- add_tracer_capsule(network, "C1", "Nested")
  network <- add_tracer_capsule(network, "", "C10")

  removed <- remove_tracer_capsule(network, "C1")

  expect_identical(names(removed$tables), "C10")
  expect_identical(list_tracer_capsules(removed), "C10")
  expect_true(any(grepl("unknown node or capsule ID 'C1'", validate_tracer_network(removed)$warnings$message)))
  expect_identical(length(network$tables), 3L)
})

test_that("capsule paths and IDs reject unsafe or reserved names", {
  network <- new_tracer_network()
  for (id in c("has.dot", "two__underscores", "../escape", "bad/name", "CON", "has space ")) {
    expect_error(add_tracer_capsule(network, "", id))
  }
  expect_error(add_tracer_capsule(network, "", paste(rep("é", 128), collapse = "")), "255 UTF-8 bytes")
  expect_error(get_tracer_table(network, "../escape"), "valid tracer_network and capsule path")
  expect_no_error(add_tracer_capsule(network, "", "café_1"))
})

test_that("table validation rejects reserved IDs and hierarchical references", {
  reserved <- make_network_test_table(c("N.1", "N__2"), c("node", "node"))
  result <- validate_tracer_table(reserved)
  expect_equal(sum(result$errors$field == "ID"), 2L)
  expect_true(any(grepl("reserved for hierarchical references", result$errors$message)))
  expect_true(any(grepl("reserved sequence '__'", result$errors$message)))

  hierarchical <- make_network_test_table(
    c("N1", "E1"),
    c("node", "edge"),
    from = c("", "C1.N2"),
    to = c("", "N1")
  )
  result <- validate_tracer_table(hierarchical)
  expect_true(any(grepl("unsupported hierarchical reference 'C1.N2'", result$errors$message)))
  expect_equal(nrow(result$warnings), 0L)

  local <- make_network_test_table(
    c("N1", "C1", "E1"),
    c("node", "capsule", "edge"),
    from = c("", "", "N1"),
    to = c("", "", "C1")
  )
  expect_equal(nrow(validate_tracer_table(local)$errors), 0L)
})

test_that("resetting a renamed parent restores paths without losing descendant edits", {
  child <- make_network_test_table("N1", "node")
  child$data$Label[1] <- "Saved label"
  working <- new_tracer_network()
  working <- add_tracer_capsule(working, "", "C1")
  working <- set_tracer_table(working, "C1", child)
  saved <- working
  path_map <- tracer_network_path_map(working)

  changed_child <- get_tracer_table(working, "C1")
  changed_child$data$Label[1] <- "Unsaved label"
  working <- set_tracer_table(working, "C1", changed_child)
  working <- rename_tracer_capsule(working, "C1", "Renamed")
  path_map <- tracer_network_rebase_path_map(path_map, "C1", "Renamed")

  reset <- tracer_network_reset_table(working, saved, path_map, "")

  expect_true("C1" %in% names(reset$network$tables))
  expect_false("Renamed" %in% names(reset$network$tables))
  expect_identical(get_tracer_table(reset$network, "C1")$data$Label, "Unsaved label")
  expect_identical(reset$path_map$saved_path[reset$path_map$working_path == "C1"], "C1")
})

test_that("saving a rename moves checkpoint paths without saving descendant edits", {
  child <- make_network_test_table("N1", "node")
  child$data$Label[1] <- "Saved label"
  working <- new_tracer_network()
  working <- add_tracer_capsule(working, "", "C1")
  working <- set_tracer_table(working, "C1", child)
  saved <- working
  path_map <- tracer_network_path_map(working)

  changed_child <- get_tracer_table(working, "C1")
  changed_child$data$Label[1] <- "Unsaved label"
  working <- set_tracer_table(working, "C1", changed_child)
  working <- rename_tracer_capsule(working, "C1", "Renamed")
  path_map <- tracer_network_rebase_path_map(path_map, "C1", "Renamed")

  checkpoint <- tracer_network_save_table(working, saved, path_map, "")

  expect_true("Renamed" %in% names(checkpoint$network$tables))
  expect_identical(get_tracer_table(checkpoint$network, "Renamed")$data$Label, "Saved label")
  expect_identical(get_tracer_table(working, "Renamed")$data$Label, "Unsaved label")
  expect_identical(
    checkpoint$path_map$saved_path[checkpoint$path_map$working_path == "Renamed"],
    "Renamed"
  )
})

test_that("resetting deletion restores the saved subtree and additions reset away", {
  child <- make_network_test_table("N1", "node")
  working <- new_tracer_network()
  working <- add_tracer_capsule(working, "", "C1", child_table = child)
  saved <- working
  path_map <- tracer_network_path_map(working)

  deleted <- remove_tracer_capsule(working, "C1")
  path_map <- tracer_network_drop_paths(path_map, "C1")
  reset <- tracer_network_reset_table(deleted, saved, path_map, "")
  expect_identical(get_tracer_table(reset$network, "C1"), child)

  added <- add_tracer_capsule(reset$network, "", "C2")
  added_map <- tracer_network_register_paths(reset$path_map, added, "C2")
  reset_added <- tracer_network_reset_table(added, reset$network, added_map, "")
  expect_false("C2" %in% names(reset_added$network$tables))
})
