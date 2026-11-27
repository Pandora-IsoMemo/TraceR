test_that("shared sidebar exposes current table and session controls", {
  html <- htmltools::renderTags(mod_sidebar_ui("sidebar"))$html

  expect_match(html, "<h3>Network</h3>", fixed = TRUE)
  expect_match(html, 'id="sidebar-generate_example"', fixed = TRUE)
  expect_match(html, "Load Example", fixed = TRUE)
  expect_match(html, 'id="sidebar-empty_network"', fixed = TRUE)
  expect_match(html, "Load Empty", fixed = TRUE)
  expect_match(html, "Save", fixed = TRUE)
  expect_match(html, "Reset", fixed = TRUE)
  expect_match(html, 'id="sidebar-capsule_tree"', fixed = TRUE)
  expect_match(html, 'id="sidebar-selected_path"', fixed = TRUE)
  expect_match(html, 'id="sidebar-add_capsule"', fixed = TRUE)
  expect_match(html, 'id="sidebar-rename_capsule"', fixed = TRUE)
  expect_match(html, 'id="sidebar-collapse_capsule"', fixed = TRUE)
  expect_match(html, 'id="sidebar-delete_capsule"', fixed = TRUE)
  expect_match(html, 'id="sidebar-download_network"', fixed = TRUE)
  expect_match(html, 'id="sidebar-network_zip_file"', fixed = TRUE)
  expect_match(html, 'id="sidebar-import_network"', fixed = TRUE)
  expect_match(html, 'id="sidebar-import_network_as_capsule"', fixed = TRUE)
  expect_false(grepl("table_import", html, fixed = TRUE))
  expect_false(grepl("download_table", html, fixed = TRUE))
  expect_match(html, "Session", fixed = TRUE)
  expect_match(html, "Download Session", fixed = TRUE)
  expect_match(html, "Import Session", fixed = TRUE)
  expect_false(grepl("Import JSON", html, fixed = TRUE))
  expect_false(grepl("sidebar-download_inputs", html, fixed = TRUE))
  expect_false(grepl("test_input", html, fixed = TRUE))
})

test_that("shared sidebar server initializes with shared state and root inputs", {
  shiny::testServer(
    mod_sidebar_server,
    args = list(
      root_inputs = shiny::reactiveValues(),
      current_network = shiny::reactive(new_tracer_network())
    ),
    {
      expect_true(is.list(session$returned))
      expect_named(session$returned, c(
        "generate_example", "empty_network", "save_table", "reset_table",
        "add_capsule", "new_capsule_id", "rename_capsule", "rename_capsule_id",
        "collapse_capsule", "delete_capsule", "network_zip_file", "import_network", "import_capsule_id",
        "import_network_as_capsule", "uploaded_session", "capsule_selection"
      ))
      session$setInputs(generate_example = 1)
      expect_equal(session$returned$generate_example(), 1)
      session$setInputs(empty_network = 1)
      expect_equal(session$returned$empty_network(), 1)
      session$setInputs(save_table = 1)
      expect_equal(session$returned$save_table(), 1)
      session$setInputs(reset_table = 1)
      expect_equal(session$returned$reset_table(), 1)
      expect_match(output$unsaved_status$html, "Saved", fixed = TRUE)
    }
  )
})

test_that("session downloads capture the complete network and selected path", {
  network <- createExampleTracerNetwork()
  selected_path <- shiny::reactiveVal("C1")
  session_data <- NULL
  testthat::local_mocked_bindings(
    downloadModelServer = function(id, dat, ...) {
      session_data <<- dat
    },
    .package = "DataTools"
  )

  shiny::testServer(
    mod_sidebar_server,
    args = list(
      root_inputs = shiny::reactiveValues(),
      current_network = shiny::reactive(network),
      selected_path = selected_path
    ),
    {
      session$flushReact()
      expect_identical(session_data(), list(
        tracer_network = network,
        selected_path = "C1"
      ))
    }
  )
})

test_that("capsule tree selections return slash-separated paths", {
  network <- new_tracer_network()
  network <- add_tracer_capsule(network, "", "C1")
  network <- add_tracer_capsule(network, "C1", "C2")

  shiny::testServer(
    mod_sidebar_server,
    args = list(
      root_inputs = shiny::reactiveValues(),
      current_network = shiny::reactive(network),
      selected_path = shiny::reactive("")
    ),
    {
      selection <- list(
        Root = structure(
          list(
            C1 = structure(
              list(C2 = structure("", stselected = TRUE)),
              stselected = FALSE
            )
          ),
          stselected = FALSE
        )
      )
      session$setInputs(capsule_tree = selection)
      expect_identical(session$returned$capsule_selection(), "C1/C2")
      expect_match(output$selected_path, "Root", fixed = TRUE)
    }
  )
})

test_that("capsule tree only rerenders for structural network changes", {
  table_data <- new_tracer_table()$data
  table_data[1, ] <- ""
  table_data$ID[1] <- "N1"
  table_data$Type[1] <- "node"
  network <- add_tracer_capsule(
    new_tracer_network(),
    "",
    "C1",
    child_table = new_tracer_table(table_data)
  )
  current_network <- shiny::reactiveVal(network)
  tree_update_count <- 0L
  testthat::local_mocked_bindings(
    updateTree = function(session, inputId, data) {
      tree_update_count <<- tree_update_count + 1L
    },
    .package = "shinyTree"
  )

  shiny::testServer(
    mod_sidebar_server,
    args = list(
      root_inputs = shiny::reactiveValues(),
      current_network = current_network,
      selected_path = shiny::reactive("")
    ),
    {
      session$flushReact()
      initial_updates <- tree_update_count

      edited_table <- get_tracer_table(current_network(), "C1")
      edited_table$data$Label[1] <- "Edited label"
      current_network(set_tracer_table(current_network(), "C1", edited_table))
      session$flushReact()
      expect_identical(tree_update_count, initial_updates)

      updated_network <- add_tracer_capsule(current_network(), "", "C2")
      current_network(updated_network)
      session$flushReact()
      expect_gt(tree_update_count, initial_updates)
    }
  )
})

test_that("selecting a capsule loads its table into the shared app state", {
  child_data <- new_tracer_table()$data
  child_data[1, ] <- ""
  child_data$ID[1] <- "N1"
  child_data$Type[1] <- "node"
  child_table <- new_tracer_table(child_data)

  network <- new_tracer_network()
  network <- add_tracer_capsule(network, "", "C1")
  network <- set_tracer_table(network, "C1", child_table)
  app <- shiny::shinyAppDir(system.file("app", package = "TraceR"))

  shiny::testServer(app$serverFuncSource(), {
    working_network(network)
    saved_network(network)
    path_map(tracer_network_path_map(network))
    current_table(get_tracer_table(network, ""))
    session$flushReact()

    tree_input <- list(
      Root = structure(
        list(C1 = structure("", stselected = TRUE)),
        stselected = FALSE
      )
    )
    session$setInputs(`sidebar-capsule_tree` = tree_input)
    session$flushReact()

    expect_identical(selected_path(), "C1")
    expect_identical(current_table(), child_table)
  })
})

test_that("case-only sidebar renames preserve nested contents and update tree navigation", {
  root_data <- new_tracer_table()$data[rep(NA_integer_, 3L), ]
  root_data[] <- ""
  root_data$ID <- c("N1", "Incoming", "Outgoing")
  root_data$Type <- c("node", "edge", "edge")
  root_data$From <- c("", "N1", "C1")
  root_data$To <- c("", "C1", "N1")
  contents <- get_tracer_table(createExampleTracerNetwork(), "C1")
  network <- add_tracer_capsule(new_tracer_network(new_tracer_table(root_data)), "", "C1", child_table = contents)
  network <- add_tracer_capsule(network, "C1", "C2", child_table = contents)
  sidebar_tree <- NULL
  testthat::local_mocked_bindings(
    updateTree = function(session, inputId, data) {
      sidebar_tree <<- data
    },
    .package = "shinyTree"
  )
  app <- shiny::shinyAppDir(system.file("app", package = "TraceR"))

  shiny::testServer(app$serverFuncSource(), {
    working_network(network)
    saved_network(network)
    path_map(tracer_network_path_map(network))
    current_table(get_tracer_table(network, ""))
    session$flushReact()
    session$setInputs(`sidebar-capsule_tree` = list(Root = structure(
      list(C1 = structure(list(C2 = ""), stselected = TRUE)),
      stselected = FALSE
    )))
    expect_identical(selected_path(), "C1")

    session$setInputs(`sidebar-rename_capsule_id` = "c1", `sidebar-rename_capsule` = 1L)
    session$flushReact()
    invisible(output[["sidebar-capsule_tree"]])

    expect_identical(selected_path(), "c1")
    expect_identical(current_table(), get_tracer_table(network, "C1"))
    expect_identical(working_network()$root$data$From[3], "c1")
    expect_identical(working_network()$root$data$To[2], "c1")
    expect_identical(list_tracer_capsules(working_network()), "c1")
    expect_identical(names(working_network()$tables), c("c1", "c1/C2"))
    expect_identical(get_tracer_table(working_network(), "c1/C2"), contents)
    expect_identical(saved_network(), network)
    expect_identical(path_map()$working_path, c("", "c1", "c1/C2"))
    expect_identical(path_map()$saved_path, c("", "C1", "C1/C2"))
    expect_identical(output[["sidebar-selected_path"]], "Root / c1")
    expect_identical(names(sidebar_tree$Root), "c1")
    expect_identical(names(sidebar_tree$Root$c1), "C2")
    expect_true(attr(sidebar_tree$Root$c1, "stselected"))
    expect_true(attr(sidebar_tree$Root$c1, "stopened"))
  })
})

test_that("navigation reconciles checkpoint paths for identically recreated capsules", {
  app <- shiny::shinyAppDir(system.file("app", package = "TraceR"))

  for (action in c("save", "discard")) {
    shiny::testServer(app$serverFuncSource(), {
      session$flushReact()
      session$setInputs(`sidebar-new_capsule_id` = "C1", `sidebar-add_capsule` = 1L)
      session$setInputs(`sidebar-save_table` = 1L)
      expect_false(unsaved_changes())
      navigate_to("C1")
      session$flushReact()

      session$setInputs(`sidebar-delete_capsule` = 1L)
      expect_identical(selected_path(), "")
      session$setInputs(`sidebar-add_capsule` = 2L)
      expect_true(tracer_network_data_identical(working_network(), saved_network()))
      expect_true(is.na(path_map()$saved_path[match("C1", path_map()$working_path)]))
      expect_false(unsaved_changes())

      navigate_to("C1")
      session$flushReact()
      expect_identical(selected_path(), "C1")
      expect_null(pending_navigation())
      expect_identical(path_map(), tracer_network_path_map(working_network(), saved_network()))
      expect_no_error(session$setInputs(`sidebar-save_table` = 2L))

      edited_data <- new_tracer_table()$data
      edited_data[1, ] <- ""
      edited_data$ID[1] <- "N1"
      edited_data$Label[1] <- "Edited node"
      edited_data$Type[1] <- "node"
      edited_table <- new_tracer_table(edited_data)
      current_table(edited_table)
      session$flushReact()
      navigate_to("")
      expect_identical(pending_navigation(), "")
      expect_identical(selected_path(), "C1")

      if (action == "save") {
        expect_no_error(session$setInputs(save_navigation = 1L))
      } else {
        expect_no_error(session$setInputs(discard_navigation = 1L))
      }
      expect_identical(selected_path(), "")
      expect_null(pending_navigation())
      expect_identical(
        get_tracer_table(working_network(), "C1"),
        if (action == "save") edited_table else new_tracer_table()
      )
      expect_true(tracer_network_data_identical(working_network(), saved_network()))
      expect_false(unsaved_changes())
    })
  }
})

test_that("node and edge renames stay table-local and follow explicit Save and Reset", {
  network <- createExampleTracerNetwork()
  original_map <- tracer_network_path_map(network)
  app <- shiny::shinyAppDir(system.file("app", package = "TraceR"))

  shiny::testServer(app$serverFuncSource(), {
    working_network(network)
    saved_network(network)
    path_map(original_map)
    current_table(get_tracer_table(network, ""))
    session$flushReact()

    rename_example_ids <- function(node_id, edge_id) {
      session$setInputs(`table-table_cell_edit` = list(row = 5L, col = 0L, value = node_id))
      session$setInputs(`table-table_cell_edit` = list(row = 10L, col = 0L, value = edge_id))
    }
    rename_example_ids("n4", "e4")
    renamed_table <- current_table()
    expect_identical(working_network()$root, renamed_table)
    expect_identical(saved_network(), network)
    expect_identical(path_map(), original_map)
    expect_identical(selected_path(), "")
    expect_identical(get_tracer_table(working_network(), "C1"), get_tracer_table(network, "C1"))
    expect_true(unsaved_changes())
    graph_tables <- as.graph_tables(renamed_table)
    expect_true("n4" %in% graph_tables$nodes$tracer_id)
    expect_true("e4" %in% graph_tables$edges$tracer_id)
    expect_identical(renamed_table$data$From[11], "n4")
    expect_identical(renamed_table$data$To[10], "n4")

    session$setInputs(`sidebar-reset_table` = 1L)
    session$setInputs(confirm_table_reset = 1L)
    expect_identical(current_table(), network$root)
    expect_false(unsaved_changes())

    rename_example_ids("n4", "e4")
    session$setInputs(`sidebar-save_table` = 1L)
    expect_identical(saved_network()$root, renamed_table)
    expect_false(unsaved_changes())
    rename_example_ids("N4", "E4")
    expect_true(unsaved_changes())
    session$setInputs(`sidebar-reset_table` = 2L)
    session$setInputs(confirm_table_reset = 2L)
    expect_identical(current_table(), renamed_table)
    expect_true(tracer_network_data_identical(working_network(), saved_network()))
    expect_false(unsaved_changes())
  })
})

test_that("whole-network ZIP actions replace or nest a hierarchy", {
  imported_network <- add_tracer_capsule(new_tracer_network(), "", "ImportedChild")
  archive <- tempfile(fileext = ".zip")
  tracer_network_to_zip(imported_network, archive)
  app <- shiny::shinyAppDir(system.file("app", package = "TraceR"))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(`sidebar-network_zip_file` = list(
      name = "network.zip",
      size = file.info(archive)$size,
      type = "application/zip",
      datapath = archive
    ))
    session$setInputs(`sidebar-import_network` = 1L)
    session$flushReact()

    expect_true("ImportedChild" %in% names(working_network()$tables))
    expect_null(pending_replacement())

    session$setInputs(`sidebar-import_capsule_id` = "ImportedAsCapsule")
    session$setInputs(`sidebar-import_network_as_capsule` = 1L)
    session$flushReact()
    expect_true("ImportedAsCapsule" %in% names(working_network()$tables))
    expect_true("ImportedAsCapsule/ImportedChild" %in% names(working_network()$tables))
    expect_identical(validate_tracer_network(working_network())$errors$path, character())
  })
})

test_that("session restoration reinstalls the full network and selected path", {
  network <- createExampleTracerNetwork()
  app <- shiny::shinyAppDir(system.file("app", package = "TraceR"))

  shiny::testServer(app$serverFuncSource(), {
    actions <- environment(request_replacement)$sidebar
    actions$uploaded_session <- shiny::reactiveVal(list(list(
      data = list(tracer_network = network, selected_path = "C1"),
      inputs = list()
    )))
    environment(request_replacement)$sidebar <- actions
    session$flushReact()

    expect_identical(working_network(), network)
    expect_identical(selected_path(), "C1")
    expect_identical(current_table(), get_tracer_table(network, "C1"))
  })
})

test_that("the loaded example's analysis capsule is navigable", {
  app <- shiny::shinyAppDir(system.file("app", package = "TraceR"))

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    session$setInputs(`sidebar-generate_example` = 1L)
    session$setInputs(`sidebar-capsule_tree` = list(
      Root = structure(
        list(C1 = structure("", stselected = TRUE)),
        stselected = FALSE
      )
    ))
    expect_identical(pending_navigation(), "C1")
    session$setInputs(save_navigation = 1L)
    expect_identical(selected_path(), "C1")
    expect_identical(current_table(), get_tracer_table(createExampleTracerNetwork(), "C1"))
  })
})

test_that("tree refreshes do not overwrite navigation into an imported capsule", {
  example <- createExampleTracerTable()
  network <- new_tracer_network(
    example,
    stats::setNames(list(new_tracer_table()), "C1")
  )
  archive <- tempfile(fileext = ".zip")
  on.exit(unlink(archive), add = TRUE)
  tracer_network_to_zip(network, archive)
  app <- shiny::shinyAppDir(system.file("app", package = "TraceR"))

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    session$setInputs(`sidebar-network_zip_file` = list(datapath = archive))
    session$setInputs(`sidebar-import_capsule_id` = "Imported")
    session$setInputs(`sidebar-import_network_as_capsule` = 1L)

    choose_imported <- list(Root = structure(
      list(Imported = structure(list(C1 = ""), stselected = TRUE)),
      stselected = FALSE
    ))
    root_echo <- list(Root = structure(
      list(Imported = list(C1 = "")),
      stselected = TRUE
    ))
    session$setInputs(`sidebar-capsule_tree` = choose_imported)
    expect_identical(pending_navigation(), "Imported")

    session$setInputs(`sidebar-capsule_tree` = root_echo)
    expect_identical(pending_navigation(), "Imported")
    session$setInputs(cancel_navigation = 1L)
    expect_identical(selected_path(), "")
    expect_null(pending_navigation())

    session$setInputs(`sidebar-capsule_tree` = choose_imported)
    expect_identical(pending_navigation(), "Imported")
    session$setInputs(`sidebar-capsule_tree` = root_echo)
    session$setInputs(save_navigation = 1L)
    expect_identical(selected_path(), "Imported")
    expect_true(tracer_table_data_identical(current_table(), example))
    expect_identical(get_tracer_table(working_network(), "Imported/C1"), new_tracer_table())
  })
})

test_that("load actions confirm dirty replacements without changing the checkpoint", {
  actions <- list(
    generate_example = shiny::reactiveVal(0L),
    empty_network = shiny::reactiveVal(0L),
    save_table = shiny::reactiveVal(0L),
    reset_table = shiny::reactiveVal(0L),
    add_capsule = shiny::reactiveVal(0L),
    new_capsule_id = shiny::reactiveVal(""),
    rename_capsule = shiny::reactiveVal(0L),
    rename_capsule_id = shiny::reactiveVal(""),
    collapse_capsule = shiny::reactiveVal(0L),
    delete_capsule = shiny::reactiveVal(0L),
    network_zip_file = shiny::reactiveVal(NULL),
    import_network = shiny::reactiveVal(0L),
    import_capsule_id = shiny::reactiveVal(""),
    import_network_as_capsule = shiny::reactiveVal(0L),
    uploaded_session = shiny::reactiveVal(NULL),
    capsule_selection = shiny::reactiveVal(NULL)
  )
  app <- shiny::shinyAppDir(system.file("app", package = "TraceR"))

  shiny::testServer(app$serverFuncSource(), {
    environment(request_replacement)$sidebar <- actions
    session$flushReact()
    checkpoint <- saved_network()

    actions$generate_example(1L)
    session$flushReact()
    expect_identical(current_table(), createExampleTracerTable())
    expect_identical(working_network(), createExampleTracerNetwork())
    expect_null(pending_replacement())
    expect_identical(saved_network(), checkpoint)

    working_data <- current_table()$data
    working_data$Label[1] <- "Unsaved work"
    current_table(new_tracer_table(working_data))
    working_table <- current_table()

    actions$generate_example(2L)
    session$flushReact()
    expect_identical(current_table(), working_table)
    expect_identical(pending_replacement()$source, "example")
    session$setInputs(cancel_table_replacement = 1L)
    expect_identical(current_table(), working_table)
    expect_null(pending_replacement())

    actions$empty_network(1L)
    session$flushReact()
    expect_identical(current_table(), working_table)
    expect_identical(pending_replacement()$source, "empty")
    session$setInputs(cancel_table_replacement = 2L)
    expect_identical(current_table(), working_table)

    actions$generate_example(3L)
    session$flushReact()
    session$setInputs(confirm_table_replacement = 1L)
    expect_identical(current_table(), createExampleTracerTable())
    expect_identical(working_network(), createExampleTracerNetwork())
    expect_identical(saved_network(), checkpoint)

    actions$empty_network(2L)
    session$flushReact()
    expect_identical(pending_replacement()$source, "empty")
    session$setInputs(confirm_table_replacement = 2L)
    expect_identical(current_table(), new_tracer_table())
    expect_identical(saved_network(), checkpoint)
    expect_false(unsaved_changes())

    actions$empty_network(3L)
    session$flushReact()
    expect_null(pending_replacement())
  })
})

test_that("keys are loaded independently in the application server", {
  withr::local_envvar(c(RUNNING_IN_DOCKER = "TRUE"))
  test_private_key <- structure(list(), class = "test_private_key")
  test_public_key <- structure(list(), class = "test_public_key")
  testthat::local_mocked_bindings(
    read_key = function(path) test_private_key,
    read_pubkey = function(path) test_public_key,
    .package = "openssl"
  )
  app <- shiny::shinyAppDir(system.file("app", package = "TraceR"))
  server <- app$serverFuncSource()
  server_environment <- new.env(parent = environment(server))
  environment(server) <- server_environment

  server_environment$file.exists <- function(path) basename(path) == "public_key.pem"
  shiny::testServer(server, {
    expect_null(private_key)
    expect_identical(public_key, test_public_key)
  })

  server_environment$file.exists <- function(path) basename(path) == "private_key.pem"
  shiny::testServer(server, {
    expect_identical(private_key, test_private_key)
    expect_null(public_key)
  })
})
test_that("rename and delete controls are disabled at the root", {
  path <- shiny::reactiveVal("")
  shiny::testServer(
    mod_sidebar_server,
    args = list(
      root_inputs = shiny::reactiveValues(),
      current_network = shiny::reactive(new_tracer_network()),
      selected_path = path
    ),
    {
      expect_no_error(session$flushReact())
      path("A")
      expect_no_error(session$flushReact())
    }
  )
})

test_that("shared operation feedback has a bounded scroll area", {
  ui <- paste(
    readLines(system.file("app/ui.R", package = "TraceR")),
    collapse = "\n"
  )
  css <- paste(
    readLines(system.file("app/www/custom.css", package = "TraceR")),
    collapse = "\n"
  )

  expect_match(ui, 'uiOutput("operation_feedback")', fixed = TRUE)
  expect_match(css, "max-height: 320px", fixed = TRUE)
  expect_match(css, "overflow-y: auto", fixed = TRUE)
})
