test_that("shared sidebar exposes current table and session controls", {
  html <- htmltools::renderTags(mod_sidebar_ui("sidebar"))$html

  expect_match(html, "Network Table", fixed = TRUE)
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
  expect_match(html, 'id="sidebar-delete_capsule"', fixed = TRUE)
  expect_match(html, 'id="sidebar-download_network"', fixed = TRUE)
  expect_match(html, 'id="sidebar-network_zip_file"', fixed = TRUE)
  expect_match(html, 'id="sidebar-import_network"', fixed = TRUE)
  expect_match(html, 'id="sidebar-import_network_as_capsule"', fixed = TRUE)
  expect_match(html, 'id="sidebar-table_import-openPopup"', fixed = TRUE)
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
      current_table = shiny::reactiveVal(new_tracer_table()),
      root_inputs = shiny::reactiveValues()
    ),
    {
      expect_true(is.list(session$returned))
      expect_named(session$returned, c(
        "generate_example", "empty_network", "save_table", "reset_table",
        "add_capsule", "new_capsule_id", "rename_capsule", "rename_capsule_id",
        "delete_capsule", "network_zip_file", "import_network", "import_capsule_id",
        "import_network_as_capsule", "table_import", "table_import_request_id",
        "uploaded_session", "capsule_selection"
      ))
      session$setInputs(generate_example = 1)
      expect_equal(session$returned$generate_example(), 1)
      session$setInputs(empty_network = 1)
      expect_equal(session$returned$empty_network(), 1)
      session$setInputs(save_table = 1)
      expect_equal(session$returned$save_table(), 1)
      session$setInputs(reset_table = 1)
      expect_equal(session$returned$reset_table(), 1)
      session$setInputs(`table_import-accept` = 1)
      session$flushReact()
      expect_equal(session$returned$table_import_request_id(), 1L)
      session$setInputs(`table_import-accept` = 2)
      session$flushReact()
      expect_equal(session$returned$table_import_request_id(), 2L)
      session$flushReact()
      expect_match(output$table_download_controls$html, "Download", fixed = TRUE)
      expect_false(grepl("Download Signed", output$table_download_controls$html, fixed = TRUE))
      expect_false(grepl("Experimental: testing only", output$table_download_controls$html, fixed = TRUE))
      expect_match(output$unsaved_status$html, "Saved", fixed = TRUE)
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
      current_table = shiny::reactiveVal(new_tracer_table()),
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

test_that("table imports emit once per completed request, including identical payloads", {
  shiny::testServer(
    mod_sidebar_server,
    args = list(
      current_table = shiny::reactiveVal(new_tracer_table()),
      root_inputs = shiny::reactiveValues()
    ),
    {
      tracker <- new.env(parent = emptyenv())
      tracker$events <- list()
      shiny::observeEvent(session$returned$table_import(), {
        tracker$events <- append(tracker$events, list(session$returned$table_import()))
      })
      session$setInputs(`table_import-accept` = 0L)
      session$flushReact()

      first_payload <- list(file = list(payload = "first"))
      imported_table_json(first_payload)
      session$flushReact()
      expect_length(tracker$events, 0L)

      session$setInputs(`table_import-accept` = 1L)
      session$flushReact()
      expect_length(tracker$events, 1L)
      expect_identical(tracker$events[[1]], list(request_id = 1L, data = first_payload))

      second_payload <- list(file = list(payload = "second"))
      imported_table_json(second_payload)
      session$flushReact()
      expect_length(tracker$events, 1L)

      session$setInputs(`table_import-accept` = 2L)
      session$flushReact()
      expect_length(tracker$events, 2L)
      expect_identical(tracker$events[[2]], list(request_id = 2L, data = second_payload))

      imported_table_json(second_payload)
      session$setInputs(`table_import-accept` = 3L)
      session$flushReact()
      expect_length(tracker$events, 3L)
      expect_identical(tracker$events[[3]], list(request_id = 3L, data = second_payload))
    }
  )
})

test_that("signed table download is shown only when a private key is available", {
  shiny::testServer(
    mod_sidebar_server,
    args = list(
      current_table = shiny::reactiveVal(new_tracer_table()),
      root_inputs = shiny::reactiveValues(),
      private_key = structure(list(), class = "test_private_key")
    ),
    {
      session$flushReact()
      expect_match(output$table_download_controls$html, "Download", fixed = TRUE)
      expect_match(output$table_download_controls$html, "Download Signed", fixed = TRUE)
      expect_match(output$table_download_controls$html, "Experimental: testing only", fixed = TRUE)
      expect_match(output$table_download_controls$html, "Keep an unsigned JSON copy", fixed = TRUE)
      expect_match(output$table_download_controls$html, 'class="alert alert-warning"', fixed = TRUE)
      expect_match(output$table_download_controls$html, "<strong>Experimental: testing only</strong>", fixed = TRUE)
    }
  )
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
    delete_capsule = shiny::reactiveVal(0L),
    network_zip_file = shiny::reactiveVal(NULL),
    import_network = shiny::reactiveVal(0L),
    import_capsule_id = shiny::reactiveVal(""),
    import_network_as_capsule = shiny::reactiveVal(0L),
    table_import = shiny::reactiveVal(NULL),
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