test_that("shared sidebar exposes current table and session controls", {
  html <- htmltools::renderTags(mod_sidebar_ui("sidebar"))$html

  expect_match(html, "Network Table", fixed = TRUE)
  expect_match(html, 'id="sidebar-generate_example"', fixed = TRUE)
  expect_match(html, "Load Example", fixed = TRUE)
  expect_match(html, 'id="sidebar-empty_network"', fixed = TRUE)
  expect_match(html, "Load Empty", fixed = TRUE)
  expect_match(html, "Save", fixed = TRUE)
  expect_match(html, "Reset", fixed = TRUE)
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
        "table_import", "table_import_request_id", "uploaded_session"
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
      expect_match(output$unsaved_status$html, "Saved", fixed = TRUE)
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
    }
  )
})

test_that("load actions confirm dirty replacements without changing the checkpoint", {
  actions <- list(
    generate_example = shiny::reactiveVal(0L),
    empty_network = shiny::reactiveVal(0L),
    save_table = shiny::reactiveVal(0L),
    reset_table = shiny::reactiveVal(0L),
    table_import = shiny::reactiveVal(NULL),
    uploaded_session = shiny::reactiveVal(NULL)
  )
  app <- shiny::shinyAppDir(system.file("app", package = "TraceR"))

  shiny::testServer(app$serverFuncSource(), {
    environment(request_replacement)$sidebar <- actions
    session$flushReact()
    checkpoint <- saved_table()

    actions$generate_example(1L)
    session$flushReact()
    expect_identical(current_table(), createExampleTracerTable())
    expect_null(pending_replacement())
    expect_identical(saved_table(), checkpoint)

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
    expect_identical(saved_table(), checkpoint)

    actions$empty_network(2L)
    session$flushReact()
    expect_identical(pending_replacement()$source, "empty")
    session$setInputs(confirm_table_replacement = 2L)
    expect_identical(current_table(), new_tracer_table())
    expect_identical(saved_table(), checkpoint)
    expect_false(unsaved_changes())

    actions$empty_network(3L)
    session$flushReact()
    expect_null(pending_replacement())
  })
})