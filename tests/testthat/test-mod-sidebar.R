test_that("shared sidebar exposes example and session controls only", {
  html <- htmltools::renderTags(mod_sidebar_ui("sidebar"))$html

  expect_match(html, 'id="sidebar-generate_example"', fixed = TRUE)
  expect_match(html, "Example Network", fixed = TRUE)
  expect_match(html, 'id="sidebar-empty_network"', fixed = TRUE)
  expect_match(html, "Empty Network", fixed = TRUE)
  expect_match(html, "Save", fixed = TRUE)
  expect_match(html, "Reset", fixed = TRUE)
  expect_match(html, "Import Network Table", fixed = TRUE)
  expect_match(html, "Download Session", fixed = TRUE)
  expect_match(html, "Import Session", fixed = TRUE)
  expect_false(grepl("Import JSON", html, fixed = TRUE))
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
      expect_match(output$table_download_controls$html, "Download Network Table", fixed = TRUE)
      expect_false(grepl("Download Signed Network Table", output$table_download_controls$html, fixed = TRUE))
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
      expect_match(output$table_download_controls$html, "Download Network Table", fixed = TRUE)
      expect_match(output$table_download_controls$html, "Download Signed Network Table", fixed = TRUE)
    }
  )
})