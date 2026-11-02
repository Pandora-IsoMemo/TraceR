test_that("table module edits, adds, deletes, and renames shared data", {
  shared_table <- shiny::reactiveVal(new_tracer_table())

  shiny::testServer(
    mod_table_server,
    args = list(current_table = shared_table),
    {
      expect_equal(nrow(shared_table()$data), 0L)
      session$setInputs(add_row = 1)
      expect_equal(nrow(shared_table()$data), 1L)

      session$setInputs(table_cell_edit = list(row = 1, col = 0, value = "N1"))
      expect_identical(shared_table()$data$ID, "N1")
      session$setInputs(table_cell_edit = list(row = 1, col = 1, value = "Updated label"))
      expect_identical(shared_table()$data$ID, "N1")
      expect_identical(shared_table()$data$Label, "Updated label")

      session$setInputs(table_cell_edit = list(row = 1, col = 2, value = "workflow"))
      expect_identical(shared_table()$data$Type, "workflow")
      expect_true(any(validate_tracer_table(shared_table())$errors$field == "Type"))

      session$setInputs(optional_column = "Optional 1", optional_column_name = "ID")
      session$setInputs(rename_optional_column = 1)
      expect_true("Optional 1" %in% names(shared_table()$data))

      session$setInputs(optional_column_name = "Research team")
      session$setInputs(rename_optional_column = 2)
      expect_identical(names(shared_table()$data)[7], "Research team")

      session$setInputs(table_rows_selected = 1, delete_row = 1)
      expect_equal(nrow(shared_table()$data), 0L)
    }
  )
})

test_that("table module UI is namespaced and includes DT", {
  html <- htmltools::renderTags(mod_table_ui("table"))$html

  expect_match(html, 'id="table-table"', fixed = TRUE)
  expect_match(html, 'id="table-add_row"', fixed = TRUE)
  expect_match(html, 'id="table-delete_row"', fixed = TRUE)
})