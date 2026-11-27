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

test_that("node and edge ID cell edits publish coherent example tables atomically", {
  shared_table <- shiny::reactiveVal(createExampleTracerTable())
  shiny::testServer(mod_table_server, args = list(current_table = shared_table), {
    changes <- list()
    shiny::observeEvent(shared_table(), {
      changes[[length(changes) + 1L]] <<- shared_table()
    }, ignoreInit = TRUE)
    session$flushReact()

    for (target_id in c("N1", "N4", "E4")) {
      before <- shared_table()
      changes <- list()
      session$setInputs(table_cell_edit = list(
        row = match(target_id, before$data$ID), col = 0L, value = tolower(target_id)
      ))
      expect_length(changes, 1L)
      expect_identical(changes[[1]], shared_table())
      expect_equal(nrow(validate_tracer_table(shared_table())$warnings), 0L)
      expect_equal(nrow(validate_tracer_table(shared_table())$errors), 0L)
      if (target_id == "E4") {
        expect_identical(shared_table()$data$From, before$data$From)
        expect_identical(shared_table()$data$To, before$data$To)
      }
    }
    expect_identical(shared_table()$data$From[c(7, 11)], c("n1", "n4"))
    expect_identical(shared_table()$data$To[10], "n4")
    expect_null(output$feedback)
  })
})

test_that("ID cell edits report rejected renames and ambiguous repairs", {
  initial <- createExampleTracerTable()
  initial$data$ID[2] <- "N1"
  shared_table <- shiny::reactiveVal(initial)
  shiny::testServer(mod_table_server, args = list(current_table = shared_table), {
    for (new_id in c("E1", "C1", "bad.id", "bad__id", "")) {
      session$setInputs(table_cell_edit = list(row = 1L, col = 0L, value = new_id))
      expect_identical(shared_table(), initial)
      expect_match(output$feedback$html, "alert-warning", fixed = TRUE)
    }

    session$setInputs(table_cell_edit = list(row = 1L, col = 0L, value = "Fixed"))
    expect_identical(shared_table()$data$ID[1], "Fixed")
    expect_identical(shared_table()$data$From, initial$data$From)
    expect_identical(shared_table()$data$To, initial$data$To)
    expect_match(output$feedback$html, "Edge references were not changed", fixed = TRUE)

    session$setInputs(table_cell_edit = list(row = 1L, col = 0L, value = "fixed"))
    expect_null(output$feedback)
  })
})

test_that("capsule lifecycle cells and rows are managed outside DT", {
  data <- new_tracer_table()$data
  data[1, ] <- ""
  data$ID[1] <- "C1"
  data$Type[1] <- "capsule"
  shared_table <- shiny::reactiveVal(new_tracer_table(data))

  shiny::testServer(
    mod_table_server,
    args = list(current_table = shared_table),
    {
      session$setInputs(table_cell_edit = list(row = 1, col = 0, value = "C2"))
      expect_identical(shared_table()$data$ID, "C1")

      session$setInputs(table_cell_edit = list(row = 1, col = 2, value = "node"))
      expect_identical(shared_table()$data$Type, "capsule")

      session$setInputs(table_rows_selected = 1, delete_row = 1)
      expect_equal(nrow(shared_table()$data), 1L)
      expect_match(output$feedback$html, "capsule controls", fixed = TRUE)
    }
  )
})

test_that("table module UI is namespaced and includes DT", {
  html <- htmltools::renderTags(mod_table_ui("table"))$html

  expect_match(html, 'id="table-table"', fixed = TRUE)
  expect_match(html, 'id="table-add_row"', fixed = TRUE)
  expect_match(html, 'id="table-delete_row"', fixed = TRUE)
})

test_that("table imports emit once per completed request, including identical payloads", {
  shiny::testServer(
    mod_table_server,
    args = list(
      current_table = shiny::reactiveVal(new_tracer_table())
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
    mod_table_server,
    args = list(
      current_table = shiny::reactiveVal(new_tracer_table()),
      private_key = structure(list(), class = "test_private_key")
    ),
    {
      session$flushReact()
      expect_match(output$table_download_controls$html, "Download", fixed = TRUE)
      expect_match(output$table_download_controls$html, "Download Signed", fixed = TRUE)
      expect_match(output$table_download_controls$html, "Experimental", fixed = TRUE)
      expect_match(output$table_download_controls$html, "Keep an unsigned JSON copy", fixed = TRUE)
      expect_match(output$table_download_controls$html, 'class="alert alert-warning"', fixed = TRUE)
    }
  )
})

test_that("table JSON controls render below the table without a key", {
  html <- htmltools::renderTags(mod_table_ui("table"))$html
  expect_match(html, 'id="table-table_import-openPopup"', fixed = TRUE)
  expect_match(html, 'id="table-table_download_controls"', fixed = TRUE)

  shiny::testServer(
    mod_table_server,
    args = list(current_table = shiny::reactiveVal(new_tracer_table())),
    {
      session$flushReact()
      expect_match(output$table_download_controls$html, "Download", fixed = TRUE)
      expect_false(grepl("Download Signed", output$table_download_controls$html, fixed = TRUE))
      expect_false(grepl("Experimental", output$table_download_controls$html, fixed = TRUE))
    }
  )
})

test_that("table_import_request_id is exposed", {
  shiny::testServer(
    mod_table_server,
    args = list(current_table = shiny::reactiveVal(new_tracer_table())),
    {
      session$setInputs(`table_import-accept` = 0)
      session$flushReact()
      session$setInputs(`table_import-accept` = 1)
      session$flushReact()
      expect_equal(session$returned$table_import_request_id(), 1L)
      session$setInputs(`table_import-accept` = 2)
      session$flushReact()
      expect_equal(session$returned$table_import_request_id(), 2L)
    }
  )
})
