test_that("shared sidebar exposes example and session controls only", {
  html <- htmltools::renderTags(mod_sidebar_ui("sidebar"))$html

  expect_match(html, 'id="sidebar-generate_example"', fixed = TRUE)
  expect_match(html, "Example Network", fixed = TRUE)
  expect_match(html, 'id="sidebar-empty_network"', fixed = TRUE)
  expect_match(html, "Empty Network", fixed = TRUE)
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
      expect_named(session$returned, c("generate_example", "empty_network", "uploaded_session"))
      session$setInputs(generate_example = 1)
      expect_equal(session$returned$generate_example(), 1)
      session$setInputs(empty_network = 1)
      expect_equal(session$returned$empty_network(), 1)
    }
  )
})