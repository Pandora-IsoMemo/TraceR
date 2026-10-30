test_that("Network module generates the example graph", {
  shiny::testServer(
    mod_network_server,
    args = list(private_key = NULL, public_key = NULL),
    {
      expect_null(graph())

      session$setInputs(generate_flowchart = 1)

      expect_equal(nrow(graph()$nodes_df), 100L)
      expect_gt(nrow(graph()$edges_df), 0L)
    }
  )
})

test_that("graph list imports preserve node and edge data", {
  graph <- createExampleGraph()

  imported_graph <- createGraphFromUpload(asGraphList(graph))

  expect_equal(imported_graph$nodes_df, graph$nodes_df)
  expect_equal(imported_graph$edges_df, graph$edges_df)
})

test_that("Network UI uses its module namespace", {
  ui <- htmltools::tagList(
    mod_network_sidebar_ui("network"),
    mod_network_ui("network")
  )
  html <- htmltools::renderTags(ui)$html

  expect_match(html, 'id="network-generate_flowchart"', fixed = TRUE)
  expect_match(html, 'id="network-download_inputs"', fixed = TRUE)
  expect_match(html, 'id="network-flowchart"', fixed = TRUE)
  expect_match(html, 'input[&#39;network-download_inputs&#39;] == true', fixed = TRUE)
})

test_that("signed downloads are shown only when a private key is available", {
  withr::local_envvar(c(SHINYPROXY_USERID = "test-user"))

  shiny::testServer(
    mod_network_server,
    args = list(private_key = NULL, public_key = NULL),
    {
      session$flushReact()
      download_ui <- output$conditional_download_buttons$html

      expect_match(download_ui, "download_unsigned-download", fixed = TRUE)
      expect_false(grepl("download_signed-download", download_ui, fixed = TRUE))
    }
  )

  shiny::testServer(
    mod_network_server,
    args = list(private_key = structure(list(), class = "test_private_key"), public_key = NULL),
    {
      session$flushReact()
      download_ui <- output$conditional_download_buttons$html

      expect_match(download_ui, "download_unsigned-download", fixed = TRUE)
      expect_match(download_ui, "download_signed-download", fixed = TRUE)
    }
  )
})