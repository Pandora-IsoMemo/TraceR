test_that("Network module derives its graph from the shared tracer_table", {
  shared_table <- shiny::reactiveVal(createExampleTracerTable())

  shiny::testServer(
    mod_network_server,
    args = list(current_table = shared_table),
    {
      expect_equal(nrow(graph()$nodes_df), 6L)
      expect_equal(nrow(graph()$edges_df), 5L)
      expect_equal(DiagrammeR::get_node_df(graph())$tracer_id[1], "N1")
      expect_true(any(graph()$global_attrs$attr == "rankdir" & graph()$global_attrs$value == "TB"))
      expect_true(any(graph()$global_attrs$attr == "layout" & graph()$global_attrs$value == "dot"))
      expect_match(output$flowchart_container$html, "html-widget-output", fixed = TRUE)

      session$setInputs(flowchart_click = list(id = 1))
      expect_match(output$clickMessage, "N1", fixed = TRUE)

      changed_data <- shared_table()$data
      changed_data$Label[1] <- "Revised question"
      changed_data$To[changed_data$ID == "E1"] <- "N3"
      shared_table(new_tracer_table(changed_data))
      session$flushReact()
      expect_equal(DiagrammeR::get_node_df(graph())$label[1], "Revised question")
      expect_equal(DiagrammeR::get_edge_df(graph())$to[1], 3L)

      invalid_data <- shared_table()$data
      invalid_data$Type[1] <- "workflow"
      shared_table(new_tracer_table(invalid_data))
      session$flushReact()
      expect_null(graph())
      expect_match(output$flowchart_container$html, "Graph unavailable", fixed = TRUE)
      expect_false(grepl("html-widget-output", output$flowchart_container$html, fixed = TRUE))
    }
  )
})

test_that("graph list imports preserve node and edge data", {
  graph <- suppressWarnings(createExampleGraph())
  graph_list <- suppressWarnings(asGraphList(graph))
  imported_graph <- suppressWarnings(createGraphFromUpload(graph_list))

  expect_equal(imported_graph$nodes_df, graph$nodes_df)
  expect_equal(imported_graph$edges_df, graph$edges_df)
})

test_that("legacy graph helpers signal deprecation", {
  graph <- suppressWarnings(createExampleGraph())
  graph_list <- suppressWarnings(asGraphList(graph))

  expect_warning(createExampleGraph(), class = "lifecycle_warning_deprecated")
  expect_warning(asGraphList(graph), class = "lifecycle_warning_deprecated")
  expect_warning(createGraphFromUpload(graph_list), class = "lifecycle_warning_deprecated")
  expect_warning(downloadModuleUI("legacy", "Download"), class = "lifecycle_warning_deprecated")
  expect_warning(importModuleUI("legacy"), class = "lifecycle_warning_deprecated")
  expect_warning(mod_network_sidebar_ui("legacy"), class = "lifecycle_warning_deprecated")

  expect_warning(
    shiny::testServer(
      function(input, output, session) check_signature_validity(list(), NULL),
      {}
    ),
    class = "lifecycle_warning_deprecated"
  )

  expect_warning(
    shiny::testServer(
      downloadModuleServer,
      args = list(graph = shiny::reactiveVal(graph), private_key = NULL, signed = FALSE),
      {}
    ),
    class = "lifecycle_warning_deprecated"
  )
  expect_warning(
    shiny::testServer(importModuleServer, args = list(public_key = NULL), {}),
    class = "lifecycle_warning_deprecated"
  )
  expect_warning(
    shiny::testServer(
      function(input, output, session) createDownloadModuleUI(output, private_key = NULL),
      {}
    ),
    class = "lifecycle_warning_deprecated"
  )
  expect_warning(
    shiny::testServer(
      function(input, output, session) {
        updateGraph(shiny::reactiveVal(NULL), shiny::reactiveVal(NULL))
      },
      {}
    ),
    class = "lifecycle_warning_deprecated"
  )
  expect_warning(
    shiny::testServer(
      mod_network_server,
      args = list(
        private_key = structure(list(), class = "test_private_key"),
        public_key = structure(list(), class = "test_public_key"),
        current_table = shiny::reactiveVal(createExampleTracerTable())
      ),
      {}
    ),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("Network UI uses its module namespace", {
  ui <- htmltools::tagList(
    mod_sidebar_ui("sidebar"),
    mod_network_ui("network")
  )
  html <- htmltools::renderTags(ui)$html

  expect_match(html, 'id="sidebar-generate_example"', fixed = TRUE)
  expect_match(html, 'id="sidebar-download_inputs"', fixed = TRUE)
  expect_false(grepl("Download JSON", html, fixed = TRUE))
  expect_false(grepl("Download Signed JSON", html, fixed = TRUE))
  expect_match(html, 'input[&#39;sidebar-download_inputs&#39;] == true', fixed = TRUE)
})