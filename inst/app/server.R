library(TraceR)

shinyServer(function(input, output, session) {
  # Reactive graph element that is updated regularly
  graph <- reactiveVal()
  upload_description <- reactiveVal()

  # Create example graph after click on button
  observeEvent(input$generate_flowchart,
               graph(createExampleGraph()))

  # Automatically render the graph after updates
  output$flowchart <- DiagrammeR::renderGrViz({
    withProgress(renderFlowchart(graph),
                 message = "Rendering graph...",
                 value = 0.75)
  })

  # Download and upload
  downloadModuleServer("download_unsigned", graph = graph)

  # export inputs and graph
  DataTools::downloadModelServer("session_download",
                      dat = reactive(asGraphList(graph())),
                      inputs = input,
                      model = reactive(NULL),
                      rPackageName = config()[["rPackageName"]],
                      defaultFileName = config()[["defaultFileName"]],
                      fileExtension = config()[["fileExtension"]],
                      modelNotes = upload_description,
                      triggerUpdate = reactive(TRUE),
                      onlySettings = TRUE)

  uploaded_data <- importModuleServer("import")
  updateGraph(graph = graph, uploadedGraph = reactive(uploaded_data$graph))
  updateInput(input, output, session, uploaded_inputs = reactive(uploaded_data$input))

  # Display clicked node id
  displayNodeId(input, output, outputId = "clickMessage")

  # Custom js to add panzoom after the svg was created
  shinyjs::runjs('
  function initializePanzoom() {
    var element = document.querySelector("#flowchart svg");
    if (element && element.tagName.toLowerCase() === "svg") {
      panzoom(element, {
        bounds: true,
        boundsPadding: 0.1
      });
    } else {
      // Retry after a short delay if the SVG is not available yet
      setTimeout(initializePanzoom, 100);
    }
  }
  initializePanzoom();  // Initial call to check and start panzoom
')
})
