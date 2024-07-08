shinyServer(function(input, output, session) {
  # Reactive graph element that is updated regularly
  graph <- reactiveVal()
  upload_description <- reactiveVal()

  # Create example graph after click on button
  observe(graph(createExampleGraph())) %>% bindEvent(input$generate_flowchart)

  # Automatically render the graph after updates
  output$flowchart <- renderGrViz({
    renderFlowchart(graph)  %>%
      withProgress(message = "Rendering graph...", value = 0.75)
  })

  # Download and upload
  downloadModuleServer("download_unsigned", graph = graph)

  # export inputs and graph
  downloadModelServer("session_download",
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
  # Does not work currently (anymore)
  shinyjs::runjs('
    setTimeout(function(){ // 500ms timeout as svg is not immediately available
     var element = document.querySelector("#flowchart svg");
     panzoom(element, {
      bounds: true,
      boundsPadding: 0.1
    });
}, 500);')
})
