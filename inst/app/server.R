shinyServer(function(input, output, session) {
  # Reactive graph element that is updated regularly
  graph <- reactiveVal()
  upload_description <- reactiveVal()

  # Create example graph after click on button
  observe(graph(createExampleGraph())) %>% bindEvent(input$generate_flowchart)

  # Automatically render the graph after updates
  output$flowchart <- renderGrViz({
    renderFlowchart(graph)
  })

  # Download and upload
  downloadModuleServer("download_unsigned", graph = graph, upload_description = upload_description)
  uploadedGraph <- importModuleServer("import")
  updateGraph(graph = graph, uploadedGraph = uploadedGraph)

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
