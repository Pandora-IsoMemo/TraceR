library(TraceR)

shinyServer(function(input, output, session) {
  # Shinyproxy user id
  user_id <- Sys.getenv("SHINYPROXY_USERID", unset = NA) # change this to unset = 123 for local testing
  private_key <- openssl::read_key("private_key.pem") # to do: store keys and access them here

  # Reactive graph element that is updated regularly
  graph <- reactiveVal()
  upload_description <- reactiveVal()

  # Create example graph after click on button
  observeEvent(input$generate_flowchart,
               graph(createExampleGraph()))

  # Automatically render the graph after updates
  output$flowchart <- DiagrammeR::renderGrViz({
    renderFlowchart(graph)  %>%
      withProgress(message = "Rendering graph...", value = 0.75)
  })

  # Download and upload
  createDownloadModuleUI(output, user_id)
  downloadModuleServer("download_unsigned", graph = graph, private_key = private_key, signed = FALSE)
  downloadModuleServer("download_signed", graph = graph, private_key = private_key, signed = TRUE)

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
