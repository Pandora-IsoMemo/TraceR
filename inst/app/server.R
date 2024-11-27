library(TraceR)

shinyServer(function(input, output, session) {
  # Adjust Path to environment
  if (!is.na(Sys.getenv("RUNNING_IN_DOCKER", unset = NA))) {
    path_private <- "/app/inst/app/private_key.pem"
    path_public <- "/app/inst/app/public_key.pem"
  } else {
    path_private <- system.file("app/private_key.pem", package = "TraceR")
    path_public <- system.file("app/public_key.pem", package = "TraceR")
  }

  # Check if key files exists
  if (file.exists(path_private) && file.exists(path_public)) {
    private_key <- openssl::read_key(path_private)
    public_key <- openssl::read_pubkey(path_public)
  } else {
    private_key <- NULL
    public_key <- NULL
  }

  # Reactive graph element that is updated regularly
  graph <- reactiveVal()
  upload_description <- reactiveVal()

  # Create example graph after click on button
  observeEvent(
    input$generate_flowchart,
    graph(createExampleGraph())
  )

  # Automatically render the graph after updates
  output$flowchart <- DiagrammeR::renderGrViz({
    withProgress(renderFlowchart(graph),
      message = "Rendering graph...",
      value = 0.75
    )
  })

  # Download and upload
  createDownloadModuleUI(output, private_key)
  downloadModuleServer(
    "download_unsigned",
    graph = graph,
    private_key = private_key,
    signed = FALSE
  )
  downloadModuleServer(
    "download_signed",
    graph = graph,
    private_key = private_key,
    signed = TRUE
  )

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
    onlySettings = TRUE
  )

  uploaded_data <- importModuleServer("import", public_key)
  updateGraph(graph = graph, uploadedGraph = reactive(uploaded_data$graph))
  updateInput(input, output, session, uploaded_inputs = reactive(uploaded_data$input))

  # Display clicked node id
  displayNodeId(input, output, outputId = "clickMessage")

  # Custom js to add panzoom after the svg was created
  initialize_graph_zooming()
})
