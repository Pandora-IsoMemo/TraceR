#' Network module UI
#'
#' @param id Module ID
#' @return Network graph UI
#' @export
mod_network_ui <- function(id) {
  ns <- NS(id)

  tagList(
    DiagrammeR::grVizOutput(ns("flowchart"), width = "800px", height = "800px"),
    textOutput(ns("clickMessage"))
  )
}

#' Network sidebar UI
#'
#' @param id Module ID
#' @return Network controls UI
#' @export
mod_network_sidebar_ui <- function(id) {
  ns <- NS(id)

  tagList(
    actionButton(ns("generate_flowchart"), "Generate Flowchart"),
    tags$br(),
    textInput(ns("test_input"), "Test Input", placeholder = "Please add some text ..."),
    tags$br(),
    textInput(ns("test_input"), "Test Input", placeholder = "Please add some text ..."),
    tags$br(),
    uiOutput(ns("conditional_download_buttons")),
    checkboxInput(
      inputId = ns("download_inputs"),
      label = "Download user inputs and graph",
      value = FALSE
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("download_inputs")),
      DataTools::downloadModelUI(
        id = ns("session_download"),
        label = "Download Session"
      ),
      tags$hr()
    ),
    importModuleUI(ns("import")),
    tags$br(),
    tags$br()
  )
}

#' Network module server
#'
#' @param id Module ID
#' @param private_key Private key used to sign graph downloads
#' @param public_key Public key used to validate imported graphs
#' @return Reactive graph state
#' @export
mod_network_server <- function(id, private_key = NULL, public_key = NULL) {
  moduleServer(id, function(input, output, session) {
    graph <- reactiveVal()
    upload_description <- reactiveVal()

    observeEvent(
      input$generate_flowchart,
      graph(createExampleGraph())
    )

    output$flowchart <- DiagrammeR::renderGrViz({
      withProgress(renderFlowchart(graph),
        message = "Rendering graph...",
        value = 0.75
      )
    })

    output$conditional_download_buttons <- renderUI({
      if (
        !is.na(Sys.getenv("SHINYPROXY_USERID", unset = NA)) &&
          !is.null(private_key)
      ) {
        tagList(
          downloadModuleUI(session$ns("download_unsigned"), label = "Download JSON"),
          tags$br(),
          tags$br(),
          downloadModuleUI(session$ns("download_signed"), label = "Download Signed JSON")
        )
      } else {
        downloadModuleUI(session$ns("download_unsigned"), label = "Download JSON")
      }
    })

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
    displayNodeId(input, output, outputId = "clickMessage")
    initialize_graph_zooming(session$ns("flowchart"))

    graph
  })
}