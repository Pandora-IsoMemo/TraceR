library(TraceR)

tagList(
  navbarPage(
    title = paste("TraceR", packageVersion("TraceR")),
    theme = shinythemes::shinytheme("flatly"),
    position = "fixed-top",
    collapsible = TRUE,
    id = "tab",
    tabPanel(
      title = "Network",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          actionButton("generate_flowchart", "Generate Flowchart"),
          tags$br(),
          tags$br(),
          # this is only a placeholder for actual inputs,
          # can be removed whe we have more inputs that can be stored in a session download:
          textInput("test_input", "Test Input", placeholder = "Please add some text ..."),
          tags$br(),
          uiOutput("conditional_download_buttons"),
          checkboxInput(
            inputId = "download_inputs",
            label = "Download user inputs and graph",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.download_inputs == true",
            DataTools::downloadModelUI(
              id = "session_download",
              label = "Download Session"
            ),
            tags$hr()
          ),
          importModuleUI("import"),
          tags$br(),
          tags$br()
        ),
        mainPanel(
          DiagrammeR::grVizOutput("flowchart", width = "800px", height = "800px"),
          textOutput("clickMessage")
        ),
      )
    )
  ),
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "pandora-isomemo.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "https://unpkg.com/panzoom@9.4.0/dist/panzoom.min.js")
  ),
  shinyjs::useShinyjs()
)
