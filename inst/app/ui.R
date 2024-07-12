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
          downloadModuleUI("download_unsigned"),
          importModuleUI("import"),
          tags$br(),
          tags$br()
        ),
        mainPanel(
          grVizOutput("flowchart", width = "600px", height = "600px"),
          textOutput("clickMessage")
        ),
      )
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "https://unpkg.com/panzoom@9.4.0/dist/panzoom.min.js")
  ),
  shinyjs::useShinyjs()
)
