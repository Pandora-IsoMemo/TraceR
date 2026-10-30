library(shiny)
library(TraceR)

tagList(
  navbarPage(
    title = paste("TraceR", packageVersion("TraceR")),
    theme = shinythemes::shinytheme("flatly"),
    position = "fixed-top",
    collapsible = TRUE,
    id = "tab",
    tabPanel(
      title = "Network Project",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          mod_network_sidebar_ui("network")
        ),
        mainPanel(
          tabsetPanel(
            id = "network_tabs",
            tabPanel(
              title = "Network",
              mod_network_ui("network")
            )
          )
        )
      )
    )
  ),
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "pandora-isomemo.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "https://unpkg.com/panzoom@9.4.0/dist/panzoom.min.js")
  ),
  shinyjs::useShinyjs(),
  shinyTools::headerButtonsUI(
    id = "header",
    help_link = "https://pandora-isomemo.github.io/TraceR/"
  )
)
