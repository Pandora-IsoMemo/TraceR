#' Shared sidebar UI
#'
#' Displays example/empty-network actions and session import/download controls.
#' Session downloads store the shared `tracer_table` as data and capture the
#' root Shiny inputs. Session imports are returned to the application server,
#' which validates and installs the table before restoring those inputs.
#'
#' @param id Module ID
#' @return Shared application controls
#' @export
mod_sidebar_ui <- function(id) {
  ns <- NS(id)

  tagList(
    actionButton(ns("generate_example"), "Example Network"),
    tags$br(),
    actionButton(ns("empty_network"), "Empty Network"),
    tags$br(),
    checkboxInput(
      inputId = ns("download_inputs"),
      label = "Download user inputs and network",
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
    DataTools::importUI(ns("session_import"), label = "Import Session")
  )
}

#' Shared sidebar server
#'
#' The sidebar does not own network data. It returns example/empty-network
#' actions and session-import reactives for the application server to coordinate.
#'
#' @param id Module ID
#' @param current_table Reactive value containing the shared `tracer_table`
#' @param root_inputs Root Shiny input object for session persistence
#' @return A list of sidebar actions and imported session data
#' @export
mod_sidebar_server <- function(id, current_table, root_inputs) {
  moduleServer(id, function(input, output, session) {
    DataTools::downloadModelServer(
      "session_download",
      dat = reactive(list(tracer_table = current_table())),
      inputs = root_inputs,
      model = reactive(NULL),
      rPackageName = config()[["rPackageName"]],
      defaultFileName = config()[["defaultFileName"]],
      fileExtension = config()[["fileExtension"]],
      modelNotes = reactive(""),
      triggerUpdate = reactive(TRUE),
      onlySettings = TRUE
    )

    uploaded_session <- DataTools::importServer(
      "session_import",
      title = "Session import",
      importType = "model",
      defaultSource = config()[["defaultSource"]][["session"]],
      ckanFileTypes = config()[["ckanFileTypes"]][["session"]],
      onlySettings = TRUE,
      options = DataTools::importOptions(rPackageName = config()[["rPackageName"]])
    )

    list(
      generate_example = reactive(input$generate_example),
      empty_network = reactive(input$empty_network),
      uploaded_session = uploaded_session
    )
  })
}