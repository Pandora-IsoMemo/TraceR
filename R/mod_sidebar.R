#' Shared sidebar UI
#'
#' Displays network-table actions, persistence controls, and session controls.
#' Session downloads store the shared `tracer_table` as data and capture the
#' root Shiny inputs. Session imports are returned to the application server,
#' which validates and installs the table before restoring those inputs. The
#' Save checkpoint is session-local; Reset restores the last checkpoint after
#' confirmation. Network-table JSON import/export is separate from DataTools
#' session import/export.
#'
#' @param id Module ID
#' @return Shared application controls
#' @export
mod_sidebar_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$h3("Network Table"),
    tags$br(),
    actionButton(ns("generate_example"), "Load Example"),
    tags$br(), tags$br(),
    actionButton(ns("empty_network"), "Load Empty"),
    tags$br(), tags$br(),
    actionButton(ns("save_table"), "Save"),
    actionButton(ns("reset_table"), "Reset"),
    uiOutput(ns("unsaved_status")),
    tags$br(),
    uiOutput(ns("table_download_controls")),
    DataTools::importUI(ns("table_import"), label = "Import"),
    tags$hr(),
    tags$h3("Session"),
    tags$br(),
    DataTools::downloadModelUI(
      id = ns("session_download"),
      label = "Download Session"
    ),
    DataTools::importUI(ns("session_import"), label = "Import Session")
  )
}

#' Shared sidebar server
#'
#' The sidebar does not own network data. It returns table and session actions
#' for the application server to coordinate.
#'
#' @param id Module ID
#' @param current_table Reactive value containing the shared `tracer_table`
#' @param root_inputs Root Shiny input object for session persistence
#' @param unsaved_changes Reactive logical indicating whether the table is dirty
#' @param private_key Optional RSA private key for signed table downloads
#' @return A list of sidebar actions, imported data, and the monotonically
#'   increasing `table_import_request_id` used to distinguish repeated imports.
#' @export
mod_sidebar_server <- function(
  id,
  current_table,
  root_inputs,
  unsaved_changes = NULL,
  private_key = NULL
) {
  moduleServer(id, function(input, output, session) {
    if (is.null(unsaved_changes)) unsaved_changes <- reactive(FALSE)

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

    output$unsaved_status <- renderUI({
      if (isTRUE(unsaved_changes())) {
        tags$span(class = "text-warning", "Unsaved changes")
      } else {
        tags$span(class = "text-success", "Saved")
      }
    })

    output$table_download_controls <- renderUI({
      if (is.null(private_key)) {
        return(downloadButton(session$ns("download_table"), "Download"))
      }

      tagList(
        downloadButton(session$ns("download_table"), "Download"),
        tags$br(), tags$br(),
        downloadButton(session$ns("download_signed_table"), "Download Signed"),
        tags$br(),
        tags$br()
      )
    })

    table_download_content <- function(file, signed = FALSE) {
      table <- current_table()
      validation <- validate_tracer_table(table)
      if (nrow(validation$errors) > 0L) {
        shiny::showNotification(
          paste(c("Cannot download an invalid network table:", validation$errors$message), collapse = "\n"),
          type = "error",
          duration = NULL
        )
        stop("Network table has validation errors.", call. = FALSE)
      }
      if (nrow(validation$warnings) > 0L) {
        shiny::showNotification(
          paste(c("Unresolved edge references:", validation$warnings$message), collapse = "\n"),
          type = "warning",
          duration = NULL
        )
      }

      json <- if (signed) {
        tracer_table_to_signed_json(table, private_key)
      } else {
        tracer_table_to_json(table)
      }
      writeBin(charToRaw(enc2utf8(json)), file)
    }

    output$download_table <- downloadHandler(
      filename = function() "data.json",
      content = function(file) table_download_content(file)
    )
    output$download_signed_table <- downloadHandler(
      filename = function() "data-signed.json",
      content = function(file) table_download_content(file, signed = TRUE)
    )

    imported_table_json <- DataTools::importServer(
      "table_import",
      title = "Import Network Table",
      importType = "list",
      defaultSource = "file",
      ckanFileTypes = "json",
      options = DataTools::importOptions(rPackageName = config()[["rPackageName"]])
    )
    table_import_request <- reactiveVal(0L)
    observeEvent(input[["table_import-accept"]], {
      session$onFlushed(function() {
        table_import_request(isolate(table_import_request()) + 1L)
      }, once = TRUE)
    }, ignoreInit = TRUE)
    table_import <- reactive({
      request_id <- table_import_request()
      req(request_id > 0L)
      imported <- isolate(imported_table_json())
      req(length(imported) > 0L)
      list(request_id = request_id, data = imported)
    })

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
      save_table = reactive(input$save_table),
      reset_table = reactive(input$reset_table),
      table_import = table_import,
      table_import_request_id = reactive(table_import_request()),
      uploaded_session = uploaded_session
    )
  })
}