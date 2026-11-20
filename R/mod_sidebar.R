#' Shared sidebar UI
#'
#' Displays network-table actions, capsule navigation, persistence controls,
#' and session controls.
#' Session downloads store the shared `tracer_table` as data and capture the
#' root Shiny inputs. Session imports are returned to the application server,
#' which validates and installs the table before restoring those inputs. The
#' Save checkpoint is session-local; Reset restores the last checkpoint after
#' confirmation. Network-table JSON and whole-network ZIP import/export are
#' separate from DataTools session import/export.
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
    tags$hr(),
    tags$h3("Capsules"),
    textInput(ns("new_capsule_id"), "New capsule ID"),
    actionButton(ns("add_capsule"), "Add capsule"),
    tags$br(), tags$br(),
    textInput(ns("rename_capsule_id"), "Rename selected capsule to"),
    actionButton(ns("rename_capsule"), "Rename selected capsule"),
    tags$br(), tags$br(),
    actionButton(ns("delete_capsule"), "Delete selected capsule", class = "btn-warning"),
    tags$hr(),
    textOutput(ns("selected_path")),
    tags$h3("Network Tree"),
    shinyTree::shinyTree(
      ns("capsule_tree"),
      search = FALSE,
      theme = "proton",
      multiple = FALSE
    ),
    tags$hr(),
    tags$h4("Network Tree ZIP"),
    downloadButton(ns("download_network"), "Download Network"),
    fileInput(ns("network_zip_file"), "Network ZIP file", accept = ".zip"),
    actionButton(ns("import_network"), "Import Network ZIP"),
    textInput(ns("import_capsule_id"), "Import as capsule ID"),
    actionButton(ns("import_network_as_capsule"), "Import ZIP as capsule"),
    tags$hr(),
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
#' The sidebar does not own network data. It renders the supplied hierarchy
#' and returns selected paths, table actions, and session actions for the
#' application server to coordinate.
#'
#' @param id Module ID
#' @param current_table Reactive value containing the shared `tracer_table`
#' @param root_inputs Root Shiny input object for session persistence
#' @param unsaved_changes Reactive logical indicating whether the table is dirty
#' @param private_key Optional RSA private key for signed table downloads
#' @param current_network Reactive value containing the `tracer_network`
#' @param selected_path Reactive value containing the selected capsule path
#' @return A list of sidebar actions for capsule management, table and network
#'   import/export, and session persistence. It includes the monotonically
#'   increasing `table_import_request_id` used to distinguish repeated imports.
#' @export
mod_sidebar_server <- function(
  id,
  current_table,
  root_inputs,
  unsaved_changes = NULL,
  private_key = NULL,
  current_network = NULL,
  selected_path = NULL
) {
  moduleServer(id, function(input, output, session) {
    if (is.null(unsaved_changes)) unsaved_changes <- reactive(FALSE)
    if (is.null(current_network)) current_network <- reactive(new_tracer_network())
    if (is.null(selected_path)) selected_path <- reactive("")
    tree_selection <- reactiveVal(NULL)
    tree_refresh <- reactiveVal(0L)

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

    build_capsule_tree <- function(path) {
      capsule_ids <- list_tracer_capsules(current_network(), path)
      children <- lapply(capsule_ids, function(capsule_id) {
        child_path <- if (identical(path, "")) {
          capsule_id
        } else {
          paste(path, capsule_id, sep = "/")
        }
        selected <- identical(child_path, selected_path())
        opened <- selected ||
          startsWith(selected_path(), paste0(child_path, "/"))
        descendants <- build_capsule_tree(child_path)
        structure(
          descendants,
          sticon = "folder",
          stselected = selected,
          stopened = opened
        )
      })
      names(children) <- capsule_ids
      children
    }

    output$capsule_tree <- shinyTree::renderTree({
      current_network()
      tree_refresh()
      path <- selected_path()
      root <- structure(
        build_capsule_tree(""),
        sticon = "home",
        stselected = identical(path, ""),
        stopened = TRUE
      )
      list(Root = root)
    })

    output$selected_path <- renderText({
      path <- selected_path()
      parts <- if (identical(path, "")) character() else strsplit(path, "/", fixed = TRUE)[[1]]
      paste(c("Root", parts), collapse = " / ")
    })

    selected_slice_to_parts <- function(x) {
      if (is.null(x) || length(x) == 0L) return(character())
      if (is.character(x) && is.null(names(x))) return(x)
      if (is.list(x)) {
        node_names <- setdiff(names(x), "_names")
        if (length(node_names) == 1L) {
          node_name <- node_names[[1L]]
          return(c(node_name, selected_slice_to_parts(x[[node_name]])))
        }
      }
      character()
    }

    observeEvent(input$capsule_tree, {
      selected_slices <- shinyTree::get_selected(input$capsule_tree, format = "slices")
      if (length(selected_slices) != 1L) return()

      parts <- selected_slice_to_parts(selected_slices[[1L]])
      if (length(parts) == 0L || parts[[1L]] != "Root") return()
      path <- paste(parts[-1L], collapse = "/")
      if (!.tracer_network_valid_path(path) ||
          (nzchar(path) && !path %in% names(current_network()$tables))) {
        return()
      }
      tree_selection(path)
      tree_refresh(isolate(tree_refresh()) + 1L)
    })

    output$table_download_controls <- renderUI({
      if (is.null(private_key)) {
        return(downloadButton(session$ns("download_table"), "Download"))
      }

      tagList(
        downloadButton(session$ns("download_table"), "Download JSON"),
        tags$br(), tags$br(),
        downloadButton(session$ns("download_signed_table"), "Download Signed JSON"),
        tags$div(
          class = "alert alert-warning",
          role = "alert",
          style = "margin-top: 10px; padding: 10px; font-size: 13px;",
          tags$strong("Experimental: testing only"),
          tags$br(),
          "Future versions may not verify current signed files. Keep an unsigned JSON copy."
        ),
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

    output$download_network <- downloadHandler(
      filename = function() "network.zip",
      content = function(file) {
        validation <- validate_tracer_network(current_network())
        if (nrow(validation$errors) > 0L) {
          shiny::showNotification(
            paste(c("Cannot download an invalid network:", validation$errors$message), collapse = "\n"),
            type = "error",
            duration = NULL
          )
          stop("Network has validation errors.", call. = FALSE)
        }
        if (nrow(validation$warnings) > 0L) {
          shiny::showNotification(
            paste(c("Unresolved edge references:", validation$warnings$message), collapse = "\n"),
            type = "warning",
            duration = NULL
          )
        }
        tracer_network_to_zip(current_network(), file)
      }
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
      add_capsule = reactive(input$add_capsule),
      new_capsule_id = reactive(input$new_capsule_id),
      rename_capsule = reactive(input$rename_capsule),
      rename_capsule_id = reactive(input$rename_capsule_id),
      delete_capsule = reactive(input$delete_capsule),
      network_zip_file = reactive(input$network_zip_file),
      import_network = reactive(input$import_network),
      import_capsule_id = reactive(input$import_capsule_id),
      import_network_as_capsule = reactive(input$import_network_as_capsule),
      table_import = table_import,
      table_import_request_id = reactive(table_import_request()),
      uploaded_session = uploaded_session,
      capsule_selection = reactive(tree_selection())
    )
  })
}