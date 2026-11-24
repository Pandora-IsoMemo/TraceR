#' Table module UI
#'
#' Displays the eleven-column `tracer_table` with editable cells, row controls,
#' optional-column renaming, and validation feedback. All rows share a fixed-
#' height scrolling viewport without pagination. New tables start at the top,
#' while adding a row clears sorting and filters, then scrolls to the appended
#' row. Required column names are fixed; optional names are stored only in the
#' data frame.
#'
#' @param id Module ID
#' @return Editable table UI
#' @export
mod_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    fluidRow(
      column(2, actionButton(ns("add_row"), "Add row")),
      column(2, actionButton(ns("delete_row"), "Delete selected row")),
      column(4, uiOutput(ns("optional_column_selector"))),
      column(
        4,
        textInput(ns("optional_column_name"), "New optional column name"),
        actionButton(ns("rename_optional_column"), "Rename column")
      )
    ),
    tags$hr(),
    uiOutput(ns("feedback")),
    uiOutput(ns("validation")),
    DT::DTOutput(ns("table")),
    tags$hr(),
    tags$h4("Table JSON"),
    fluidRow(
      column(6, uiOutput(ns("table_download_controls"))),
      column(6, DataTools::importUI(ns("table_import"), label = "Import"))
    ),
    tags$style(htmltools::HTML("#table-table .dataTables_scrollBody tbody td { height: 1.5em; }")),
    tags$script(htmltools::HTML("
      Shiny.addCustomMessageHandler('traceRScrollTable', function(message) {
        function scrollTable() {
          var output = document.getElementById(message.id);
          var body = output && output.querySelector('.dataTables_scrollBody');
          if (body) {
            body.scrollTop = message.action === 'bottom' ? body.scrollHeight : 0;
          }
        }
        [0, 100, 300].forEach(function(delay) {
          setTimeout(scrollTable, delay);
        });
      });

      Shiny.addCustomMessageHandler('traceRAppendTableRow', function(message) {
        var output = document.getElementById(message.id);
        var body = output && output.querySelector('.dataTables_scrollBody');
        var tableElement = body && body.querySelector('table');
        if (!tableElement || !$.fn.DataTable.isDataTable(tableElement)) return;

        var table = $(tableElement).DataTable();
        var scrollAfterAppend = function() {
          if (table.rows().count() < message.expectedRows) return;
          $(tableElement).off('draw.dt.traceRAppend', scrollAfterAppend);
          body.scrollTop = body.scrollHeight;
        };

        $(tableElement).on('draw.dt.traceRAppend', scrollAfterAppend);
        table.order([]);
      });
    "))
  )
}

#' Table module server
#'
#' Cell edits and row operations update the supplied shared table reactive.
#' Invalid intermediate content is retained and reported for the Network
#' module to handle. The module also provides network-table JSON download
#' (signed when a private key is supplied) and import.
#'
#' @param id Module ID
#' @param current_table Reactive value containing the shared `tracer_table`
#' @param private_key Optional RSA private key for signed table downloads
#' @return A list with `scroll_to_top()`, the `table_import` reactive, and the
#'   monotonically increasing `table_import_request_id` used to distinguish
#'   repeated imports.
#' @export
mod_table_server <- function(id, current_table, private_key = NULL) {
  moduleServer(id, function(input, output, session) {
    feedback <- reactiveVal(NULL)
    column_names <- reactiveVal(isolate(names(current_table()$data)))
    proxy <- DT::dataTableProxy("table", session = session)

    request_scroll <- function(action) {
      session$onFlushed(function() {
        session$sendCustomMessage(
          "traceRScrollTable",
          list(id = session$ns("table"), action = action)
        )
      }, once = TRUE)
    }

    prepare_append <- function(expected_rows) {
      session$sendCustomMessage(
        "traceRAppendTableRow",
        list(id = session$ns("table"), expectedRows = expected_rows)
      )
      DT::clearSearch(proxy)
    }

    output$table <- DT::renderDT({
      column_names()
      data <- isolate(current_table()$data)
      DT::datatable(
        data,
        class = "display nowrap",
        rownames = FALSE,
        filter = "top",
        selection = "single",
        editable = list(target = "cell"),
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "400px",
          scrollCollapse = FALSE,
          lengthChange = FALSE
        )
      )
    })

    observeEvent(current_table(), {
      data <- current_table()$data
      if (!identical(names(data), column_names())) {
        column_names(names(data))
        return()
      }

      DT::replaceData(
        proxy,
        data,
        rownames = FALSE,
        resetPaging = FALSE,
        clearSelection = "none"
      )
    }, ignoreInit = TRUE)

    output$optional_column_selector <- renderUI({
      optional_columns <- setdiff(
        column_names(),
        c("ID", "Label", "Type", "From", "To", "Unique identifier")
      )
      selectInput(
        session$ns("optional_column"),
        "Optional column",
        choices = optional_columns
      )
    })

    output$feedback <- renderUI({
      message <- feedback()
      if (is.null(message)) return(NULL)
      div(class = paste("alert", paste0("alert-", message$type)), message$text)
    })

    output$validation <- renderUI({
      result <- validate_tracer_table(current_table())
      issues <- c(result$errors$message, result$warnings$message)
      if (length(issues) == 0L) return(NULL)

      tags$div(
        class = if (nrow(result$errors) > 0L) "alert alert-danger" else "alert alert-warning",
        role = "alert",
        tags$strong(if (nrow(result$errors) > 0L) "Table has validation errors" else "Table has warnings"),
        tags$ul(lapply(issues, tags$li))
      )
    })

    observeEvent(input$table_cell_edit, {
      edit <- input$table_cell_edit
      current <- current_table()
      row <- edit$row
      column <- edit$col + 1L
      field <- names(current$data)[column]

      if (row >= 1L && row <= nrow(current$data) &&
          identical(field, "ID") && identical(current$data$Type[row], "capsule")) {
        feedback(list(type = "warning", text = "Rename capsules using the capsule controls."))
        return()
      }
      if (row >= 1L && row <= nrow(current$data) && identical(field, "Type")) {
        old_type <- current$data$Type[row]
        if (identical(old_type, "capsule") || identical(edit$value, "capsule")) {
          feedback(list(type = "warning", text = "Create or remove capsules using the capsule controls."))
          return()
        }
      }

      data <- DT::editData(current$data, edit, rownames = FALSE)
      current_table(new_tracer_table(data))
    })

    observeEvent(input$add_row, {
      data <- current_table()$data
      blank_row <- as.data.frame(
        stats::setNames(rep(list(""), ncol(data)), names(data)),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      new_data <- rbind(data, blank_row)
      prepare_append(nrow(new_data))
      current_table(new_tracer_table(new_data))
    })

    observeEvent(input$delete_row, {
      selected <- input$table_rows_selected
      if (length(selected) != 1L || selected < 1L || selected > nrow(current_table()$data)) {
        return()
      }

      if (identical(current_table()$data$Type[selected], "capsule")) {
        feedback(list(type = "warning", text = "Delete capsules using the capsule controls."))
        return()
      }

      data <- current_table()$data[-selected, , drop = FALSE]
      current_table(new_tracer_table(data))
    })

    observeEvent(input$rename_optional_column, {
      data <- current_table()$data
      optional_columns <- setdiff(
        names(data),
        c("ID", "Label", "Type", "From", "To", "Unique identifier")
      )
      new_name <- input$optional_column_name

      if (is.null(input$optional_column) || !input$optional_column %in% optional_columns) {
        feedback(list(type = "warning", text = "Select an optional column to rename."))
        return()
      }
      if (is.null(new_name) || !nzchar(trimws(new_name))) {
        feedback(list(type = "warning", text = "Column names cannot be empty."))
        return()
      }
      if (new_name %in% setdiff(names(data), input$optional_column)) {
        feedback(list(type = "warning", text = "Column names must be unique."))
        return()
      }

      names(data)[match(input$optional_column, names(data))] <- new_name
      current_table(new_tracer_table(data))
      feedback(NULL)
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


    list(
      scroll_to_top = function() request_scroll("top"),
      table_import = table_import,
      table_import_request_id = reactive(table_import_request())
    )
  })
}