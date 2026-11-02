#' Table module UI
#'
#' Displays the eleven-column `tracer_table` with editable cells, row controls,
#' optional-column renaming, and validation feedback. All rows share a fixed-
#' height scrolling viewport without pagination. New tables start at the top,
#' while adding a row scrolls to the bottom. Required column names are fixed;
#' optional names are stored only in the data frame.
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
    "))
  )
}

#' Table module server
#'
#' Cell edits and row operations update the supplied shared table reactive.
#' Invalid intermediate content is retained and reported for the Network
#' module to handle.
#'
#' @param id Module ID
#' @param current_table Reactive value containing the shared `tracer_table`
#' @return `NULL`, invisibly
#' @export
mod_table_server <- function(id, current_table) {
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
        names(current_table()$data),
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
      data <- DT::editData(current_table()$data, edit, rownames = FALSE)
      current_table(new_tracer_table(data))
    })

    observeEvent(input$add_row, {
      data <- current_table()$data
      blank_row <- as.data.frame(
        stats::setNames(rep(list(""), ncol(data)), names(data)),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      current_table(new_tracer_table(rbind(data, blank_row)))
      request_scroll("bottom")
    })

    observeEvent(input$delete_row, {
      selected <- input$table_rows_selected
      if (length(selected) != 1L || selected < 1L || selected > nrow(current_table()$data)) {
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

    list(scroll_to_top = function() request_scroll("top"))
  })
}