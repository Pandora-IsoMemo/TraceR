library(TraceR)

shinyServer(function(input, output, session) {
  current_table <- reactiveVal(new_tracer_table())
  saved_table <- reactiveVal(new_tracer_table())
  unsaved_changes <- reactive({
    !tracer_table_data_identical(current_table(), saved_table())
  })
  pending_replacement <- reactiveVal(NULL)
  inputs_to_restore <- reactiveVal(NULL)

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

  sidebar <- mod_sidebar_server(
    "sidebar",
    current_table = current_table,
    root_inputs = input,
    unsaved_changes = unsaved_changes,
    private_key = private_key
  )

  table_module <- mod_table_server("table", current_table = current_table)

  show_messages <- function(title, messages, type) {
    shinyalert::shinyalert(
      title = title,
      text = paste(messages, collapse = "\n"),
      type = type
    )
  }

  apply_replacement <- function(replacement) {
    current_table(new_tracer_table(replacement$table$data))
    table_module$scroll_to_top()
    if (!is.null(replacement$inputs)) {
      inputs_to_restore(replacement$inputs)
    }

    messages <- replacement$notices
    alert_type <- if (length(messages) > 0L) "warning" else "success"
    if (identical(replacement$source, "signed_json")) {
      messages <- c(messages, "Signed network table imported and RSA-SHA256 signature verified.")
    } else if (identical(replacement$source, "unsigned_json")) {
      messages <- c(messages, "Unsigned network table imported. Its authenticity was not verified.")
      alert_type <- "info"
    }
    if (length(replacement$warnings) > 0L) {
      messages <- c(messages, "Unresolved edge references:", replacement$warnings)
      alert_type <- "warning"
    }
    if (length(messages) > 0L) {
      show_messages("Import complete", messages, alert_type)
    }
    pending_replacement(NULL)
  }

  request_replacement <- function(
    table,
    warnings = character(),
    notices = character(),
    source,
    inputs = NULL
  ) {
    replacement <- list(
      table = table,
      warnings = warnings,
      notices = notices,
      source = source,
      inputs = inputs
    )
    if (!isTRUE(unsaved_changes())) {
      apply_replacement(replacement)
      return(invisible(NULL))
    }

    pending_replacement(replacement)
    showModal(modalDialog(
      title = "Replace unsaved network table?",
      tags$p("This will discard changes that have not been saved."),
      if (length(notices) > 0L) tags$ul(lapply(notices, tags$li)),
      if (length(warnings) > 0L) tags$ul(lapply(warnings, tags$li)),
      footer = tagList(
        actionButton("cancel_table_replacement", "Cancel"),
        actionButton("confirm_table_replacement", "Replace table", class = "btn-warning")
      ),
      easyClose = FALSE
    ))
    invisible(NULL)
  }

  observeEvent(sidebar$save_table(), {
    table <- current_table()
    validation <- validate_tracer_table(table)
    if (nrow(validation$errors) > 0L) {
      show_messages("Cannot save invalid network table", validation$errors$message, "error")
      return()
    }

    saved_table(new_tracer_table(table$data))
    if (nrow(validation$warnings) > 0L) {
      show_messages(
        "Network table saved with unresolved references",
        c("Unresolved edge references:", validation$warnings$message),
        "warning"
      )
    } else {
      show_messages("Network table saved", "Saved for this session.", "success")
    }
  }, ignoreInit = TRUE)

  observeEvent(sidebar$reset_table(), {
    if (!isTRUE(unsaved_changes())) return()
    showModal(modalDialog(
      title = "Discard unsaved changes?",
      "The current network table will be restored from the last saved checkpoint.",
      footer = tagList(
        actionButton("cancel_table_reset", "Cancel"),
        actionButton("confirm_table_reset", "Reset", class = "btn-warning")
      ),
      easyClose = FALSE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_table_reset, {
    removeModal()
    current_table(new_tracer_table(saved_table()$data))
    table_module$scroll_to_top()
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_table_reset, removeModal(), ignoreInit = TRUE)

  observeEvent(input$confirm_table_replacement, {
    replacement <- pending_replacement()
    removeModal()
    if (!is.null(replacement)) apply_replacement(replacement)
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_table_replacement, {
    pending_replacement(NULL)
    removeModal()
  }, ignoreInit = TRUE)

  observeEvent(sidebar$table_import(), {
    import_request <- sidebar$table_import()
    uploaded <- import_request$data
    req(length(uploaded) > 0L)
    imported <- uploaded[[1L]]

    tryCatch({
      if (is.list(imported) && any(c("nodes_df", "edges_df") %in% names(imported))) {
        stop("Legacy graph JSON is not supported. Import a tracer_table JSON file.", call. = FALSE)
      }
      if (is.list(imported) && !identical(imported$format, "tracer_table_signed") &&
          any(c("signature", "algorithm") %in% names(imported))) {
        stop("Signed table JSON must use the tracer_table_signed envelope.", call. = FALSE)
      }

      signed <- is.list(imported) && identical(imported$format, "tracer_table_signed")
      table <- if (signed) {
        tracer_table_from_signed_json(imported, public_key)
      } else {
        tracer_table_from_json(imported)
      }
      validation <- validate_tracer_table(table)
      if (nrow(validation$errors) > 0L) {
        stop(paste(validation$errors$message, collapse = "\n"), call. = FALSE)
      }

      request_replacement(
        table,
        warnings = validation$warnings$message,
        source = if (signed) "signed_json" else "unsigned_json"
      )
    }, error = function(error) {
      show_messages("Network table import failed", conditionMessage(error), "error")
    })
  })

  observeEvent(sidebar$generate_example(), {
    current_table(createExampleTracerTable())
    table_module$scroll_to_top()
  }, ignoreInit = TRUE)

  observeEvent(sidebar$empty_network(), {
    current_table(new_tracer_table())
    table_module$scroll_to_top()
  }, ignoreInit = TRUE)

  mod_network_server(
    "network",
    current_table = current_table
  )
  updateInput(
    input,
    output,
    session,
    uploaded_inputs = reactive(inputs_to_restore())
  )

  observeEvent(sidebar$uploaded_session(), {
    uploaded <- sidebar$uploaded_session()
    req(length(uploaded) > 0L)
    session_bundle <- uploaded[[1L]]
    session_data <- session_bundle[["data"]]
    imported_table <- if (is.list(session_data)) session_data[["tracer_table"]] else NULL

    if (is.null(imported_table)) {
      shinyalert::shinyalert(
        title = "Unsupported session",
        text = "This session does not contain a TraceR table. Older graph-only sessions cannot be restored.",
        type = "warning"
      )
      return()
    }

    restored_table <- tryCatch({
      if (!inherits(imported_table, "tracer_table") || !is.data.frame(imported_table$data)) {
        stop("The session's tracer_table object is malformed.")
      }
      new_tracer_table(imported_table$data)
    }, error = function(error) error)

    if (inherits(restored_table, "error")) {
      shinyalert::shinyalert(
        title = "Invalid session table",
        text = restored_table$message,
        type = "error"
      )
      return()
    }

    session_inputs <- session_bundle[["inputs"]]
    validation <- validate_tracer_table(restored_table)
    request_replacement(
      restored_table,
      warnings = validation$warnings$message,
      notices = validation$errors$message,
      source = "session",
      inputs = if (is.null(session_inputs)) list() else session_inputs
    )
  })
})
