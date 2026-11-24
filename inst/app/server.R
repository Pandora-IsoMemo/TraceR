library(TraceR)

shinyServer(function(input, output, session) {
  initial_network <- new_tracer_network()
  working_network <- reactiveVal(initial_network)
  saved_network <- reactiveVal(initial_network)
  path_map <- reactiveVal(tracer_network_path_map(initial_network))
  selected_path <- reactiveVal("")
  current_table <- reactiveVal(get_tracer_table(initial_network, ""))
  unsaved_changes <- reactive({
    !tracer_network_data_identical(working_network(), saved_network()) ||
      !tracer_table_data_identical(
        current_table(),
        get_tracer_table(working_network(), selected_path())
      )
  })
  pending_replacement <- reactiveVal(NULL)
  pending_navigation <- reactiveVal(NULL)
  pending_capsule_deletion <- reactiveVal(NULL)
  inputs_to_restore <- reactiveVal(NULL)

  # Adjust Path to environment
  if (!is.na(Sys.getenv("RUNNING_IN_DOCKER", unset = NA))) {
    path_private <- "/app/inst/app/private_key.pem"
    path_public <- "/app/inst/app/public_key.pem"
  } else {
    path_private <- system.file("app/private_key.pem", package = "TraceR")
    path_public <- system.file("app/public_key.pem", package = "TraceR")
  }

  private_key <- if (file.exists(path_private)) openssl::read_key(path_private) else NULL
  public_key <- if (file.exists(path_public)) openssl::read_pubkey(path_public) else NULL

  sidebar <- mod_sidebar_server(
    "sidebar",
    current_table = current_table,
    root_inputs = input,
    unsaved_changes = unsaved_changes,
    current_network = reactive(working_network()),
    selected_path = reactive(selected_path())
  )

  table_module <- mod_table_server(
    "table",
    current_table = current_table,
    private_key = private_key
  )

  network_from_root_table <- function(table) {
    network <- new_tracer_network()
    capsule_ids <- table$data$ID[
      !is.na(table$data$Type) & table$data$Type == "capsule"
    ]
    for (capsule_id in capsule_ids) {
      network <- add_tracer_capsule(network, "", capsule_id)
    }
    set_tracer_table(network, "", table)
  }

  show_messages <- function(title, messages, type) {
    shinyalert::shinyalert(
      title = title,
      text = paste(messages, collapse = "\n"),
      type = type
    )
  }

  save_network_table <- function(path = selected_path()) {
    if (identical(path, selected_path())) {
      working_network(set_tracer_table(working_network(), path, current_table()))
    }
    table <- get_tracer_table(working_network(), path)
    validation <- validate_tracer_table(table)
    if (nrow(validation$errors) > 0L) {
      show_messages("Cannot save invalid network table", validation$errors$message, "error")
      return(FALSE)
    }

    checkpoint <- tracer_network_save_table(
      working_network(),
      saved_network(),
      path_map(),
      path
    )
    saved_network(checkpoint$network)
    path_map(checkpoint$path_map)
    if (nrow(validation$warnings) > 0L) {
      show_messages(
        "Network table saved with unresolved references",
        c("Unresolved edge references:", validation$warnings$message),
        "warning"
      )
    } else {
      show_messages("Network table saved", "Saved for this session.", "success")
    }
    TRUE
  }

  reset_network_table <- function(path = selected_path()) {
    reset <- tracer_network_reset_table(
      working_network(),
      saved_network(),
      path_map(),
      path
    )
    working_network(reset$network)
    path_map(reset$path_map)
    if (identical(path, selected_path())) {
      current_table(get_tracer_table(reset$network, path))
    }
    invisible(TRUE)
  }

  select_path <- function(path) {
    selected_path(path)
    current_table(get_tracer_table(working_network(), path))
  }

  parent_capsule_path <- function(path) {
    parts <- strsplit(path, "/", fixed = TRUE)[[1]]
    if (length(parts) == 1L) "" else paste(parts[-length(parts)], collapse = "/")
  }

  network_zip_upload_path <- function(uploaded) {
    if (!is.list(uploaded) || is.null(uploaded$datapath) ||
        length(uploaded$datapath) == 0L || is.na(uploaded$datapath[[1L]])) {
      return(NULL)
    }
    uploaded$datapath[[1L]]
  }

  navigate_to <- function(path) {
    if (identical(path, selected_path())) return(invisible(NULL))
    if (!isTRUE(unsaved_changes())) {
      select_path(path)
      return(invisible(NULL))
    }

    pending_navigation(path)
    showModal(modalDialog(
      title = "Unsaved changes",
      tags$p("Save or discard changes at the current level before navigating."),
      tags$p(
        "Reset preserves working data in renamed descendant capsules, restores deleted saved subtrees, ",
        "and removes newly added subtrees."
      ),
      footer = tagList(
        actionButton("cancel_navigation", "Cancel"),
        actionButton("discard_navigation", "Discard and navigate", class = "btn-warning"),
        actionButton("save_navigation", "Save and navigate", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
    invisible(NULL)
  }

  apply_replacement <- function(replacement) {
    working_network(replacement$network)
    path_map(tracer_network_path_map(replacement$network, saved_network = NULL))
    selected_path("")
    current_table(get_tracer_table(replacement$network, ""))
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
    } else if (identical(replacement$source, "network_zip")) {
      messages <- c(messages, "Whole network imported from ZIP.")
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
    inputs = NULL,
    network = NULL
  ) {
    replacement_network <- if (is.null(network)) {
      tryCatch(network_from_root_table(table), error = function(error) error)
    } else {
      network
    }
    if (inherits(replacement_network, "error")) {
      show_messages("Could not install network table", conditionMessage(replacement_network), "error")
      return(invisible(NULL))
    }
    validation <- validate_tracer_network(replacement_network)
    if (nrow(validation$errors) > 0L) {
      show_messages("Could not install network", validation$errors$message, "error")
      return(invisible(NULL))
    }
    replacement <- list(
      table = replacement_network$root,
      network = replacement_network,
      warnings = unique(c(warnings, validation$warnings$message)),
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
      title = "Replace unsaved network?",
      tags$p("This will discard network changes that have not been saved."),
      if (length(notices) > 0L) tags$ul(lapply(notices, tags$li)),
      if (length(warnings) > 0L) tags$ul(lapply(warnings, tags$li)),
      footer = tagList(
        actionButton("cancel_table_replacement", "Cancel"),
        actionButton("confirm_table_replacement", "Replace network", class = "btn-warning")
      ),
      easyClose = FALSE
    ))
    invisible(NULL)
  }

  observeEvent(current_table(), {
    table <- current_table()
    existing_capsules <- list_tracer_capsules(working_network(), selected_path())
    proposed_capsules <- table$data$ID[
      !is.na(table$data$Type) & table$data$Type == "capsule"
    ]
    if (!setequal(existing_capsules, proposed_capsules) ||
        length(existing_capsules) != length(proposed_capsules)) {
      return()
    }
    working_network(set_tracer_table(working_network(), selected_path(), table))
  }, ignoreInit = TRUE)

  observeEvent(sidebar$import_network(), {
    archive_path <- network_zip_upload_path(sidebar$network_zip_file())
    if (is.null(archive_path)) {
      show_messages("Network import failed", "Choose a ZIP archive first.", "warning")
      return()
    }
    tryCatch({
      network <- tracer_network_from_zip(archive_path)
      validation <- validate_tracer_network(network)
      request_replacement(
        network$root,
        warnings = validation$warnings$message,
        source = "network_zip",
        network = network
      )
    }, error = function(error) {
      show_messages("Network import failed", conditionMessage(error), "error")
    })
  }, ignoreInit = TRUE)

  observeEvent(sidebar$import_network_as_capsule(), {
    archive_path <- network_zip_upload_path(sidebar$network_zip_file())
    if (is.null(archive_path)) {
      show_messages("Capsule import failed", "Choose a ZIP archive first.", "warning")
      return()
    }
    tryCatch({
      imported_network <- tracer_network_from_zip(archive_path)
      parent_path <- selected_path()
      capsule_id <- sidebar$import_capsule_id()
      child_path <- if (identical(parent_path, "")) {
        capsule_id
      } else {
        paste(parent_path, capsule_id, sep = "/")
      }
      updated <- import_tracer_network_as_capsule(
        working_network(),
        parent_path,
        capsule_id,
        imported_network,
        parent_table = current_table()
      )
      path_map(tracer_network_register_paths(path_map(), updated, child_path))
      working_network(updated)
      current_table(get_tracer_table(updated, parent_path))
      validation <- validate_tracer_network(imported_network)
      if (nrow(validation$warnings) > 0L) {
        show_messages(
          "Network imported as capsule",
          c("Unresolved edge references:", validation$warnings$message),
          "warning"
        )
      } else {
        show_messages("Network imported as capsule", "The imported hierarchy is now a child capsule.", "success")
      }
    }, error = function(error) {
      show_messages("Capsule import failed", conditionMessage(error), "error")
    })
  }, ignoreInit = TRUE)

  observeEvent(sidebar$capsule_selection(), {
    path <- sidebar$capsule_selection()
    if (is.null(path)) return()
    navigate_to(path)
  }, ignoreInit = TRUE)

  observeEvent(sidebar$save_table(), {
    save_network_table()
  }, ignoreInit = TRUE)

  observeEvent(sidebar$reset_table(), {
    if (!isTRUE(unsaved_changes())) return()
    showModal(modalDialog(
      title = "Discard unsaved changes?",
      tags$p("The selected table will be restored from its last saved checkpoint."),
      tags$p(
        "Working data in renamed descendants is preserved and their paths are restored. ",
        "Deleted saved subtrees are restored. Newly added subtrees and their unsaved data are removed."
      ),
      footer = tagList(
        actionButton("cancel_table_reset", "Cancel"),
        actionButton("confirm_table_reset", "Reset", class = "btn-warning")
      ),
      easyClose = FALSE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_table_reset, {
    removeModal()
    tryCatch(
      reset_network_table(),
      error = function(error) show_messages("Could not reset network table", conditionMessage(error), "error")
    )
    table_module$scroll_to_top()
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_table_reset, removeModal(), ignoreInit = TRUE)

  observeEvent(input$save_navigation, {
    path <- pending_navigation()
    removeModal()
    pending_navigation(NULL)
    if (is.null(path) || !isTRUE(save_network_table())) return()
    if (nzchar(path) && !path %in% tracer_network_paths(working_network())) {
      show_messages("Navigation cancelled", "The target capsule no longer exists.", "warning")
      return()
    }
    select_path(path)
  }, ignoreInit = TRUE)

  observeEvent(input$discard_navigation, {
    path <- pending_navigation()
    removeModal()
    pending_navigation(NULL)
    reset_succeeded <- tryCatch({
      reset_network_table()
      TRUE
    }, error = function(error) {
      show_messages("Could not discard changes", conditionMessage(error), "error")
      FALSE
    })
    if (!reset_succeeded) return()
    if (!is.null(path) &&
        (identical(path, "") || path %in% tracer_network_paths(working_network()))) {
      select_path(path)
    } else {
      show_messages("Navigation cancelled", "The target capsule was removed by Reset.", "warning")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_navigation, {
    pending_navigation(NULL)
    removeModal()
  }, ignoreInit = TRUE)

  observeEvent(sidebar$add_capsule(), {
    id <- sidebar$new_capsule_id()
    parent_path <- selected_path()
    tryCatch({
      updated <- add_tracer_capsule(
        working_network(),
        parent_path,
        id,
        parent_table = current_table()
      )
      child_path <- if (identical(parent_path, "")) id else paste(parent_path, id, sep = "/")
      path_map(tracer_network_register_paths(path_map(), updated, child_path))
      working_network(updated)
      current_table(get_tracer_table(updated, parent_path))
    }, error = function(error) {
      show_messages("Could not add capsule", conditionMessage(error), "error")
    })
  }, ignoreInit = TRUE)

  observeEvent(sidebar$rename_capsule(), {
    path <- selected_path()
    if (identical(path, "")) {
      show_messages("Select a capsule", "The root table cannot be renamed.", "warning")
      return()
    }
    new_id <- sidebar$rename_capsule_id()
    parent_path <- parent_capsule_path(path)
    new_path <- if (identical(parent_path, "")) new_id else paste(parent_path, new_id, sep = "/")
    tryCatch({
      updated <- rename_tracer_capsule(working_network(), path, new_id)
      path_map(tracer_network_rebase_path_map(path_map(), path, new_path))
      working_network(updated)
      select_path(new_path)
    }, error = function(error) {
      show_messages("Could not rename capsule", conditionMessage(error), "error")
    })
  }, ignoreInit = TRUE)

  delete_capsule_now <- function(path) {
    parent_path <- parent_capsule_path(path)
    updated <- remove_tracer_capsule(working_network(), path)
    path_map(tracer_network_drop_paths(path_map(), path))
    working_network(updated)
    select_path(parent_path)
  }

  observeEvent(sidebar$delete_capsule(), {
    path <- selected_path()
    if (identical(path, "")) {
      show_messages("Select a capsule", "The root table cannot be deleted.", "warning")
      return()
    }
    subtree_paths <- tracer_network_paths(working_network())
    subtree_paths <- subtree_paths[
      subtree_paths == path | startsWith(subtree_paths, paste0(path, "/"))
    ]
    has_content <- any(vapply(subtree_paths, function(subtree_path) {
      nrow(get_tracer_table(working_network(), subtree_path)$data) > 0L
    }, logical(1)))
    checkpoint_path <- path_map()$saved_path[match(path, path_map()$working_path)]
    if (length(checkpoint_path) == 1L && !is.na(checkpoint_path) &&
        checkpoint_path %in% tracer_network_paths(saved_network())) {
      saved_paths <- tracer_network_paths(saved_network())
      saved_paths <- saved_paths[
        saved_paths == checkpoint_path |
          startsWith(saved_paths, paste0(checkpoint_path, "/"))
      ]
      has_content <- has_content || any(vapply(saved_paths, function(saved_path) {
        nrow(get_tracer_table(saved_network(), saved_path)$data) > 0L
      }, logical(1)))
    }
    if (!has_content) {
      tryCatch(delete_capsule_now(path), error = function(error) {
        show_messages("Could not delete capsule", conditionMessage(error), "error")
      })
      return()
    }

    pending_capsule_deletion(path)
    showModal(modalDialog(
      title = "Delete capsule subtree?",
      tags$p("This removes the capsule and all nested tables."),
      tags$p(
        "Unsaved descendant edits will be discarded. Resetting the parent before saving ",
        "restores the saved subtree."
      ),
      footer = tagList(
        actionButton("cancel_capsule_deletion", "Cancel"),
        actionButton("confirm_capsule_deletion", "Delete subtree", class = "btn-warning")
      ),
      easyClose = FALSE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_capsule_deletion, {
    path <- pending_capsule_deletion()
    pending_capsule_deletion(NULL)
    removeModal()
    if (!is.null(path)) {
      tryCatch(delete_capsule_now(path), error = function(error) {
        show_messages("Could not delete capsule", conditionMessage(error), "error")
      })
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_capsule_deletion, {
    pending_capsule_deletion(NULL)
    removeModal()
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_table_replacement, {
    replacement <- pending_replacement()
    removeModal()
    if (!is.null(replacement)) apply_replacement(replacement)
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_table_replacement, {
    pending_replacement(NULL)
    removeModal()
  }, ignoreInit = TRUE)

  observeEvent(table_module$table_import(), {
    import_request <- table_module$table_import()
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
    network <- createExampleTracerNetwork()
    request_replacement(network$root, source = "example", network = network)
  }, ignoreInit = TRUE)

  observeEvent(sidebar$empty_network(), {
    request_replacement(new_tracer_table(), source = "empty")
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
