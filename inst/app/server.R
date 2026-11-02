library(TraceR)

shinyServer(function(input, output, session) {
  current_table <- reactiveVal(new_tracer_table())
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
    root_inputs = input
  )

  table_module <- mod_table_server("table", current_table = current_table)

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
    private_key = private_key,
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

    current_table(restored_table)
  table_module$scroll_to_top()
    session_inputs <- session_bundle[["inputs"]]
    inputs_to_restore(if (is.null(session_inputs)) list() else session_inputs)
  })
})
