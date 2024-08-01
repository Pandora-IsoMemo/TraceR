#' Import JSON Module UI
#'
#' @param id Module ID
#' @return Shiny UI elements
#'
#' @export
importModuleUI <- function(id) {
  ns <- NS(id)

  tagList(
    importDataUI(ns("file_import"), label = "Import JSON"),
    tags$br(), tags$br(),
    importDataUI(ns("session_import"), label = "Import Session")
  )
}

#' Import JSON Module Server
#'
#' @param id Module ID
#' @param public_key public_key
#' @return uploaded data
#'
#' @export
importModuleServer <- function(id, public_key) {
  moduleServer(
    id,
    function(input, output, session) {
      data <- reactiveValues(graph = NULL,
                             input = NULL)

      # import data from JSON file from Pandora, url, file
      uploaded_file <- importDataServer(
        "file_import",
        importType = "list",
        defaultSource = config()[["defaultSource"]][["file"]],
        ckanFileTypes = config()[["ckanFileTypes"]][["file"]],
        fileExtension = config()[["fileExtension"]][["file"]]
      )

      observe({
        req(length(uploaded_file()) > 0)
        logDebug("importModuleServer: Update graph")

        # update data
        req(!is.null(uploaded_file()[[1]]))
        data$graph <- uploaded_file()[[1]]

        check_signature_validity(graph_list = data$graph, public_key)
      }) %>%
        bindEvent(uploaded_file())

      # load session
      uploaded_session <- importDataServer(
        "session_import",
        title = "Session import",
        importType = "model",
        defaultSource = config()[["defaultSource"]][["session"]],
        ckanFileTypes = config()[["ckanFileTypes"]][["session"]],
        fileExtension = config()[["fileExtension"]][["session"]],
        onlySettings = TRUE,
        options = importOptions(rPackageName = config()[["rPackageName"]])
      )

      observe({
        req(length(uploaded_session()) > 0)
        logDebug("importModuleServer: Update graph and inputs")

        # Update data
        req(!is.null(uploaded_session()[[1]][["data"]]))
        data$graph <- uploaded_session()[[1]][["data"]]

        check_signature_validity(graph_list = data$graph, public_key)

        # Update input if data is available
        req(!is.null(uploaded_session()[[1]][["inputs"]]))
        data$input <- uploaded_session()[[1]][["inputs"]]
      }) %>%
        bindEvent(uploaded_session())

      return(data)
    }
  )
}

#' Check validity of signature
#'
#' @param graph_list list containing graph elements
#' @param public_key public key
#'
#' @export
check_signature_validity <- function(graph_list, public_key){
  if (is.na(Sys.getenv("SHINYPROXY_USERID", unset = NA))){
    message <- "Validating signatures of uploaded files is not possible for apps running locally."
    shinyalert(
      title = "Warning",
      text = message,
      type = "warning"
    )
  } else {
  # App is running on shinyproxy
  if ("signature" %in% names(graph_list)) {
    # Check validity of signature
    received_json_data <- toJSON(graph_list[setdiff(names(graph_list), "signature")], pretty = TRUE, auto_unbox = TRUE)
    received_signature <- base64_decode(graph_list$signature)
    # Verify the signature
    is_valid <- tryCatch({
      signature_verify(data = charToRaw(received_json_data), sig = received_signature, pubkey = public_key)
    }, error = function(e) {
      return(FALSE)
    })
    if(is_valid){
      title <- "Verification successful!"
      message <- "Signature of uploaded json successfully verified."
      type <- "info"
    } else {
      title <- "Verification failed!"
      message <- "Signature of uploaded json could not be verified."
      type <- "warning"
    }
    shinyalert(
      title = title,
      text = message,
      type = type
    )
  } else {
    message <- "No signature was found in the uploaded json."
    shinyalert(
      title = "Warning",
      text = message,
      type = "warning"
    )
  }
  }
}
