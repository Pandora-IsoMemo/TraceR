.tracer_table_json_format <- "tracer_table"
.tracer_table_json_version <- 1L
.tracer_table_signed_format <- "tracer_table_signed"
.tracer_table_signed_version <- 1L
.tracer_table_signature_algorithm <- "RSA-SHA256"

.tracer_table_to_payload <- function(x) {
  if (!inherits(x, "tracer_table")) {
    stop("Object must inherit from 'tracer_table'.", call. = FALSE)
  }
  table <- new_tracer_table(x$data)
  data <- table$data

  rows <- lapply(seq_len(nrow(data)), function(row_index) {
    row <- lapply(data, function(column) enc2utf8(column[[row_index]]))
    names(row) <- names(data)
    row
  })

  list(
    format = .tracer_table_json_format,
    version = .tracer_table_json_version,
    columns = unname(enc2utf8(names(data))),
    data = rows
  )
}

.tracer_table_json_encode <- function(x) {
  as.character(jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    pretty = FALSE,
    digits = NA,
    force = TRUE
  ))
}

.tracer_table_json_parse <- function(json) {
  if (is.raw(json)) {
    json <- rawToChar(json)
  }
  if (is.character(json) && length(json) == 1L && !is.na(json)) {
    return(jsonlite::fromJSON(json, simplifyVector = FALSE))
  }
  if (is.list(json)) {
    return(json)
  }
  stop("JSON input must be one JSON string or a parsed JSON object.", call. = FALSE)
}

.tracer_table_json_scalar <- function(value, field) {
  if (is.null(value)) {
    return(NA_character_)
  }
  if (length(value) == 1L && is.na(value) && (is.character(value) || is.logical(value))) {
    return(NA_character_)
  }
  if (!is.character(value) || length(value) != 1L) {
    stop(paste0("Field '", field, "' must be a string or null."), call. = FALSE)
  }
  enc2utf8(value)
}

.tracer_table_payload_to_object <- function(payload) {
  required_fields <- c("format", "version", "columns", "data")
  if (!is.list(payload) || anyDuplicated(names(payload)) || !setequal(names(payload), required_fields)) {
    stop("Invalid tracer_table JSON fields.", call. = FALSE)
  }
  if (!identical(payload$format, .tracer_table_json_format)) {
    stop("Unsupported JSON format; expected 'tracer_table'.", call. = FALSE)
  }
  if (!is.numeric(payload$version) || length(payload$version) != 1L ||
      is.na(payload$version) || payload$version != .tracer_table_json_version) {
    stop("Unsupported tracer_table JSON version.", call. = FALSE)
  }

  columns <- payload$columns
  if (is.list(columns)) {
    if (!all(vapply(columns, function(value) is.character(value) && length(value) == 1L, logical(1)))) {
      stop("The columns field must contain only strings.", call. = FALSE)
    }
    columns <- unlist(columns, use.names = FALSE)
  }
  if (!is.character(columns) || anyNA(columns)) {
    stop("The columns field must contain only strings.", call. = FALSE)
  }
  columns <- enc2utf8(columns)

  rows <- payload$data
  if (is.data.frame(rows)) {
    rows <- lapply(seq_len(nrow(rows)), function(row_index) {
      lapply(rows, function(column) column[[row_index]])
    })
    names(rows) <- NULL
  }
  if (!is.list(rows)) {
    stop("The data field must be an array of row objects.", call. = FALSE)
  }

  normalized_rows <- lapply(rows, function(row) {
    if (!is.list(row) || is.null(names(row)) || anyDuplicated(names(row)) ||
        length(row) != length(columns) || !setequal(names(row), columns)) {
      stop("Every data row must contain exactly the declared columns.", call. = FALSE)
    }
    stats::setNames(
      lapply(columns, function(column) .tracer_table_json_scalar(row[[column]], column)),
      columns
    )
  })

  values <- stats::setNames(lapply(columns, function(column) {
    vapply(normalized_rows, function(row) row[[column]], character(1), USE.NAMES = FALSE)
  }), columns)
  new_tracer_table(as.data.frame(values, check.names = FALSE, stringsAsFactors = FALSE))
}

#' Serialize a tracer_table as versioned JSON
#'
#' The version 1 object contains `format = "tracer_table"`, `version = 1`, an
#' explicit ordered `columns` array, and ordered row objects in `data`. Missing
#' character values are JSON `null`; empty strings remain empty strings. The
#' compact serialization is deterministic for a given table and contains no
#' DiagrammeR or Shiny state.
#'
#' @param x A structurally valid `tracer_table`.
#' @return A compact JSON string.
#' @export
tracer_table_to_json <- function(x) {
  .tracer_table_json_encode(.tracer_table_to_payload(x))
}

#' Deserialize versioned tracer_table JSON
#'
#' This function accepts either raw JSON text or a parsed JSON object. Signed
#' envelopes must be verified with `tracer_table_from_signed_json()` instead.
#'
#' @param json JSON text, raw bytes, or a parsed JSON object.
#' @return A structurally valid `tracer_table`.
#' @export
tracer_table_from_json <- function(json) {
  payload <- .tracer_table_json_parse(json)
  if (identical(payload$format, .tracer_table_signed_format)) {
    stop("Signed tracer_table JSON must be verified before deserialization.", call. = FALSE)
  }
  .tracer_table_payload_to_object(payload)
}

.tracer_table_signed_bytes <- function(payload) {
  prefix <- paste0(
    "TraceR/", .tracer_table_signed_format, "/", .tracer_table_signed_version,
    "\n", .tracer_table_signature_algorithm, "\n"
  )
  charToRaw(enc2utf8(paste0(prefix, payload)))
}

#' Serialize and sign a tracer_table JSON payload
#'
#' The version 1 envelope contains `format = "tracer_table_signed"`, `version
#' = 1`, `algorithm = "RSA-SHA256"`, the exact compact unsigned JSON payload as
#' a string, and a base64 `signature`. The signature covers a TraceR
#' format/version prefix, the algorithm identifier, and those exact UTF-8
#' payload bytes. No other algorithm is accepted for this format.
#'
#' @param x A structurally valid `tracer_table`.
#' @param private_key An OpenSSL RSA private key.
#' @return A signed-envelope JSON string.
#' @export
tracer_table_to_signed_json <- function(x, private_key) {
  if (is.null(private_key)) {
    stop("An RSA private key is required to sign a tracer_table.", call. = FALSE)
  }
  payload <- tracer_table_to_json(x)
  signature <- openssl::signature_create(
    data = .tracer_table_signed_bytes(payload),
    hash = openssl::sha256,
    key = private_key
  )
  .tracer_table_json_encode(list(
    format = .tracer_table_signed_format,
    version = .tracer_table_signed_version,
    algorithm = .tracer_table_signature_algorithm,
    payload = payload,
    signature = as.character(openssl::base64_encode(signature))
  ))
}

#' Verify and deserialize signed tracer_table JSON
#'
#' Rejects unknown algorithms, missing keys, malformed envelopes, and invalid
#' signatures. Verification uses the payload string bytes from the envelope
#' directly and does not reconstruct or reformat the signed payload. Unsigned
#' input must use [tracer_table_from_json()].
#'
#' @param json Signed JSON text, raw bytes, or a parsed JSON object.
#' @param public_key An OpenSSL RSA public key.
#' @return The verified `tracer_table`.
#' @export
tracer_table_from_signed_json <- function(json, public_key) {
  if (is.null(public_key)) {
    stop("A public key is required to verify a signed tracer_table.", call. = FALSE)
  }
  envelope <- .tracer_table_json_parse(json)
  required_fields <- c("format", "version", "algorithm", "payload", "signature")
  if (!is.list(envelope) || anyDuplicated(names(envelope)) ||
      !setequal(names(envelope), required_fields)) {
    stop("Invalid signed tracer_table JSON fields.", call. = FALSE)
  }
  if (!identical(envelope$format, .tracer_table_signed_format)) {
    stop("Unsupported signed JSON format.", call. = FALSE)
  }
  if (!is.numeric(envelope$version) || length(envelope$version) != 1L ||
      is.na(envelope$version) || envelope$version != .tracer_table_signed_version) {
    stop("Unsupported signed tracer_table JSON version.", call. = FALSE)
  }
  if (!identical(envelope$algorithm, .tracer_table_signature_algorithm)) {
    stop("Unsupported signature algorithm; expected RSA-SHA256.", call. = FALSE)
  }
  if (!is.character(envelope$payload) || length(envelope$payload) != 1L || is.na(envelope$payload) ||
      !is.character(envelope$signature) || length(envelope$signature) != 1L || is.na(envelope$signature)) {
    stop("Signed tracer_table JSON requires a payload and signature string.", call. = FALSE)
  }

  signature <- tryCatch(
    openssl::base64_decode(envelope$signature),
    error = function(error) stop("Malformed base64 signature.", call. = FALSE)
  )
  verified <- tryCatch(
    openssl::signature_verify(
      data = .tracer_table_signed_bytes(envelope$payload),
      sig = signature,
      hash = openssl::sha256,
      pubkey = public_key
    ),
    error = function(error) FALSE
  )
  if (!isTRUE(verified)) {
    stop("tracer_table signature verification failed.", call. = FALSE)
  }

  tracer_table_from_json(envelope$payload)
}