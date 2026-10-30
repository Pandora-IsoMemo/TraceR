.tracer_table_required_columns <- c(
  "ID",
  "Label",
  "Type",
  "From",
  "To",
  "Unique identifier"
)
.tracer_table_default_optional_columns <- paste0("Optional ", 1:5)
.tracer_table_default_columns <- c(
  .tracer_table_required_columns,
  .tracer_table_default_optional_columns
)

.tracer_table_issues <- function(rows = integer(), fields = character(), messages = character()) {
  data.frame(
    row = as.integer(rows),
    field = as.character(fields),
    message = as.character(messages),
    stringsAsFactors = FALSE
  )
}

.tracer_table_structure_issues <- function(x) {
  issues <- .tracer_table_issues()
  add_issue <- function(field, message) {
    issues[nrow(issues) + 1L, ] <<- .tracer_table_issues(NA_integer_, field, message)
  }

  if (!inherits(x, "tracer_table")) {
    add_issue("class", "Object must inherit from 'tracer_table'.")
  }

  if (!is.list(x) || !identical(names(x), "data")) {
    add_issue("data", "Object must be a list containing only a 'data' component.")
  }

  data <- if (is.list(x) && "data" %in% names(x)) x[["data"]] else NULL
  if (!is.data.frame(data)) {
    add_issue("data", "The 'data' component must be a data.frame.")
    return(issues)
  }

  columns <- names(data)
  if (length(columns) != ncol(data) || anyNA(columns) || any(trimws(columns) == "")) {
    add_issue("columns", "Column names must be non-empty and present for every column.")
  }

  if (anyDuplicated(columns)) {
    duplicates <- unique(columns[duplicated(columns)])
    add_issue("columns", paste0("Column names must be unique; duplicated: ", paste(duplicates, collapse = ", "), "."))
  }

  missing_required <- setdiff(.tracer_table_required_columns, columns)
  for (column in missing_required) {
    add_issue(column, paste0("Required column '", column, "' is missing."))
  }

  optional_columns <- setdiff(columns, .tracer_table_required_columns)
  if (length(optional_columns) != 5L) {
    add_issue(
      "columns",
      paste0("Exactly five optional columns are required; found ", length(optional_columns), ".")
    )
  }

  non_character <- columns[!vapply(data, is.character, logical(1))]
  for (column in non_character) {
    add_issue(column, paste0("Column '", column, "' must be character."))
  }

  issues
}

.tracer_table_is_missing <- function(x) {
  is.na(x) | trimws(x) == ""
}

.tracer_table_append_issue <- function(issues, row, field, message) {
  rbind(issues, .tracer_table_issues(row, field, message))
}

.tracer_table_content_issues <- function(data) {
  errors <- .tracer_table_issues()
  warnings <- .tracer_table_issues()
  if (nrow(data) == 0L) {
    return(list(errors = errors, warnings = warnings))
  }

  populated <- rep(FALSE, nrow(data))
  for (column in data) {
    populated <- populated | !.tracer_table_is_missing(column)
  }

  ids <- data[["ID"]]
  id_missing <- .tracer_table_is_missing(ids)
  for (row in which(populated & id_missing)) {
    errors <- .tracer_table_append_issue(
      errors,
      row,
      "ID",
      paste0("Row ", row, " contains data but has no ID.")
    )
  }

  duplicate_ids <- unique(ids[!id_missing & populated & (duplicated(ids) | duplicated(ids, fromLast = TRUE))])
  for (id in duplicate_ids) {
    rows <- which(!id_missing & ids == id)
    message <- paste0(
      "Duplicate ID '", id, "' in rows ", paste(rows, collapse = ", "), "."
    )
    for (row in rows) {
      errors <- .tracer_table_append_issue(errors, row, "ID", message)
    }
  }

  types <- data[["Type"]]
  type_missing <- .tracer_table_is_missing(types)
  for (row in which(populated & type_missing)) {
    errors <- .tracer_table_append_issue(
      errors,
      row,
      "Type",
      paste0("Row ", row, " contains data but has no Type.")
    )
  }

  allowed_types <- c("node", "edge", "capsule")
  invalid_types <- which(populated & !type_missing & !types %in% allowed_types)
  for (row in invalid_types) {
    errors <- .tracer_table_append_issue(
      errors,
      row,
      "Type",
      paste0("Invalid Type '", types[row], "' in row ", row, "; expected node, edge, or capsule.")
    )
  }

  edge_rows <- which(!is.na(types) & types == "edge")
  for (row in edge_rows) {
    edge_name <- if (id_missing[row]) paste0("row ", row) else paste0("edge '", ids[row], "'")

    for (field in c("From", "To")) {
      reference <- data[[field]][row]
      if (.tracer_table_is_missing(reference)) {
        errors <- .tracer_table_append_issue(
          errors,
          row,
          field,
          paste0("Edge ", edge_name, " is missing required field '", field, "'.")
        )
        next
      }

      reference_rows <- which(!id_missing & ids == reference)
      reference_types <- types[reference_rows]
      edge_references <- reference_rows[!is.na(reference_types) & reference_types == "edge"]
      element_references <- reference_rows[!is.na(reference_types) & reference_types %in% c("node", "capsule")]

      if (length(edge_references) > 0L) {
        errors <- .tracer_table_append_issue(
          errors,
          row,
          field,
          paste0(
            "Edge ", edge_name, " references edge ID '", reference,
            "' in field '", field, "'; edge endpoints must reference a node or capsule."
          )
        )
      } else if (length(element_references) == 0L) {
        warnings <- .tracer_table_append_issue(
          warnings,
          row,
          field,
          paste0(
            "Edge ", edge_name, " references unknown node or capsule ID '",
            reference, "' in field '", field, "'."
          )
        )
      }
    }
  }

  list(errors = errors, warnings = warnings)
}

#' The TraceR table class
#'
#' A `tracer_table` represents one network level and has the structure
#' `structure(list(data = data), class = "tracer_table")`. Its `data` frame has
#' exactly eleven character columns: `ID`, `Label`, `Type`, `From`, `To`, and
#' `Unique identifier`, plus five optional columns. Optional field names are
#' stored only as the names of those columns in `data`.
#'
#' `NA_character_`, empty strings, and whitespace-only strings are considered
#' missing during validation. An entirely blank row is ignored so it can be
#' used as an editing row; populated rows are checked for IDs and types. Edge
#' references are checked within this table only. Hierarchical capsule paths
#' are not currently resolved.
#'
#' @name tracer_table
#' @docType class
#' @aliases tracer_table
NULL

#' Construct a single-level TraceR table
#'
#' Creates a list-based `tracer_table` containing one data frame. By default,
#' the data frame has six required character columns (`ID`, `Label`, `Type`,
#' `From`, `To`, and `Unique identifier`) followed by five character columns
#' named `Optional 1` through `Optional 5`. Optional column names may be
#' changed by callers; the names in `data` are their sole source of truth.
#'
#' Supplied data must satisfy the structural schema, but content validation is
#' separate so incomplete editing states, duplicate IDs, and unresolved edge
#' references can be represented. Column order is preserved for supplied data.
#' Use `validate_tracer_table()` to inspect table contents.
#'
#' @param data `NULL` for an empty table, or a structurally valid data frame.
#' @return An object of class `tracer_table` with a single `data` component.
#' @export
#' @examples
#' table <- new_tracer_table()
#' ncol(table$data)
#'
#' data <- table$data
#' data[1, ] <- NA_character_
#' data$ID[1] <- "node-1"
#' new_tracer_table(data)
new_tracer_table <- function(data = NULL) {
  if (is.null(data)) {
    data <- as.data.frame(
      replicate(length(.tracer_table_default_columns), character(), simplify = FALSE),
      check.names = FALSE
    )
    names(data) <- .tracer_table_default_columns
  }

  x <- structure(list(data = data), class = "tracer_table")
  issues <- .tracer_table_structure_issues(x)
  if (nrow(issues) > 0L) {
    stop(paste(issues$message, collapse = " "), call. = FALSE)
  }

  x
}

#' Validate a TraceR table
#'
#' Checks the structure and content of a `tracer_table` without modifying it.
#' Empty rows are ignored. Missing or duplicate IDs, missing or invalid types,
#' missing edge endpoints, and edge-to-edge references are errors. References
#' that do not identify a node or capsule in the current table are warnings.
#' Hierarchical capsule paths are not resolved.
#'
#' @param x Object to validate.
#' @return A list with `errors` and `warnings`, each a data frame with integer
#'   `row`, character `field`, and human-readable `message` columns. Structural
#'   errors are reported before content validation; if present, content checks
#'   are skipped. The input is never modified.
#' @export
#' @examples
#' validate_tracer_table(new_tracer_table())
validate_tracer_table <- function(x) {
  errors <- .tracer_table_structure_issues(x)
  if (nrow(errors) > 0L) {
    return(list(errors = errors, warnings = .tracer_table_issues()))
  }

  .tracer_table_content_issues(x$data)
}