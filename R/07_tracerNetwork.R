.tracer_network_issue <- function(
  path = character(),
  row = integer(),
  field = character(),
  message = character()
) {
  data.frame(
    path = as.character(path),
    row = as.integer(row),
    field = as.character(field),
    message = as.character(message),
    stringsAsFactors = FALSE
  )
}

.tracer_network_append_issue <- function(issues, path, row, field, message) {
  rbind(issues, .tracer_network_issue(path, row, field, message))
}

.tracer_network_valid_path <- function(path, allow_root = TRUE) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    return(FALSE)
  }
  if (identical(path, "")) {
    return(allow_root)
  }
  if (startsWith(path, "/") || endsWith(path, "/") || grepl("//", path, fixed = TRUE)) {
    return(FALSE)
  }

  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  length(parts) > 0L && all(vapply(parts, function(part) {
    is.null(.tracer_table_id_error(part)) &&
      is.null(.tracer_capsule_id_error(part))
  }, logical(1)))
}

.tracer_network_tables <- function(network) {
  c(stats::setNames(list(network$root), ""), network$tables)
}

.tracer_network_table_from_map <- function(tables, path) {
  index <- match(path, names(tables))
  if (is.na(index)) NULL else tables[[index]]
}

.tracer_network_parent_path <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (length(parts) == 1L) "" else paste(parts[-length(parts)], collapse = "/")
}

.tracer_network_issue_rows <- function(path, issues) {
  if (nrow(issues) == 0L) {
    return(.tracer_network_issue()[FALSE, , drop = FALSE])
  }
  data.frame(
    path = rep(path, nrow(issues)),
    row = issues$row,
    field = issues$field,
    message = issues$message,
    stringsAsFactors = FALSE
  )
}

.tracer_network_relationship_issues <- function(network) {
  issues <- .tracer_network_issue()[FALSE, , drop = FALSE]
  tables <- .tracer_network_tables(network)
  paths <- names(tables)

  for (path in paths) {
    if (identical(path, "")) next
    if (!.tracer_network_valid_path(path, allow_root = FALSE)) {
      issues <- .tracer_network_append_issue(
        issues, path, NA_integer_, "path",
        paste0("Invalid capsule path '", path, "'.")
      )
      next
    }

    parent_path <- .tracer_network_parent_path(path)
    if (!parent_path %in% paths) {
      issues <- .tracer_network_append_issue(
        issues, path, NA_integer_, "path",
        paste0("Capsule path '", path, "' has no parent table '", parent_path, "'.")
      )
      next
    }

    parent <- .tracer_network_table_from_map(tables, parent_path)
    if (!inherits(parent, "tracer_table") || !is.data.frame(parent$data) ||
        !all(c("ID", "Type") %in% names(parent$data))) {
      next
    }
    path_parts <- strsplit(path, "/", fixed = TRUE)[[1]]
    capsule_id <- path_parts[[length(path_parts)]]
    matching_rows <- which(
      !.tracer_table_is_missing(parent$data$ID) &
        parent$data$ID == capsule_id &
        !is.na(parent$data$Type) &
        parent$data$Type == "capsule"
    )
    if (length(matching_rows) != 1L) {
      issues <- .tracer_network_append_issue(
        issues, path, NA_integer_, "path",
        paste0(
          "Capsule path '", path,
          "' must correspond to exactly one capsule row in its parent table."
        )
      )
    }
  }

  for (path in paths) {
    table <- .tracer_network_table_from_map(tables, path)
    if (!inherits(table, "tracer_table") || !is.data.frame(table$data) ||
        !all(c("ID", "Type") %in% names(table$data))) {
      next
    }

    data <- table$data
    capsule_rows <- which(!is.na(data$Type) & data$Type == "capsule")
    for (row in capsule_rows) {
      capsule_id <- data$ID[row]
      if (.tracer_table_is_missing(capsule_id) ||
          !is.null(.tracer_table_id_error(capsule_id)) ||
          !is.null(.tracer_capsule_id_error(capsule_id))) {
        next
      }

      child_path <- if (identical(path, "")) {
        capsule_id
      } else {
        paste(path, capsule_id, sep = "/")
      }
      if (!child_path %in% paths) {
        issues <- .tracer_network_append_issue(
          issues, path, row, "ID",
          paste0("Capsule '", capsule_id, "' has no child table at path '", child_path, "'.")
        )
      }
    }
  }

  issues
}

.tracer_network_structure_issues <- function(network) {
  issues <- .tracer_network_issue()[FALSE, , drop = FALSE]
  if (!inherits(network, "tracer_network")) {
    return(.tracer_network_issue(NA_character_, NA_integer_, "class", "Object must inherit from 'tracer_network'."))
  }
  if (!is.list(network) || !identical(names(network), c("root", "tables"))) {
    return(.tracer_network_issue(
      NA_character_, NA_integer_, "structure",
      "A tracer_network must contain only 'root' and 'tables' components."
    ))
  }
  if (!is.list(network$tables)) {
    return(.tracer_network_issue(NA_character_, NA_integer_, "tables", "'tables' must be a named list."))
  }

  paths <- names(network$tables)
  if (length(network$tables) > 0L &&
      (is.null(paths) || anyNA(paths) || any(paths == "") || anyDuplicated(paths))) {
    issues <- .tracer_network_append_issue(
      issues, NA_character_, NA_integer_, "tables",
      "Child tables must have unique, non-empty capsule paths."
    )
  }

  all_tables <- c(list(root = network$root), network$tables)
  all_paths <- c("", paths)
  for (index in seq_along(all_tables)) {
    path <- all_paths[[index]]
    table <- all_tables[[index]]
    if (!inherits(table, "tracer_table")) {
      issues <- .tracer_network_append_issue(
        issues, path, NA_integer_, "class",
        "Every network level must contain a tracer_table."
      )
      next
    }

    structure <- .tracer_table_structure_issues(table)
    issues <- rbind(issues, .tracer_network_issue_rows(path, structure))
  }

  if (length(network$tables) > 0L && !is.null(paths)) {
    for (path in paths) {
      if (!.tracer_network_valid_path(path, allow_root = FALSE)) {
        issues <- .tracer_network_append_issue(
          issues, path, NA_integer_, "path",
          paste0("Invalid capsule path '", path, "'.")
        )
      }
    }
  }

  issues <- rbind(issues, .tracer_network_relationship_issues(network))
  issues
}

#' The TraceR network class
#'
#' A `tracer_network` stores one root `tracer_table` and a named list of
#' descendant tables indexed by slash-separated capsule paths. The root path
#' is the empty string.
#'
#' @name tracer_network
#' @docType class
#' @aliases tracer_network
NULL

#' Create a hierarchical TraceR network
#'
#' The constructor checks table schemas and capsule-to-child consistency.
#' Use `validate_tracer_network()` to validate table contents.
#'
#' @param root_table The root `tracer_table`.
#' @param tables A named list of descendant `tracer_table` objects keyed by
#'   capsule path, such as `"C1/C2"`.
#' @return A validated `tracer_network`.
#' @export
new_tracer_network <- function(root_table = new_tracer_table(), tables = list()) {
  network <- structure(list(root = root_table, tables = tables), class = "tracer_network")
  issues <- .tracer_network_structure_issues(network)
  if (nrow(issues) > 0L) {
    stop(paste(issues$message, collapse = " "), call. = FALSE)
  }
  network
}

#' Validate a TraceR network hierarchy
#'
#' Checks each table and verifies that capsule rows and child-table paths
#' correspond exactly. Local unresolved edge references remain warnings.
#'
#' @param network Object to validate.
#' @return A list with `errors` and `warnings` data frames containing `path`,
#'   `row`, `field`, and `message` columns.
#' @export
validate_tracer_network <- function(network) {
  empty <- .tracer_network_issue()[FALSE, , drop = FALSE]
  errors <- .tracer_network_structure_issues(network)
  if (nrow(errors) > 0L || !inherits(network, "tracer_network") ||
      !is.list(network) || !identical(names(network), c("root", "tables")) ||
      !is.list(network$tables)) {
    return(list(errors = errors, warnings = empty))
  }

  warnings <- empty
  for (path in names(.tracer_network_tables(network))) {
    table <- .tracer_network_table_from_map(.tracer_network_tables(network), path)
    if (!inherits(table, "tracer_table")) next
    if (nrow(.tracer_table_structure_issues(table)) > 0L) next
    content <- .tracer_table_content_issues(table$data)
    errors <- rbind(errors, .tracer_network_issue_rows(path, content$errors))
    warnings <- rbind(warnings, .tracer_network_issue_rows(path, content$warnings))
  }

  list(errors = errors, warnings = warnings)
}

#' Retrieve a table from a TraceR network
#'
#' @param network A `tracer_network`.
#' @param path Capsule path, or `""` for the root.
#' @return The `tracer_table` at `path`.
#' @export
get_tracer_table <- function(network, path = "") {
  if (!inherits(network, "tracer_network") || !.tracer_network_valid_path(path)) {
    stop("A valid tracer_network and capsule path are required.", call. = FALSE)
  }
  if (identical(path, "")) return(network$root)
  if (!path %in% names(network$tables)) {
    stop(paste0("No tracer_table exists at path '", path, "'."), call. = FALSE)
  }
  network$tables[[path]]
}

#' Replace a table in a TraceR network
#'
#' This operation does not create, rename, or remove capsule paths. The
#' proposed table must preserve the current set of child capsules.
#'
#' @param network A `tracer_network`.
#' @param path Capsule path, or `""` for the root.
#' @param table Replacement `tracer_table`.
#' @return An updated `tracer_network`.
#' @export
set_tracer_table <- function(network, path, table) {
  current <- get_tracer_table(network, path)
  if (!inherits(table, "tracer_table")) {
    stop("'table' must inherit from 'tracer_table'.", call. = FALSE)
  }
  table <- new_tracer_table(table$data)
  current_capsules <- list_tracer_capsules(network, path)
  proposed_capsules <- if ("Type" %in% names(table$data) && "ID" %in% names(table$data)) {
    table$data$ID[!is.na(table$data$Type) & table$data$Type == "capsule"]
  } else {
    character()
  }
  proposed_capsules <- proposed_capsules[!is.na(proposed_capsules) & trimws(proposed_capsules) != ""]
  if (!setequal(current_capsules, proposed_capsules) ||
      length(current_capsules) != length(proposed_capsules)) {
    stop(
      "The capsule IDs must remain unchanged; use the capsule hierarchy operations to modify them.",
      call. = FALSE
    )
  }

  updated <- network
  if (identical(path, "")) {
    updated$root <- table
  } else {
    updated$tables[[path]] <- table
  }
  consistency <- .tracer_network_structure_issues(updated)
  if (nrow(consistency) > 0L) {
    stop(paste(consistency$message, collapse = " "), call. = FALSE)
  }
  updated
}

#' List table paths in a TraceR network
#'
#' @param network A `tracer_network`.
#' @return The root path `""` followed by descendant capsule paths.
#' @export
tracer_network_paths <- function(network) {
  if (!inherits(network, "tracer_network")) {
    stop("Object must inherit from 'tracer_network'.", call. = FALSE)
  }
  c("", names(network$tables))
}

#' List child capsule IDs in a TraceR network
#'
#' @param network A `tracer_network`.
#' @param path Parent capsule path, or `""` for the root.
#' @return Capsule IDs in table row order.
#' @export
list_tracer_capsules <- function(network, path = "") {
  table <- get_tracer_table(network, path)
  rows <- which(!is.na(table$data$Type) & table$data$Type == "capsule")
  table$data$ID[rows]
}

#' Add a capsule and empty child table
#'
#' @param network A `tracer_network`.
#' @param parent_path Parent capsule path, or `""` for the root.
#' @param capsule_id New capsule ID.
#' @param parent_table Optional current parent-table value to preserve other
#'   unsaved edits.
#' @param child_table Table to use for the new child; defaults to an empty table.
#' @return An updated `tracer_network`.
#' @export
add_tracer_capsule <- function(
  network,
  parent_path,
  capsule_id,
  parent_table = NULL,
  child_table = new_tracer_table()
) {
  current_parent <- get_tracer_table(network, parent_path)
  if (!is.null(parent_table)) current_parent <- parent_table
  if (!inherits(current_parent, "tracer_table") || !inherits(child_table, "tracer_table")) {
    stop("Parent and child tables must inherit from 'tracer_table'.", call. = FALSE)
  }
  if (!is.character(capsule_id) || length(capsule_id) != 1L || is.na(capsule_id) ||
      !nzchar(trimws(capsule_id))) {
    stop("Capsule ID must be a non-empty string.", call. = FALSE)
  }
  id_error <- .tracer_table_id_error(capsule_id)
  if (!is.null(id_error)) {
    stop(id_error, call. = FALSE)
  }
  id_error <- .tracer_capsule_id_error(capsule_id)
  if (!is.null(id_error)) {
    stop(id_error, call. = FALSE)
  }

  data <- current_parent$data
  if (capsule_id %in% data$ID || tolower(capsule_id) %in% tolower(data$ID[!is.na(data$ID)])) {
    stop(paste0("ID '", capsule_id, "' already exists in the parent table."), call. = FALSE)
  }
  existing_capsules <- list_tracer_capsules(network, parent_path)
  draft_capsules <- data$ID[!is.na(data$Type) & data$Type == "capsule"]
  if (!setequal(existing_capsules, draft_capsules) ||
      length(existing_capsules) != length(draft_capsules)) {
    stop("The parent table contains uncommitted capsule changes.", call. = FALSE)
  }

  new_row <- as.data.frame(
    stats::setNames(rep(list(""), ncol(data)), names(data)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  new_row$ID <- capsule_id
  new_row$Type <- "capsule"
  data <- rbind(data, new_row)
  parent_table <- new_tracer_table(data)
  child_path <- if (identical(parent_path, "")) {
    capsule_id
  } else {
    paste(parent_path, capsule_id, sep = "/")
  }
  updated <- network
  if (identical(parent_path, "")) {
    updated$root <- parent_table
  } else {
    updated$tables[[parent_path]] <- parent_table
  }
  if (child_path %in% names(updated$tables)) {
    stop(paste0("A table already exists at path '", child_path, "'."), call. = FALSE)
  }
  updated$tables[[child_path]] <- child_table
  new_tracer_network(updated$root, updated$tables)
}

#' Rename a capsule and all descendant paths
#'
#' @param network A `tracer_network`.
#' @param path Capsule path to rename.
#' @param new_id New capsule ID.
#' @param parent_table Optional current parent-table value to preserve other
#'   unsaved edits.
#' @return An updated `tracer_network`.
#' @export
rename_tracer_capsule <- function(network, path, new_id, parent_table = NULL) {
  if (!.tracer_network_valid_path(path, allow_root = FALSE)) {
    stop("A valid capsule path is required.", call. = FALSE)
  }
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  old_id <- parts[[length(parts)]]
  parent_path <- .tracer_network_parent_path(path)
  current_parent <- get_tracer_table(network, parent_path)
  if (!is.null(parent_table)) current_parent <- parent_table
  if (!inherits(current_parent, "tracer_table") ||
      !is.character(new_id) || length(new_id) != 1L || is.na(new_id) ||
      !nzchar(trimws(new_id))) {
    stop("Invalid parent table or new capsule ID.", call. = FALSE)
  }
  id_error <- .tracer_table_id_error(new_id)
  if (is.null(id_error)) id_error <- .tracer_capsule_id_error(new_id)
  if (!is.null(id_error)) {
    stop(id_error, call. = FALSE)
  }
  data <- current_parent$data
  capsule_row <- which(
    !is.na(data$ID) & data$ID == old_id &
      !is.na(data$Type) & data$Type == "capsule"
  )
  if (length(capsule_row) != 1L) {
    stop(paste0("Capsule '", old_id, "' is not present exactly once in its parent table."), call. = FALSE)
  }
  if (new_id == old_id) return(network)
  other_ids <- data$ID[-capsule_row]
  if (tolower(new_id) %in% tolower(other_ids[!is.na(other_ids)])) {
    stop(paste0("ID '", new_id, "' already exists in the parent table."), call. = FALSE)
  }

  data$ID[capsule_row] <- new_id
  edge_rows <- which(!is.na(data$Type) & data$Type == "edge")
  for (field in c("From", "To")) {
    references <- edge_rows[
      !is.na(data[[field]][edge_rows]) & data[[field]][edge_rows] == old_id
    ]
    data[[field]][references] <- new_id
  }

  updated <- network
  if (identical(parent_path, "")) {
    updated$root <- new_tracer_table(data)
  } else {
    updated$tables[[parent_path]] <- new_tracer_table(data)
  }

  new_path <- if (identical(parent_path, "")) {
    new_id
  } else {
    paste(parent_path, new_id, sep = "/")
  }
  descendants <- names(updated$tables)
  affected <- descendants[
    descendants == path | startsWith(descendants, paste0(path, "/"))
  ]
  replacements <- ifelse(
    affected == path,
    new_path,
    paste0(new_path, substring(affected, nchar(path) + 1L))
  )
  if (any(replacements %in% setdiff(descendants, affected))) {
    stop("Renaming would conflict with an existing capsule path.", call. = FALSE)
  }
  names(updated$tables)[match(affected, names(updated$tables))] <- replacements
  new_tracer_network(updated$root, updated$tables)
}

#' Remove a capsule and its descendants
#'
#' @param network A `tracer_network`.
#' @param path Capsule path to remove.
#' @param parent_table Optional current parent-table value to preserve other
#'   unsaved edits.
#' @return An updated `tracer_network`.
#' @export
remove_tracer_capsule <- function(network, path, parent_table = NULL) {
  if (!.tracer_network_valid_path(path, allow_root = FALSE)) {
    stop("A valid capsule path is required.", call. = FALSE)
  }
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  capsule_id <- parts[[length(parts)]]
  parent_path <- .tracer_network_parent_path(path)
  current_parent <- get_tracer_table(network, parent_path)
  if (!is.null(parent_table)) current_parent <- parent_table
  data <- current_parent$data
  capsule_row <- which(
    !is.na(data$ID) & data$ID == capsule_id &
      !is.na(data$Type) & data$Type == "capsule"
  )
  if (length(capsule_row) != 1L) {
    stop(paste0("Capsule '", capsule_id, "' is not present exactly once in its parent table."), call. = FALSE)
  }

  data <- data[-capsule_row, , drop = FALSE]
  updated <- network
  if (identical(parent_path, "")) {
    updated$root <- new_tracer_table(data)
  } else {
    updated$tables[[parent_path]] <- new_tracer_table(data)
  }
  descendants <- names(updated$tables)
  removed <- descendants[
    descendants == path | startsWith(descendants, paste0(path, "/"))
  ]
  updated$tables[removed] <- NULL
  new_tracer_network(updated$root, updated$tables)
}

.tracer_collapse_empty_report <- function() {
  list(
    promoted_ids = data.frame(
      old_id = character(),
      new_id = character(),
      type = character(),
      stringsAsFactors = FALSE
    ),
    updated_paths = data.frame(
      old_path = character(),
      new_path = character(),
      stringsAsFactors = FALSE
    ),
    generated_edges = data.frame(
      id = character(),
      from = character(),
      to = character(),
      original_edges = character(),
      stringsAsFactors = FALSE
    ),
    updated_edges = data.frame(
      id = character(),
      old_from = character(),
      old_to = character(),
      from = character(),
      to = character(),
      stringsAsFactors = FALSE
    ),
    removed_edges = data.frame(
      id = character(),
      from = character(),
      to = character(),
      stringsAsFactors = FALSE
    ),
    metadata_conflicts = data.frame(
      generated_edge = character(),
      original_edges = character(),
      field = character(),
      original_value_1 = character(),
      original_value_2 = character(),
      result = character(),
      stringsAsFactors = FALSE
    ),
    edge_changes = data.frame(
      action = character(),
      original_edges = character(),
      resulting_edge = character(),
      original_connections = character(),
      resulting_connection = character(),
      stringsAsFactors = FALSE
    ),
    summary = list(
      promoted_nodes = 0L,
      promoted_internal_edges = 0L,
      promoted_capsules = 0L,
      generated_edges = 0L,
      updated_edges = 0L,
      removed_edges = 0L,
      metadata_conflicts = 0L
    )
  )
}

.tracer_collapse_result <- function(
  success,
  network = NULL,
  issues = .tracer_network_issue()[FALSE, , drop = FALSE],
  report = .tracer_collapse_empty_report(),
  path = ""
) {
  c(
    list(
      success = success,
      network = network,
      issues = issues,
      path = path
    ),
    report
  )
}

#' Collapse a capsule into its parent table
#'
#' Promotes the selected capsule's immediate table by one hierarchy level,
#' prefixes promoted IDs, updates local edge references and descendant paths,
#' and rewrites parent-level edges connected to the capsule. The input network
#' is never modified. Expected validation failures are returned as structured
#' issues and leave `network` as `NULL`. The parent and capsule tables must
#' use the same five optional column names. A successful result includes
#' detailed edge changes and metadata conflicts for the application to report.
#'
#' @param network A `tracer_network`.
#' @param path Non-root capsule path to collapse.
#' @return A list with `success`, `network`, `issues`, `path`, `promoted_ids`,
#'   `updated_paths`, `generated_edges`, `updated_edges`, `removed_edges`,
#'   `metadata_conflicts`, `edge_changes`, and `summary`. On failure,
#'   `network` is `NULL` and `issues` describes the blocking problems.
#' @export
collapse_tracer_capsule <- function(network, path) {
  report <- .tracer_collapse_empty_report()
  issues <- .tracer_network_issue()[FALSE, , drop = FALSE]
  add_issue <- function(issue_path, row, field, message) {
    issues <<- rbind(
      issues,
      .tracer_network_issue(issue_path, row, field, message)
    )
  }
  fail <- function() {
    .tracer_collapse_result(FALSE, issues = issues, report = report, path = path)
  }

  if (!inherits(network, "tracer_network") ||
      !is.list(network) || !identical(names(network), c("root", "tables")) ||
      !is.list(network$tables)) {
    add_issue("", NA_integer_, "network", "A valid tracer_network is required.")
    return(fail())
  }
  if (!.tracer_network_valid_path(path, allow_root = FALSE)) {
    add_issue(path, NA_integer_, "path", "Select a valid non-root capsule path.")
    return(fail())
  }
  if (!path %in% names(network$tables)) {
    add_issue(path, NA_integer_, "path", paste0("No capsule table exists at '", path, "'."))
    return(fail())
  }

  structure_issues <- .tracer_network_structure_issues(network)
  if (nrow(structure_issues) > 0L) {
    issues <- rbind(issues, structure_issues)
    return(fail())
  }

  parent_path <- .tracer_network_parent_path(path)
  path_parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  capsule_id <- path_parts[[length(path_parts)]]
  parent <- get_tracer_table(network, parent_path)
  child <- get_tracer_table(network, path)
  parent_validation <- validate_tracer_table(parent)
  child_validation <- validate_tracer_table(child)

  for (table_validation in list(
    list(path = parent_path, result = parent_validation),
    list(path = path, result = child_validation)
  )) {
    if (nrow(table_validation$result$errors) > 0L) {
      issues <- rbind(
        issues,
        .tracer_network_issue_rows(
          table_validation$path,
          table_validation$result$errors
        )
      )
    }
  }
  if (nrow(issues) > 0L) return(fail())

  if (nrow(child_validation$warnings) > 0L) {
    for (index in seq_len(nrow(child_validation$warnings))) {
      warning <- child_validation$warnings[index, ]
      add_issue(
        path,
        warning$row,
        warning$field,
        paste0(
          warning$message,
          " Correct this internal reference before collapsing."
        )
      )
    }
  }

  required_columns <- .tracer_table_required_columns
  parent_optional <- setdiff(names(parent$data), required_columns)
  child_optional <- setdiff(names(child$data), required_columns)
  if (!setequal(parent_optional, child_optional)) {
    add_issue(
      path,
      NA_integer_,
      "columns",
      paste0(
        "The parent and capsule tables use different optional column names. ",
        "Rename the optional columns so both tables use the same five names before collapsing."
      )
    )
  }
  if (nrow(issues) > 0L) return(fail())

  parent_data <- parent$data
  child_data <- child$data[, names(parent_data), drop = FALSE]
  capsule_rows <- which(
    !.tracer_table_is_missing(parent_data$ID) &
      parent_data$ID == capsule_id &
      !is.na(parent_data$Type) &
      parent_data$Type == "capsule"
  )
  if (length(capsule_rows) != 1L) {
    add_issue(
      parent_path,
      NA_integer_,
      "ID",
      paste0("Capsule '", capsule_id, "' must occur exactly once in its parent table.")
    )
    return(fail())
  }
  capsule_row <- capsule_rows[[1L]]

  child_ids <- child_data$ID
  mapped_rows <- which(!.tracer_table_is_missing(child_ids))
  promoted_ids <- if (length(mapped_rows) == 0L) {
    character()
  } else {
    paste0(capsule_id, "_", child_ids[mapped_rows])
  }
  id_map <- stats::setNames(promoted_ids, child_ids[mapped_rows])
  report$promoted_ids <- data.frame(
    old_id = child_ids[mapped_rows],
    new_id = promoted_ids,
    type = child_data$Type[mapped_rows],
    stringsAsFactors = FALSE
  )

  edge_rows <- which(!is.na(child_data$Type) & child_data$Type == "edge")
  vertex_rows <- which(!is.na(child_data$Type) & child_data$Type %in% c("node", "capsule"))
  vertices <- child_data$ID[vertex_rows]
  internal_from <- child_data$From[edge_rows]
  internal_to <- child_data$To[edge_rows]
  starts <- vertices[!vertices %in% internal_to]
  ends <- vertices[!vertices %in% internal_from]

  parent_edge_rows <- which(!is.na(parent_data$Type) & parent_data$Type == "edge")
  incoming_rows <- parent_edge_rows[parent_data$To[parent_edge_rows] == capsule_id]
  outgoing_rows <- parent_edge_rows[parent_data$From[parent_edge_rows] == capsule_id]
  affected_rows <- parent_edge_rows[
    parent_edge_rows %in% union(incoming_rows, outgoing_rows)
  ]
  is_empty <- length(vertex_rows) == 0L && length(edge_rows) == 0L
  pass_incoming_rows <- incoming_rows[
    parent_data$From[incoming_rows] != capsule_id
  ]
  pass_outgoing_rows <- outgoing_rows[
    parent_data$To[outgoing_rows] != capsule_id
  ]

  if (!is_empty && length(incoming_rows) > 0L && length(starts) == 0L) {
    add_issue(
      path,
      NA_integer_,
      "From",
      paste0(
        "Capsule '", path,
        "' has incoming parent edges but no internal starting element. ",
        "Affected edges: ", paste(parent_data$ID[incoming_rows], collapse = ", "), ". ",
        "Add a node or capsule with no incoming internal edge, or remove the incoming connections."
      )
    )
  }
  if (!is_empty && length(outgoing_rows) > 0L && length(ends) == 0L) {
    add_issue(
      path,
      NA_integer_,
      "To",
      paste0(
        "Capsule '", path,
        "' has outgoing parent edges but no internal end element. ",
        "Affected edges: ", paste(parent_data$ID[outgoing_rows], collapse = ", "), ". ",
        "Add a node or capsule with no outgoing internal edge, or remove the outgoing connections."
      )
    )
  }

  dangling_parent <- parent_validation$warnings
  if (nrow(dangling_parent) > 0L) {
    for (index in seq_len(nrow(dangling_parent))) {
      warning <- dangling_parent[index, ]
      reference <- parent_data[[warning$field]][warning$row]
      if (reference %in% id_map) {
        add_issue(
          parent_path,
          warning$row,
          warning$field,
          paste0(
            "Parent edge reference '", reference,
            "' is currently unresolved and would become resolved by a promoted ID. ",
            "Correct or remove this dangling reference before collapsing."
          )
        )
      }
    }
  }
  if (nrow(issues) > 0L) return(fail())

  promoted <- child_data
  if (length(mapped_rows) > 0L) {
    promoted$ID[mapped_rows] <- unname(id_map[child_ids[mapped_rows]])
  }
  for (field in c("From", "To")) {
    rows <- which(!is.na(promoted$Type) & promoted$Type == "edge")
    if (length(rows) > 0L) {
      promoted[[field]][rows] <- unname(id_map[promoted[[field]][rows]])
    }
  }

  generated_edges <- list()
  updated_edges <- list()
  removed_edges <- list()
  metadata_conflicts <- list()
  edge_changes <- list()
  parent_result <- list()
  metadata_columns <- setdiff(names(parent_data), c("ID", "From", "To"))

  add_removed_edge <- function(row) {
    removed_edges[[length(removed_edges) + 1L]] <<- c(
      id = parent_data$ID[row],
      from = parent_data$From[row],
      to = parent_data$To[row]
    )
    edge_changes[[length(edge_changes) + 1L]] <<- c(
      action = "Removed",
      original_edges = parent_data$ID[row],
      resulting_edge = "",
      original_connections = paste0(
        parent_data$ID[row], ": ", parent_data$From[row],
        " -> ", parent_data$To[row]
      ),
      resulting_connection = ""
    )
  }

  append_generated_edge <- function(
    row,
    id,
    from,
    to,
    original_ids,
    original_connections
  ) {
    output <- row
    output$ID <- id
    output$From <- from
    output$To <- to
    parent_result[[length(parent_result) + 1L]] <<- output
    generated_edges[[length(generated_edges) + 1L]] <<- c(
      id = id,
      from = from,
      to = to,
      original_edges = paste(original_ids, collapse = ", ")
    )
    edge_changes[[length(edge_changes) + 1L]] <<- c(
      action = "Generated",
      original_edges = paste(original_ids, collapse = ", "),
      resulting_edge = id,
      original_connections = paste(original_connections, collapse = "; "),
      resulting_connection = paste0(id, ": ", from, " -> ", to)
    )
  }

  combine_metadata <- function(first, second, generated_id, original_ids, field) {
    first_missing <- .tracer_table_is_missing(first)
    second_missing <- .tracer_table_is_missing(second)
    if (first_missing && second_missing) return("")
    if (first_missing) return(second)
    if (second_missing || identical(first, second)) return(first)

    metadata_conflicts[[length(metadata_conflicts) + 1L]] <<- c(
      generated_edge = generated_id,
      original_edges = paste(original_ids, collapse = ", "),
      field = field,
      original_value_1 = first,
      original_value_2 = second,
      result = ""
    )
    ""
  }

  for (row in seq_len(nrow(parent_data))) {
    if (row == capsule_row) {
      if (nrow(promoted) > 0L) {
        parent_result[[length(parent_result) + 1L]] <- promoted
      }
      next
    }
    if (!row %in% affected_rows) {
      parent_result[[length(parent_result) + 1L]] <- parent_data[row, , drop = FALSE]
      next
    }

    old_id <- parent_data$ID[row]
    old_from <- parent_data$From[row]
    old_to <- parent_data$To[row]
    if (is_empty) {
      if (old_from == capsule_id && old_to == capsule_id) {
        add_removed_edge(row)
        next
      }
      add_removed_edge(row)
      next
    }

    from_ids <- if (old_from == capsule_id) unname(id_map[ends]) else old_from
    to_ids <- if (old_to == capsule_id) unname(id_map[starts]) else old_to
    combinations <- data.frame(
      from = rep(from_ids, each = length(to_ids)),
      to = rep(to_ids, times = length(from_ids)),
      stringsAsFactors = FALSE
    )
    if (nrow(combinations) == 1L) {
      output <- parent_data[row, , drop = FALSE]
      output$From <- combinations$from[[1L]]
      output$To <- combinations$to[[1L]]
      parent_result[[length(parent_result) + 1L]] <- output
      updated_edges[[length(updated_edges) + 1L]] <- c(
        id = old_id,
        old_from = old_from,
        old_to = old_to,
        from = output$From,
        to = output$To
      )
      edge_changes[[length(edge_changes) + 1L]] <- c(
        action = "Updated",
        original_edges = old_id,
        resulting_edge = old_id,
        original_connections = paste0(old_id, ": ", old_from, " -> ", old_to),
        resulting_connection = paste0(
          old_id, ": ", output$From, " -> ", output$To
        )
      )
    } else {
      add_removed_edge(row)
      for (index in seq_len(nrow(combinations))) {
        append_generated_edge(
          parent_data[row, , drop = FALSE],
          paste0(old_id, "_", index),
          combinations$from[[index]],
          combinations$to[[index]],
          old_id,
          paste0(old_id, ": ", old_from, " -> ", old_to)
        )
      }
    }
  }

  if (is_empty &&
      length(pass_incoming_rows) > 0L &&
      length(pass_outgoing_rows) > 0L) {
    parent_result <- list()
    generated_edges <- list()
    updated_edges <- list()
    removed_edges <- list()
    metadata_conflicts <- list()
    edge_changes <- list()

    for (row in seq_len(nrow(parent_data))) {
      if (row == capsule_row) next
      if (!row %in% affected_rows) {
        parent_result[[length(parent_result) + 1L]] <- parent_data[row, , drop = FALSE]
      } else {
        add_removed_edge(row)
      }
      if (row %in% pass_incoming_rows) {
        for (outgoing_row in pass_outgoing_rows) {
          generated_id <- paste(
            parent_data$ID[row],
            parent_data$ID[outgoing_row],
            sep = "_"
          )
          from <- parent_data$From[row]
          to <- parent_data$To[outgoing_row]
          output <- parent_data[row, , drop = FALSE]
          output$ID <- generated_id
          output$From <- from
          output$To <- to
          original_ids <- c(parent_data$ID[row], parent_data$ID[outgoing_row])
          for (field in metadata_columns) {
            output[[field]] <- combine_metadata(
              parent_data[[field]][row],
              parent_data[[field]][outgoing_row],
              generated_id,
              original_ids,
              field
            )
          }
          append_generated_edge(
            output,
            generated_id,
            from,
            to,
            original_ids,
            c(
              paste0(
                parent_data$ID[row], ": ", parent_data$From[row],
                " -> ", parent_data$To[row]
              ),
              paste0(
                parent_data$ID[outgoing_row], ": ",
                parent_data$From[outgoing_row], " -> ",
                parent_data$To[outgoing_row]
              )
            )
          )
        }
      }
    }
  }

  if (length(affected_rows) == 0L && is_empty) {
    parent_result <- list(parent_data[-capsule_row, , drop = FALSE])
  }

  parent_result <- if (length(parent_result) == 0L) {
    parent_data[FALSE, , drop = FALSE]
  } else {
    do.call(rbind, parent_result)
  }
  bind_records <- function(records, empty) {
    if (length(records) == 0L) {
      empty
    } else {
      as.data.frame(do.call(rbind, records), stringsAsFactors = FALSE)
    }
  }
  generated_edges <- bind_records(generated_edges, report$generated_edges)
  updated_edges <- bind_records(updated_edges, report$updated_edges)
  removed_edges <- bind_records(removed_edges, report$removed_edges)
  metadata_conflicts <- bind_records(metadata_conflicts, report$metadata_conflicts)
  edge_changes <- bind_records(edge_changes, report$edge_changes)

  candidate_parent <- new_tracer_table(parent_result)
  updated <- network
  if (identical(parent_path, "")) {
    updated$root <- candidate_parent
  } else {
    updated$tables[[parent_path]] <- candidate_parent
  }

  old_descendants <- names(network$tables)[
    names(network$tables) == path |
      startsWith(names(network$tables), paste0(path, "/"))
  ]
  moved_old_paths <- setdiff(old_descendants, path)
  moved_new_paths <- character(length(moved_old_paths))
  for (index in seq_along(moved_old_paths)) {
    relative <- substring(moved_old_paths[[index]], nchar(path) + 2L)
    relative_parts <- strsplit(relative, "/", fixed = TRUE)[[1]]
    new_first_id <- paste0(capsule_id, "_", relative_parts[[1L]])
    new_relative <- paste(
      c(new_first_id, relative_parts[-1L]),
      collapse = "/"
    )
    moved_new_paths[[index]] <- if (identical(parent_path, "")) {
      new_relative
    } else {
      paste(parent_path, new_relative, sep = "/")
    }
    report$updated_paths <- rbind(
      report$updated_paths,
      data.frame(
        old_path = moved_old_paths[[index]],
        new_path = moved_new_paths[[index]],
        stringsAsFactors = FALSE
      )
    )
  }

  unaffected_tables <- updated$tables[!names(updated$tables) %in% old_descendants]
  collision_paths <- intersect(moved_new_paths, names(unaffected_tables))
  if (length(collision_paths) > 0L) {
    for (conflict in collision_paths) {
      add_issue(
        conflict,
        NA_integer_,
        "path",
        paste0("Promoting nested capsules would conflict with existing path '", conflict, "'.")
      )
    }
    return(fail())
  }
  moved_tables <- network$tables[moved_old_paths]
  names(moved_tables) <- moved_new_paths
  updated$tables <- c(unaffected_tables, moved_tables)
  candidate <- structure(
    list(root = updated$root, tables = updated$tables),
    class = "tracer_network"
  )

  candidate_structure <- .tracer_network_structure_issues(candidate)
  candidate_content <- validate_tracer_table(candidate_parent)
  if (nrow(candidate_structure) > 0L) {
    issues <- rbind(issues, candidate_structure)
  }
  if (nrow(candidate_content$errors) > 0L) {
    issues <- rbind(
      issues,
      .tracer_network_issue_rows(parent_path, candidate_content$errors)
    )
  }
  if (nrow(issues) > 0L) return(fail())

  report$generated_edges <- generated_edges
  report$updated_edges <- updated_edges
  report$removed_edges <- removed_edges
  report$metadata_conflicts <- metadata_conflicts
  report$edge_changes <- edge_changes
  report$summary <- list(
    promoted_nodes = sum(report$promoted_ids$type == "node"),
    promoted_internal_edges = sum(report$promoted_ids$type == "edge"),
    promoted_capsules = sum(report$promoted_ids$type == "capsule"),
    generated_edges = nrow(generated_edges),
    updated_edges = nrow(updated_edges),
    removed_edges = nrow(removed_edges),
    metadata_conflicts = nrow(metadata_conflicts)
  )

  .tracer_collapse_result(
    TRUE,
    network = candidate,
    issues = issues,
    report = report,
    path = path
  )
}

#' Rebase checkpoint paths after collapsing a capsule
#'
#' Promoted descendant capsules retain their previous checkpoint paths until
#' the parent table is saved.
#'
#' @param path_map A working-to-saved path map.
#' @param old_path The working path of the collapsed capsule.
#' @param new_network The network after a successful collapse.
#' @return An updated working-to-saved path map.
#' @export
tracer_network_collapse_path_map <- function(path_map, old_path, new_network) {
  if (!is.data.frame(path_map) ||
      !identical(names(path_map), c("working_path", "saved_path"))) {
    stop("'path_map' must have 'working_path' and 'saved_path' columns.", call. = FALSE)
  }

  parts <- strsplit(old_path, "/", fixed = TRUE)[[1]]
  capsule_id <- parts[[length(parts)]]
  parent_path <- .tracer_network_parent_path(old_path)
  affected <- path_map$working_path[
    startsWith(path_map$working_path, paste0(old_path, "/"))
  ]
  moved <- path_map[match(affected, path_map$working_path), , drop = FALSE]
  if (nrow(moved) > 0L) {
    moved$working_path <- vapply(moved$working_path, function(old_child_path) {
      relative <- substring(old_child_path, nchar(old_path) + 2L)
      relative_parts <- strsplit(relative, "/", fixed = TRUE)[[1]]
      new_relative <- paste(
        c(paste0(capsule_id, "_", relative_parts[[1L]]), relative_parts[-1L]),
        collapse = "/"
      )
      .tracer_network_path_join(parent_path, new_relative)
    }, character(1))
  }

  retained <- path_map[
    !path_map$working_path %in% c(old_path, affected),
    ,
    drop = FALSE
  ]
  updated <- rbind(retained, moved)
  paths <- tracer_network_paths(new_network)
  updated <- updated[updated$working_path %in% paths, , drop = FALSE]
  missing_paths <- setdiff(paths, updated$working_path)
  if (length(missing_paths) > 0L) {
    updated <- rbind(
      updated,
      data.frame(
        working_path = missing_paths,
        saved_path = rep(NA_character_, length(missing_paths)),
        stringsAsFactors = FALSE
      )
    )
  }
  updated[match(paths, updated$working_path), , drop = FALSE]
}

.tracer_network_subtree_paths <- function(network, path) {
  paths <- tracer_network_paths(network)
  if (identical(path, "")) {
    paths
  } else {
    paths[paths == path | startsWith(paths, paste0(path, "/"))]
  }
}

.tracer_network_extract_subtree <- function(network, path) {
  paths <- .tracer_network_subtree_paths(network, path)
  if (length(paths) == 0L || !path %in% paths) {
    stop(paste0("No tracer_table exists at path '", path, "'."), call. = FALSE)
  }

  descendant_paths <- setdiff(paths, path)
  relative_paths <- if (identical(path, "")) {
    descendant_paths
  } else {
    substring(descendant_paths, nchar(path) + 2L)
  }
  tables <- lapply(descendant_paths, function(descendant_path) {
    get_tracer_table(network, descendant_path)
  })
  names(tables) <- relative_paths
  structure(
    list(root = get_tracer_table(network, path), tables = tables),
    class = "tracer_network"
  )
}

.tracer_network_replace_subtree <- function(network, path, subtree) {
  if (!inherits(subtree, "tracer_network")) {
    stop("'subtree' must inherit from 'tracer_network'.", call. = FALSE)
  }
  if (!identical(path, "") && !path %in% tracer_network_paths(network)) {
    stop(paste0("No tracer_table exists at path '", path, "'."), call. = FALSE)
  }

  updated <- network
  old_paths <- .tracer_network_subtree_paths(updated, path)
  remove_paths <- setdiff(old_paths, "")
  updated$tables[remove_paths] <- NULL
  if (identical(path, "")) {
    updated$root <- subtree$root
  } else {
    updated$tables[[path]] <- subtree$root
  }
  for (relative_path in names(subtree$tables)) {
    target_path <- if (identical(path, "")) {
      relative_path
    } else {
      paste(path, relative_path, sep = "/")
    }
    updated$tables[[target_path]] <- subtree$tables[[relative_path]]
  }

  result <- structure(list(root = updated$root, tables = updated$tables), class = "tracer_network")
  issues <- .tracer_network_structure_issues(result)
  if (nrow(issues) > 0L) {
    stop(paste(issues$message, collapse = " "), call. = FALSE)
  }
  result
}

.tracer_network_path_join <- function(parent_path, child_id) {
  if (identical(parent_path, "")) child_id else paste(parent_path, child_id, sep = "/")
}

.tracer_network_map_lookup <- function(path_map, working_path) {
  if (!is.data.frame(path_map) ||
      !identical(names(path_map), c("working_path", "saved_path")) ||
      anyNA(path_map$working_path) || anyDuplicated(path_map$working_path)) {
    stop("'path_map' must have 'working_path' and 'saved_path' columns.", call. = FALSE)
  }
  matches <- which(path_map$working_path == working_path)
  if (length(matches) != 1L) {
    stop(paste0("No unique checkpoint path is registered for '", working_path, "'."), call. = FALSE)
  }
  path_map$saved_path[matches]
}

.tracer_network_map_replace_subtree <- function(path_map, working_path, saved_path, subtree, retained_map) {
  remove <- if (identical(working_path, "")) {
    rep(TRUE, nrow(path_map))
  } else {
    path_map$working_path == working_path |
      startsWith(path_map$working_path, paste0(working_path, "/"))
  }
  retained <- path_map[!remove, , drop = FALSE]
  working_paths <- if (identical(working_path, "")) {
    c("", names(subtree$tables))
  } else {
    c(
      working_path,
      if (length(subtree$tables) == 0L) {
        character()
      } else {
        paste(working_path, names(subtree$tables), sep = "/")
      }
    )
  }
  relative_paths <- if (identical(working_path, "")) {
    working_paths
  } else {
    c("", names(subtree$tables))
  }
  saved_paths <- vapply(relative_paths, function(relative_path) {
    if (identical(relative_path, "")) saved_path else .tracer_network_path_join(saved_path, relative_path)
  }, character(1))
  matches <- match(working_paths, retained_map$working_path)
  mapped <- !is.na(matches)
  saved_paths[mapped] <- retained_map$saved_path[matches[mapped]]
  rbind(
    retained,
    data.frame(
      working_path = working_paths,
      saved_path = saved_paths,
      stringsAsFactors = FALSE
    )
  )
}

#' Create a working-to-saved path map
#'
#' The map tracks capsule paths across uncommitted renames. It is session state,
#' not a persistent capsule identity.
#'
#' @param network A `tracer_network`.
#' @param saved_network Optional checkpoint network. Defaults to `network`;
#'   use `NULL` when every descendant is currently unsaved.
#' @return A data frame with `working_path` and `saved_path` columns.
#' @export
tracer_network_path_map <- function(network, saved_network = network) {
  paths <- tracer_network_paths(network)
  saved_paths <- if (is.null(saved_network)) character() else tracer_network_paths(saved_network)
  saved_path <- ifelse(paths %in% saved_paths, paths, NA_character_)
  saved_path[paths == ""] <- ""
  data.frame(
    working_path = paths,
    saved_path = saved_path,
    stringsAsFactors = FALSE
  )
}

#' Compare two TraceR networks
#'
#' Table row names are ignored, as in [tracer_table_data_identical()].
#'
#' @param x,y Objects to compare.
#' @return `TRUE` when both networks contain the same paths and table data.
#' @export
tracer_network_data_identical <- function(x, y) {
  if (!inherits(x, "tracer_network") || !inherits(y, "tracer_network")) {
    return(FALSE)
  }
  x_paths <- tracer_network_paths(x)
  y_paths <- tracer_network_paths(y)
  if (!setequal(x_paths, y_paths) || length(x_paths) != length(y_paths)) {
    return(FALSE)
  }
  all(vapply(x_paths, function(path) {
    tracer_table_data_identical(
      get_tracer_table(x, path),
      get_tracer_table(y, path)
    )
  }, logical(1)))
}

#' Rebase working paths after a capsule rename
#'
#' @param path_map A working-to-saved path map.
#' @param old_path Previous capsule path.
#' @param new_path Renamed capsule path.
#' @return An updated path map.
#' @export
tracer_network_rebase_path_map <- function(path_map, old_path, new_path) {
  if (!is.data.frame(path_map) ||
      !identical(names(path_map), c("working_path", "saved_path"))) {
    stop("'path_map' must have 'working_path' and 'saved_path' columns.", call. = FALSE)
  }
  affected <- path_map$working_path == old_path |
    startsWith(path_map$working_path, paste0(old_path, "/"))
  suffix <- substring(path_map$working_path[affected], nchar(old_path) + 1L)
  path_map$working_path[affected] <- paste0(new_path, suffix)
  path_map
}

#' Register newly added working paths
#'
#' @param path_map A working-to-saved path map.
#' @param network The network after adding a subtree.
#' @param path Root path of the new subtree.
#' @return An updated path map.
#' @export
tracer_network_register_paths <- function(path_map, network, path) {
  paths <- .tracer_network_subtree_paths(network, path)
  additions <- setdiff(paths, path_map$working_path)
  if (length(additions) == 0L) return(path_map)
  rbind(
    path_map,
    data.frame(
      working_path = additions,
      saved_path = rep(NA_character_, length(additions)),
      stringsAsFactors = FALSE
    )
  )
}

#' Remove paths from a working-to-saved path map
#'
#' @param path_map A working-to-saved path map.
#' @param path Root path of the removed subtree.
#' @return An updated path map.
#' @export
tracer_network_drop_paths <- function(path_map, path) {
  affected <- path_map$working_path == path |
    startsWith(path_map$working_path, paste0(path, "/"))
  path_map[!affected, , drop = FALSE]
}

#' Save one table and its structural changes to a network checkpoint
#'
#' Descendant table contents are copied from the checkpoint when their paths
#' are unchanged or renamed. A newly added subtree is saved with the parent.
#' Retained descendants keep pending rename and unsaved-addition mappings;
#' only checkpoint prefixes moved by this save are rebased.
#'
#' @param working_network The current working network.
#' @param saved_network The current checkpoint network.
#' @param path_map A working-to-saved path map.
#' @param path The working path to checkpoint.
#' @return A list with updated `network` and `path_map` components.
#' @export
tracer_network_save_table <- function(working_network, saved_network, path_map, path) {
  validation <- validate_tracer_table(get_tracer_table(working_network, path))
  if (nrow(validation$errors) > 0L) {
    stop(paste(validation$errors$message, collapse = " "), call. = FALSE)
  }

  saved_path <- .tracer_network_map_lookup(path_map, path)
  if (is.na(saved_path)) {
    stop("Save the parent capsule before saving this table.", call. = FALSE)
  }
  working_parent <- get_tracer_table(working_network, path)
  child_tables <- list()
  retained_map <- path_map[FALSE, , drop = FALSE]

  for (capsule_id in list_tracer_capsules(working_network, path)) {
    working_child_path <- .tracer_network_path_join(path, capsule_id)
    child_saved_path <- .tracer_network_map_lookup(path_map, working_child_path)
    child_subtree <- if (is.na(child_saved_path)) {
      .tracer_network_extract_subtree(working_network, working_child_path)
    } else {
      .tracer_network_extract_subtree(saved_network, child_saved_path)
    }
    if (!is.na(child_saved_path)) {
      child_map <- path_map[
        path_map$working_path == working_child_path |
          startsWith(path_map$working_path, paste0(working_child_path, "/")),
        ,
        drop = FALSE
      ]
      moved <- !is.na(child_map$saved_path) &
        (child_map$saved_path == child_saved_path |
           startsWith(child_map$saved_path, paste0(child_saved_path, "/")))
      child_map$saved_path[moved] <- paste0(
        .tracer_network_path_join(saved_path, capsule_id),
        substring(child_map$saved_path[moved], nchar(child_saved_path) + 1L)
      )
      retained_map <- rbind(retained_map, child_map)
    }
    child_tables[[capsule_id]] <- child_subtree$root
    for (relative_path in names(child_subtree$tables)) {
      child_tables[[paste(capsule_id, relative_path, sep = "/")]] <-
        child_subtree$tables[[relative_path]]
    }
  }

  saved_subtree <- structure(
    list(root = working_parent, tables = child_tables),
    class = "tracer_network"
  )
  updated_saved <- .tracer_network_replace_subtree(saved_network, saved_path, saved_subtree)
  updated_map <- .tracer_network_map_replace_subtree(
    path_map,
    path,
    saved_path,
    .tracer_network_extract_subtree(working_network, path),
    retained_map
  )

  list(network = updated_saved, path_map = updated_map)
}

#' Reset one working table from its network checkpoint
#'
#' Restores the selected table's saved structure. Descendant working data is
#' retained across capsule renames; deleted saved subtrees are restored.
#' Retained descendants keep their checkpoint mappings, including pending
#' renames and unsaved additions, while moved working prefixes are rebased.
#'
#' @param working_network The current working network.
#' @param saved_network The current checkpoint network.
#' @param path_map A working-to-saved path map.
#' @param path The working path to reset.
#' @return A list with updated `network` and `path_map` components.
#' @export
tracer_network_reset_table <- function(working_network, saved_network, path_map, path) {
  saved_path <- .tracer_network_map_lookup(path_map, path)
  if (is.na(saved_path)) {
    stop("Reset the parent capsule to remove this unsaved table.", call. = FALSE)
  }

  saved_subtree <- .tracer_network_extract_subtree(saved_network, saved_path)
  child_tables <- list()
  retained_map <- path_map[FALSE, , drop = FALSE]
  for (capsule_id in list_tracer_capsules(saved_network, saved_path)) {
    saved_child_path <- .tracer_network_path_join(saved_path, capsule_id)
    matches <- which(!is.na(path_map$saved_path) & path_map$saved_path == saved_child_path)
    if (length(matches) > 1L) {
      stop("Multiple working paths map to the same saved capsule path.", call. = FALSE)
    }
    child_subtree <- if (length(matches) == 1L) {
      .tracer_network_extract_subtree(working_network, path_map$working_path[matches])
    } else {
      .tracer_network_extract_subtree(saved_network, saved_child_path)
    }
    if (length(matches) == 1L) {
      working_child_path <- path_map$working_path[matches]
      child_map <- path_map[
        path_map$working_path == working_child_path |
          startsWith(path_map$working_path, paste0(working_child_path, "/")),
        ,
        drop = FALSE
      ]
      child_map <- tracer_network_rebase_path_map(
        child_map,
        working_child_path,
        .tracer_network_path_join(path, capsule_id)
      )
      retained_map <- rbind(retained_map, child_map)
    }
    child_tables[[capsule_id]] <- child_subtree$root
    for (relative_path in names(child_subtree$tables)) {
      child_tables[[paste(capsule_id, relative_path, sep = "/")]] <-
        child_subtree$tables[[relative_path]]
    }
  }

  reset_subtree <- structure(
    list(root = saved_subtree$root, tables = child_tables),
    class = "tracer_network"
  )
  updated_working <- .tracer_network_replace_subtree(working_network, path, reset_subtree)
  updated_map <- .tracer_network_map_replace_subtree(
    path_map, path, saved_path, reset_subtree, retained_map
  )
  list(network = updated_working, path_map = updated_map)
}
