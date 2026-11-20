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
  if (new_id != old_id &&
      (new_id %in% current_parent$data$ID ||
       tolower(new_id) %in% tolower(current_parent$data$ID[!is.na(current_parent$data$ID)]))) {
    stop(paste0("ID '", new_id, "' already exists in the parent table."), call. = FALSE)
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

.tracer_network_map_replace_subtree <- function(path_map, working_path, saved_path, subtree) {
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

  for (capsule_id in list_tracer_capsules(working_network, path)) {
    working_child_path <- .tracer_network_path_join(path, capsule_id)
    child_saved_path <- .tracer_network_map_lookup(path_map, working_child_path)
    child_subtree <- if (is.na(child_saved_path)) {
      .tracer_network_extract_subtree(working_network, working_child_path)
    } else {
      .tracer_network_extract_subtree(saved_network, child_saved_path)
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
    .tracer_network_extract_subtree(working_network, path)
  )

  list(network = updated_saved, path_map = updated_map)
}

#' Reset one working table from its network checkpoint
#'
#' Restores the selected table's saved structure. Descendant working data is
#' retained across capsule renames; deleted saved subtrees are restored.
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
  updated_map <- .tracer_network_map_replace_subtree(path_map, path, saved_path, reset_subtree)
  list(network = updated_working, path_map = updated_map)
}
