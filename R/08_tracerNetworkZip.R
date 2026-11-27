.tracer_zip_component_error <- function(component) {
  if (!nzchar(component) || component %in% c(".", "..")) {
    return("ZIP paths cannot contain empty, '.' or '..' components.")
  }
  if (grepl("[/:*?\"<>|]", component) || grepl("\\\\", component)) {
    return("ZIP paths contain a filesystem separator or reserved filename character.")
  }
  if (grepl("[[:cntrl:]]", component) || grepl("[. ]$", component)) {
    return("ZIP paths cannot contain control characters or end in a dot or space.")
  }
  if (nchar(enc2utf8(component), type = "bytes") > 255L) {
    return("ZIP path components cannot exceed 255 UTF-8 bytes.")
  }
  device_name <- sub("\\..*$", "", component)
  if (grepl("^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])$", device_name, ignore.case = TRUE)) {
    return("ZIP paths cannot use reserved Windows device names.")
  }
  NULL
}

.tracer_zip_path <- function(path, is_directory) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || startsWith(path, "/") || grepl("\\\\", path) ||
      grepl("//", path, fixed = TRUE) || grepl("^[A-Za-z]:", path) ||
      grepl("[[:cntrl:]]", path)) {
    stop("ZIP contains an absolute or invalid entry path.", call. = FALSE)
  }
  path <- enc2utf8(path)
  if (is_directory) {
    path <- sub("/+$", "", path)
  } else if (endsWith(path, "/")) {
    stop("ZIP file entries cannot end in '/'.", call. = FALSE)
  }
  components <- strsplit(path, "/", fixed = TRUE)[[1]]
  for (component in components) {
    component_error <- .tracer_zip_component_error(component)
    if (!is.null(component_error)) stop(component_error, call. = FALSE)
  }
  path
}

.tracer_network_zip_capsule_path_valid <- function(path) {
  if (identical(path, "")) return(TRUE)
  components <- strsplit(path, "/", fixed = TRUE)[[1]]
  all(vapply(components, function(component) {
    is.null(.tracer_table_id_error(component)) &&
      is.null(.tracer_capsule_id_error(component))
  }, logical(1)))
}

.tracer_network_zip_read_table <- function(
  zipfile,
  entry,
  expected_size,
  max_table_size,
  max_total_remaining,
  index
) {
  destination <- tempfile(paste0("tracer-network-entry-", index, "-"))
  if (!dir.create(destination)) {
    stop("Could not create a temporary directory for ZIP import.", call. = FALSE)
  }
  on.exit(unlink(destination, recursive = TRUE, force = TRUE), add = TRUE)

  zip::unzip(
    zipfile,
    files = entry,
    overwrite = FALSE,
    junkpaths = TRUE,
    exdir = destination
  )
  extracted_file <- file.path(destination, basename(entry))
  if (!file.exists(extracted_file) || dir.exists(extracted_file)) {
    stop(paste0("Could not extract ZIP entry '", entry, "'."), call. = FALSE)
  }
  size <- file.info(extracted_file)$size
  if (is.na(size) || size > max_table_size || size > max_total_remaining) {
    stop("Extracted ZIP content exceeds the configured size limits.", call. = FALSE)
  }
  if (size != expected_size) {
    stop("Extracted ZIP entry size does not match its directory metadata.", call. = FALSE)
  }

  connection <- file(extracted_file, open = "rb")
  on.exit(close(connection), add = TRUE)
  json <- readBin(connection, what = "raw", n = as.integer(size))
  list(table = tracer_table_from_json(json), size = size)
}

#' Export a TraceR network as a ZIP archive
#'
#' The archive contains `network/data.json` and one `data.json` for each
#' capsule directory. Every table uses the existing versioned table JSON
#' format.
#'
#' @param network A `tracer_network`.
#' @param file Destination ZIP path.
#' @return `file`, invisibly.
#' @export
tracer_network_to_zip <- function(network, file) {
  validation <- validate_tracer_network(network)
  if (nrow(validation$errors) > 0L) {
    stop(paste(validation$errors$message, collapse = "\n"), call. = FALSE)
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("'file' must be one non-empty path.", call. = FALSE)
  }

  destination_directory <- normalizePath(dirname(file), winslash = "/", mustWork = TRUE)
  destination <- file.path(destination_directory, basename(file))
  temporary_root <- tempfile("tracer-network-export-")
  if (!dir.create(temporary_root)) {
    stop("Could not create a temporary directory for ZIP export.", call. = FALSE)
  }
  on.exit(unlink(temporary_root, recursive = TRUE, force = TRUE), add = TRUE)

  network_directory <- file.path(temporary_root, "network")
  if (!dir.create(network_directory)) {
    stop("Could not create the temporary network directory.", call. = FALSE)
  }

  for (path in tracer_network_paths(network)) {
    table <- get_tracer_table(network, path)
    table_directory <- if (identical(path, "")) {
      network_directory
    } else {
      file.path(network_directory, path)
    }
    if (!dir.exists(table_directory) &&
        !dir.create(table_directory, recursive = TRUE, showWarnings = FALSE)) {
      stop(paste0("Could not create directory for capsule path '", path, "'."), call. = FALSE)
    }
    writeBin(
      charToRaw(enc2utf8(tracer_table_to_json(table))),
      file.path(table_directory, "data.json")
    )
  }

  zip::zipr(
    zipfile = destination,
    files = "network",
    compression_level = 0,
    root = temporary_root,
    recurse = TRUE,
    include_directories = TRUE,
    mode = "cherry-pick"
  )
  invisible(file)
}

#' Import a TraceR network from a ZIP archive
#'
#' Archive paths and entry types are checked before extraction. Files are
#' extracted one at a time with paths discarded, after which the reconstructed
#' hierarchy is validated.
#'
#' @param file ZIP archive path.
#' @param max_entries Maximum number of files and directory entries.
#' @param max_total_uncompressed Maximum total uncompressed file size in bytes.
#' @param max_archive_size Maximum ZIP archive size in bytes.
#' @param max_table_size Maximum size of one `data.json`, in bytes.
#' @param max_compression_ratio Maximum uncompressed-to-compressed size ratio.
#' @param max_depth Maximum capsule nesting depth.
#' @details Defaults limit archives to 1,000 entries, 100 MiB of uncompressed
#'   files, 110 MiB of ZIP data, 10 MiB per table, a 100:1 compression ratio,
#'   and 64 capsule levels.
#' @return A validated `tracer_network`.
#' @export
tracer_network_from_zip <- function(
  file,
  max_entries = 1000L,
  max_total_uncompressed = 100 * 1024^2,
  max_archive_size = 110 * 1024^2,
  max_table_size = 10 * 1024^2,
  max_compression_ratio = 100,
  max_depth = 64L
) {
  limits <- list(
    max_entries = max_entries,
    max_total_uncompressed = max_total_uncompressed,
    max_archive_size = max_archive_size,
    max_table_size = max_table_size,
    max_compression_ratio = max_compression_ratio,
    max_depth = max_depth
  )
  valid_limits <- vapply(limits, function(limit) {
    is.numeric(limit) && !is.complex(limit) && length(limit) == 1L && !is.na(limit) &&
      is.finite(limit) && limit >= 0
  }, logical(1))
  if (!all(valid_limits)) {
    stop("ZIP import limits must be finite non-negative scalars with valid integer limits.", call. = FALSE)
  }
  integer_limits_valid <- max_entries <= .Machine$integer.max &&
    max_depth <= .Machine$integer.max
  if (max_entries < 1 || max_table_size < 1 ||
      max_compression_ratio < 1 || max_depth < 0 ||
      !integer_limits_valid ||
      max_entries != as.integer(max_entries) || max_depth != as.integer(max_depth)) {
    stop("ZIP import limits must be finite non-negative scalars with valid integer limits.", call. = FALSE)
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) ||
      !nzchar(file) || !file.exists(file) || dir.exists(file)) {
    stop("'file' must identify an existing ZIP archive.", call. = FALSE)
  }
  archive_size <- file.info(file)$size
  if (is.na(archive_size) || archive_size > max_archive_size) {
    stop("ZIP archive exceeds the configured archive size limit.", call. = FALSE)
  }

  entries <- zip::zip_list(file)
  if (nrow(entries) == 0L || nrow(entries) > max_entries) {
    stop("ZIP archive has no entries or exceeds the configured entry limit.", call. = FALSE)
  }
  if (anyNA(entries$type) || any(!entries$type %in% c("file", "directory"))) {
    stop("ZIP contains a symlink or unsupported entry type.", call. = FALSE)
  }
  is_directory <- entries$type == "directory" | endsWith(entries$filename, "/")
  is_file <- entries$type == "file" & !is_directory

  paths <- vapply(seq_len(nrow(entries)), function(index) {
    .tracer_zip_path(entries$filename[index], is_directory[index])
  }, character(1))
  path_keys <- tolower(paths)
  if (anyDuplicated(path_keys)) {
    stop("ZIP contains duplicate or case-colliding entry paths.", call. = FALSE)
  }

  file_rows <- which(is_file)
  sizes <- entries$uncompressed_size[file_rows]
  compressed_sizes <- entries$compressed_size[file_rows]
  if (anyNA(sizes) || anyNA(compressed_sizes) || any(sizes < 0) ||
      any(compressed_sizes < 0) || sum(sizes) > max_total_uncompressed ||
      any(sizes > max_table_size)) {
    stop("ZIP exceeds the configured uncompressed size limits.", call. = FALSE)
  }
  ratios <- ifelse(sizes == 0, 0, sizes / pmax(compressed_sizes, 1))
  if (any(ratios > max_compression_ratio)) {
    stop("ZIP exceeds the configured compression ratio limit.", call. = FALSE)
  }

  file_paths <- paths[file_rows]
  if (any(basename(file_paths) != "data.json")) {
    stop("ZIP may contain only data.json files.", call. = FALSE)
  }

  wrapper <- ""
  if (!"data.json" %in% file_paths) {
    first_components <- vapply(strsplit(file_paths, "/", fixed = TRUE), `[[`, character(1), 1L)
    if (length(unique(tolower(first_components))) != 1L ||
        any(!startsWith(file_paths, paste0(first_components[[1L]], "/")))) {
      stop("ZIP must contain a root data.json file.", call. = FALSE)
    }
    wrapper <- first_components[[1L]]
    if (!is.null(.tracer_zip_component_error(wrapper))) {
      stop("ZIP wrapper directory name is unsafe.", call. = FALSE)
    }
    if (any(paths != wrapper & !startsWith(paths, paste0(wrapper, "/")))) {
      stop("ZIP contains entries outside its wrapper directory.", call. = FALSE)
    }
    paths <- vapply(paths, function(path) {
      if (identical(path, wrapper)) "" else substring(path, nchar(wrapper) + 2L)
    }, character(1))
    file_paths <- paths[file_rows]
  }
  if (!"data.json" %in% file_paths) {
    stop("ZIP must contain a root data.json file.", call. = FALSE)
  }

  directory_rows <- which(is_directory)
  directory_paths <- paths[directory_rows]
  directory_paths <- directory_paths[nzchar(directory_paths)]
  expected_directories <- unique(unlist(lapply(file_paths, function(path) {
    components <- strsplit(path, "/", fixed = TRUE)[[1]]
    if (length(components) <= 1L) return(character())
    vapply(seq_len(length(components) - 1L), function(index) {
      paste(components[seq_len(index)], collapse = "/")
    }, character(1))
  }), use.names = FALSE))
  if (any(!directory_paths %in% expected_directories)) {
    stop("ZIP contains an unexpected or empty directory entry.", call. = FALSE)
  }

  table_paths <- vapply(file_paths, function(path) {
    components <- strsplit(path, "/", fixed = TRUE)[[1]]
    if (length(components) == 1L) "" else paste(components[-length(components)], collapse = "/")
  }, character(1))
  depths <- lengths(strsplit(table_paths[nzchar(table_paths)], "/", fixed = TRUE))
  if (length(depths) > 0L && any(depths > max_depth)) {
    stop("ZIP exceeds the configured capsule nesting limit.", call. = FALSE)
  }
  if (any(!vapply(table_paths, .tracer_network_zip_capsule_path_valid, logical(1)))) {
    stop("ZIP contains an invalid capsule path.", call. = FALSE)
  }
  if (anyDuplicated(tolower(table_paths))) {
    stop("ZIP contains duplicate capsule table paths.", call. = FALSE)
  }

  tables <- vector("list", length(file_rows))
  total_extracted <- 0
  for (index in seq_along(file_rows)) {
    extracted <- .tracer_network_zip_read_table(
      file,
      entries$filename[file_rows[index]],
      sizes[index],
      max_table_size,
      max_total_uncompressed - total_extracted,
      index
    )
    tables[[index]] <- extracted$table
    total_extracted <- total_extracted + extracted$size
  }
  names(tables) <- table_paths
  root_index <- match("", table_paths)
  if (is.na(root_index)) {
    stop("ZIP must contain a root data.json file.", call. = FALSE)
  }
  root_table <- tables[[root_index]]
  tables <- tables[-root_index]
  network <- new_tracer_network(root_table, tables)
  validation <- validate_tracer_network(network)
  if (nrow(validation$errors) > 0L) {
    stop(paste(validation$errors$message, collapse = "\n"), call. = FALSE)
  }
  network
}

#' Import a network beneath a new capsule
#'
#' @param network Existing `tracer_network` to update.
#' @param parent_path Parent capsule path, or `""` for the root.
#' @param capsule_id ID for the imported network's new capsule.
#' @param imported_network Valid network to attach.
#' @param parent_table Optional current parent-table value to preserve other
#'   unsaved edits.
#' @return An updated `tracer_network`.
#' @export
import_tracer_network_as_capsule <- function(
  network,
  parent_path,
  capsule_id,
  imported_network,
  parent_table = NULL
) {
  validation <- validate_tracer_network(imported_network)
  if (nrow(validation$errors) > 0L) {
    stop(paste(validation$errors$message, collapse = "\n"), call. = FALSE)
  }

  imported_paths <- tracer_network_paths(imported_network)
  updated <- add_tracer_capsule(
    network,
    parent_path,
    capsule_id,
    parent_table = parent_table,
    child_table = new_tracer_table()
  )
  child_path <- .tracer_network_path_join(parent_path, capsule_id)
  updated$tables[[child_path]] <- get_tracer_table(imported_network, "")
  for (path in setdiff(imported_paths, "")) {
    target_path <- paste(child_path, path, sep = "/")
    updated$tables[[target_path]] <- get_tracer_table(imported_network, path)
  }
  new_tracer_network(updated$root, updated$tables)
}
