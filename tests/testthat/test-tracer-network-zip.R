make_zip_test_table <- function(id = NULL, type = NULL, label = NULL) {
  data <- new_tracer_table()$data
  if (!is.null(id)) {
    data[1, ] <- ""
    data$ID[1] <- id
    data$Label[1] <- if (is.null(label)) id else label
    data$Type[1] <- type
  }
  new_tracer_table(data)
}

make_zip_test_network <- function() {
  network <- new_tracer_network()
  network <- add_tracer_capsule(network, "", "Top")
  network <- add_tracer_capsule(network, "Top", "Nested")
  leaf <- make_zip_test_table("N1", "node", "Unicode café")
  set_tracer_table(network, "Top/Nested", leaf)
}

make_zip_test_wide_network <- function(capsule_count) {
  ids <- sprintf("C%03d", seq_len(capsule_count))
  root_data <- new_tracer_table()$data
  root_data <- root_data[rep(NA_integer_, capsule_count), , drop = FALSE]
  root_data[] <- ""
  root_data$ID <- ids
  root_data$Type <- "capsule"
  tables <- stats::setNames(
    rep(list(new_tracer_table()), capsule_count),
    ids
  )
  new_tracer_network(new_tracer_table(root_data), tables)
}

make_zip_test_deep_network <- function(depth) {
  ids <- sprintf("C%03d", seq_len(depth))
  root <- make_zip_test_table(ids[[1L]], "capsule")
  tables <- lapply(seq_len(depth), function(index) {
    if (index == depth) {
      new_tracer_table()
    } else {
      make_zip_test_table(ids[[index + 1L]], "capsule")
    }
  })
  paths <- vapply(seq_len(depth), function(index) {
    paste(ids[seq_len(index)], collapse = "/")
  }, character(1))
  names(tables) <- paths
  new_tracer_network(root, tables)
}

make_zip_entry_fixture <- function(filenames, types = rep("file", length(filenames))) {
  archive <- tempfile(fileext = ".zip")
  withr::defer(unlink(archive), envir = parent.frame())
  uint <- function(values, size) {
    as.raw(unlist(lapply(values, function(value) {
      floor(value / 256^(seq_len(size) - 1L)) %% 256
    })))
  }
  modes <- c(file = "100644", directory = "040755", symlink = "120777", FIFO = "010644")
  local_entries <- raw()
  directory <- raw()

  # Empty stored entries allow malformed metadata without filesystem-specific
  # symlink support or ZIP tools that normalize unsafe and duplicate paths.
  for (index in seq_along(filenames)) {
    filename <- charToRaw(enc2utf8(filenames[index]))
    offset <- length(local_entries)
    local_entries <- c(
      local_entries,
      uint(0x04034b50, 4L),
      uint(c(20, 0, 0, 0, 0), 2L),
      uint(c(0, 0, 0), 4L),
      uint(c(length(filename), 0), 2L),
      filename
    )
    directory <- c(
      directory,
      uint(0x02014b50, 4L),
      uint(c(0x0314, 20, 0, 0, 0, 0), 2L),
      uint(c(0, 0, 0), 4L),
      uint(c(length(filename), 0, 0, 0, 0), 2L),
      uint(strtoi(modes[[types[index]]], base = 8L) * 65536, 4L),
      uint(offset, 4L),
      filename
    )
  }
  writeBin(c(
    local_entries,
    directory,
    uint(0x06054b50, 4L),
    uint(c(0, 0, length(filenames), length(filenames)), 2L),
    uint(c(length(directory), length(local_entries)), 4L),
    uint(0, 2L)
  ), archive)
  archive
}

test_that("whole-network ZIP export and import preserve nested tables", {
  network <- make_zip_test_network()
  archive <- tempfile(fileext = ".zip")

  expect_identical(tracer_network_to_zip(network, archive), archive)
  expect_true(file.exists(archive))
  entries <- zip::zip_list(archive)
  expect_true("network/data.json" %in% entries$filename)
  file_entries <- entries[entries$type == "file", ]
  expect_gt(nrow(file_entries), 0L)
  expect_equal(file_entries$compressed_size, file_entries$uncompressed_size)

  imported <- tracer_network_from_zip(archive)
  expect_identical(imported$root, network$root)
  expect_setequal(names(imported$tables), names(network$tables))
  for (path in names(network$tables)) {
    expect_identical(get_tracer_table(imported, path), get_tracer_table(network, path))
  }
  expect_identical(validate_tracer_network(imported)$errors$path, character())
})

test_that("whole-network ZIP export rejects networks beyond app import limits", {
  archive <- tempfile(fileext = ".zip")

  expect_error(
    tracer_network_to_zip(make_zip_test_wide_network(500L), archive),
    "1,002 ZIP entries"
  )
  expect_false(file.exists(archive))

  expect_error(
    tracer_network_to_zip(make_zip_test_deep_network(65L), archive),
    "nesting depth 65"
  )
  expect_false(file.exists(archive))

  oversized_table <- make_zip_test_table(
    "N1",
    "node",
    strrep("x", 10 * 1024^2)
  )
  expect_error(
    tracer_network_to_zip(new_tracer_network(oversized_table), archive),
    "exceeding the app import limit of 10,485,760 bytes"
  )
  expect_false(file.exists(archive))
})

test_that("whole-network ZIP import enforces configured security limits", {
  archive <- tempfile(fileext = ".zip")
  tracer_network_to_zip(make_zip_test_network(), archive)

  expect_error(tracer_network_from_zip(archive, max_entries = 1L), "entry limit")
  expect_error(tracer_network_from_zip(archive, max_total_uncompressed = 1), "size limits")
  expect_error(tracer_network_from_zip(archive, max_archive_size = 1), "archive size limit")
  expect_error(tracer_network_from_zip(archive, max_table_size = 1), "size limits")
  expect_no_error(tracer_network_from_zip(archive, max_compression_ratio = 1))
  expect_error(tracer_network_from_zip(archive, max_depth = 1L), "nesting limit")
  expect_error(tracer_network_from_zip(archive, max_total_uncompressed = -1), "finite non-negative")
  expect_error(.tracer_zip_path("../data.json", FALSE), "empty, '.' or '..'")
  expect_error(.tracer_zip_path("Top\\\\data.json", FALSE), "absolute or invalid")
})

test_that("ZIP import rejects symlink and unsupported entries before extraction", {
  extraction_calls <- 0L
  testthat::local_mocked_bindings(
    unzip = function(...) {
      extraction_calls <<- extraction_calls + 1L
      stop("Unexpected ZIP extraction.")
    },
    .package = "zip"
  )

  for (type in c("symlink", "FIFO")) {
    archive <- make_zip_entry_fixture(
      c("data.json", "Top/data.json"), c("file", type)
    )
    entries <- zip::zip_list(archive)
    expect_identical(entries$filename, c("data.json", "Top/data.json"))
    expect_identical(entries$type, c("file", type))

    expect_error(tracer_network_from_zip(archive), "symlink or unsupported entry type")
  }
  expect_identical(extraction_calls, 0L)
})

test_that("ZIP import rejects duplicate and case-colliding entries before extraction", {
  extraction_calls <- 0L
  testthat::local_mocked_bindings(
    unzip = function(...) {
      extraction_calls <<- extraction_calls + 1L
      stop("Unexpected ZIP extraction.")
    },
    .package = "zip"
  )
  fixtures <- list(
    list(filenames = c("data.json", "data.json"), types = c("file", "file")),
    list(filenames = c("data.json", "DATA.json"), types = c("file", "file")),
    list(filenames = c("data.json", "Top/", "top/"), types = c("file", "directory", "directory"))
  )
  for (fixture in fixtures) {
    archive <- make_zip_entry_fixture(fixture$filenames, fixture$types)
    entries <- zip::zip_list(archive)
    expect_identical(entries$filename, fixture$filenames)
    expect_identical(entries$type, fixture$types)

    expect_error(tracer_network_from_zip(archive), "duplicate or case-colliding entry paths")
  }
  expect_identical(extraction_calls, 0L)
})

test_that("ZIP import rejects traversal and absolute paths before extraction", {
  extraction_calls <- 0L
  testthat::local_mocked_bindings(
    unzip = function(...) {
      extraction_calls <<- extraction_calls + 1L
      stop("Unexpected ZIP extraction.")
    },
    .package = "zip"
  )
  unsafe_paths <- c("../data.json", "network/../../data.json", "/data.json", "C:/data.json", "Top\\data.json")
  for (path in unsafe_paths) {
    archive <- make_zip_entry_fixture(c("data.json", path))
    entries <- zip::zip_list(archive)
    expect_identical(entries$filename, c("data.json", path))
    expect_identical(entries$type, c("file", "file"))
    message <- if (grepl("..", path, fixed = TRUE)) {
      "empty, '.' or '..'"
    } else {
      "absolute or invalid entry path"
    }

    expect_error(tracer_network_from_zip(archive), message, fixed = TRUE)
  }
  expect_identical(extraction_calls, 0L)
})

test_that("a network can be imported as a nested capsule", {
  destination <- add_tracer_capsule(new_tracer_network(), "", "Existing")
  imported <- make_zip_test_network()

  combined <- import_tracer_network_as_capsule(
    destination,
    "Existing",
    "Imported",
    imported,
    parent_table = get_tracer_table(destination, "Existing")
  )

  expect_true("Existing/Imported" %in% names(combined$tables))
  expect_true("Existing/Imported/Top" %in% names(combined$tables))
  expect_true("Existing/Imported/Top/Nested" %in% names(combined$tables))
  expect_identical(
    get_tracer_table(combined, "Existing/Imported/Top/Nested")$data$Label[1],
    "Unicode café"
  )
  expect_identical(validate_tracer_network(combined)$errors$path, character())
})
