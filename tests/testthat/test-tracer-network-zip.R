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
