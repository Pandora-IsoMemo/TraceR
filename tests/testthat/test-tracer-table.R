make_test_table_data <- function(ids, types, from = rep(NA_character_, length(ids)), to = rep(NA_character_, length(ids))) {
  column_names <- c("ID", "Label", "Type", "From", "To", "Unique identifier", paste0("Optional ", 1:5))
  data <- as.data.frame(
    replicate(length(column_names), rep(NA_character_, length(ids)), simplify = FALSE),
    check.names = FALSE
  )
  names(data) <- column_names
  data$ID <- ids
  data$Label <- rep("label", length(ids))
  data$Type <- types
  data$From <- from
  data$To <- to
  data
}

test_that("new_tracer_table creates the default empty schema", {
  table <- new_tracer_table()

  expect_s3_class(table, "tracer_table")
  expect_identical(names(table), "data")
  expect_s3_class(table$data, "data.frame")
  expect_equal(nrow(table$data), 0L)
  expect_equal(
    names(table$data),
    c("ID", "Label", "Type", "From", "To", "Unique identifier", paste0("Optional ", 1:5))
  )
  expect_true(all(vapply(table$data, is.character, logical(1))))
  expect_equal(nrow(validate_tracer_table(table)$errors), 0L)
})

test_that("new_tracer_table accepts structurally valid data and preserves column order", {
  data <- new_tracer_table()$data
  data <- data[c("Optional 1", "ID", "Label", "Type", "From", "To", "Unique identifier", paste0("Optional ", 2:5))]
  data[1, ] <- NA_character_
  data$ID[1] <- "node-1"

  table <- new_tracer_table(data)

  expect_s3_class(table, "tracer_table")
  expect_identical(names(table$data), names(data))
  expect_identical(table$data$ID, "node-1")
})

test_that("new_tracer_table rejects malformed structure", {
  data <- new_tracer_table()$data

  expect_error(new_tracer_table(data[-1]), "Required column 'ID' is missing")

  data$ID <- integer()
  expect_error(new_tracer_table(data), "Column 'ID' must be character")
})

test_that("structural validation returns structured issues", {
  invalid <- structure(list(data = data.frame(ID = integer())), class = "tracer_table")

  result <- validate_tracer_table(invalid)

  expect_named(result, c("errors", "warnings"))
  expect_named(result$errors, c("row", "field", "message"))
  expect_gt(nrow(result$errors), 0L)
  expect_equal(nrow(result$warnings), 0L)
  expect_true(all(is.na(result$errors$row)))

  missing_class <- validate_tracer_table(list(data = new_tracer_table()$data))
  expect_true(any(missing_class$errors$field == "class"))
})

test_that("structure validation rejects malformed columns and object components", {
  data <- new_tracer_table()$data

  missing_optional <- data[-11]
  expect_error(new_tracer_table(missing_optional), "Exactly five optional columns")

  extra_optional <- data
  extra_optional[[12]] <- character()
  names(extra_optional)[12] <- "Optional 6"
  expect_error(new_tracer_table(extra_optional), "Exactly five optional columns")

  duplicate_names <- data
  names(duplicate_names)[11] <- "ID"
  expect_error(new_tracer_table(duplicate_names), "Column names must be unique")

  blank_name <- data
  names(blank_name)[11] <- "  "
  expect_error(new_tracer_table(blank_name), "Column names must be non-empty")

  optional_conflict <- data
  names(optional_conflict)[7] <- "ID"
  expect_error(new_tracer_table(optional_conflict), "Column names must be unique")

  renamed_required <- data
  names(renamed_required)[1] <- "Identifier"
  expect_error(new_tracer_table(renamed_required), "Required column 'ID' is missing")

  extra_component <- structure(list(data = data, metadata = list()), class = "tracer_table")
  expect_gt(nrow(validate_tracer_table(extra_component)$errors), 0L)
})

test_that("optional column names may change and missing optional values are valid", {
  data <- make_test_table_data("node-1", "node")
  names(data)[7:11] <- c("Author", "Year", "Description", "Source", "Notes")
  data$Year <- NA_character_

  table <- new_tracer_table(data)
  result <- validate_tracer_table(table)

  expect_identical(names(table$data)[7:11], c("Author", "Year", "Description", "Source", "Notes"))
  expect_equal(length(table), 1L)
  expect_equal(nrow(result$errors), 0L)
  expect_equal(nrow(result$warnings), 0L)
})

test_that("blank rows are allowed, while populated rows require IDs and types", {
  data <- make_test_table_data(NA_character_, NA_character_)
  data[] <- lapply(data, function(column) NA_character_)
  blank_result <- validate_tracer_table(new_tracer_table(data))
  expect_equal(nrow(blank_result$errors), 0L)

  data$Label <- "in progress"
  result <- validate_tracer_table(new_tracer_table(data))
  expect_true(any(result$errors$row == 1L & result$errors$field == "ID"))
  expect_true(any(result$errors$row == 1L & result$errors$field == "Type"))
})

test_that("IDs must be populated and unique but have no extra naming restrictions", {
  data <- make_test_table_data(
    ids = c("same", "same", NA_character_, "", "  ", "path/with.dots_and_underscores"),
    types = rep("node", 6)
  )
  data$Label[3:5] <- "populated"

  result <- validate_tracer_table(new_tracer_table(data))

  expect_equal(sum(result$errors$field == "ID" & result$errors$row %in% 1:2), 2L)
  expect_equal(sum(result$errors$field == "ID" & result$errors$row %in% 3:5), 3L)
  expect_false(any(result$errors$row == 6L))
})

test_that("supported types validate and unsupported or missing types are errors", {
  valid_types <- make_test_table_data(
    ids = c("node-1", "node-2", "capsule-1", "capsule-2", "node-edge", "capsule-edge", "nested-edge"),
    types = c("node", "node", "capsule", "capsule", "edge", "edge", "edge"),
    from = c(NA_character_, NA_character_, NA_character_, NA_character_, "node-1", "node-1", "capsule-1"),
    to = c(NA_character_, NA_character_, NA_character_, NA_character_, "node-2", "capsule-1", "capsule-2")
  )
  expect_equal(nrow(validate_tracer_table(new_tracer_table(valid_types))$errors), 0L)

  invalid_types <- make_test_table_data(
    ids = c("bad", "missing"),
    types = c("workflow", NA_character_)
  )
  result <- validate_tracer_table(new_tracer_table(invalid_types))

  expect_true(any(grepl("Invalid Type 'workflow'", result$errors$message, fixed = TRUE)))
  expect_true(any(result$errors$row == 2L & result$errors$field == "Type"))
})

test_that("edge endpoints are required and unknown references are warnings", {
  data <- make_test_table_data(
    ids = c("node", "missing-from", "missing-to", "unknown"),
    types = c("node", "edge", "edge", "edge"),
    from = c(NA_character_, NA_character_, "node", "absent-from"),
    to = c(NA_character_, "node", NA_character_, "absent-to")
  )
  result <- validate_tracer_table(new_tracer_table(data))

  expect_true(any(result$errors$row == 2L & result$errors$field == "From"))
  expect_true(any(result$errors$row == 3L & result$errors$field == "To"))
  expect_setequal(result$warnings$field, c("From", "To"))
  expect_setequal(result$warnings$row, c(4L, 4L))
})

test_that("edge references to other edges are errors", {
  data <- make_test_table_data(
    ids = c("node", "edge-1", "edge-2"),
    types = c("node", "edge", "edge"),
    from = c(NA_character_, "node", "node"),
    to = c(NA_character_, "edge-2", "node")
  )
  result <- validate_tracer_table(new_tracer_table(data))

  expect_true(any(result$errors$row == 2L & result$errors$field == "To"))
  expect_true(any(grepl("references edge ID 'edge-2'", result$errors$message, fixed = TRUE)))
  expect_equal(nrow(result$warnings), 0L)
})

test_that("references to rows without a valid node or capsule type are warnings", {
  data <- make_test_table_data(
    ids = c("incomplete", "edge"),
    types = c(NA_character_, "edge"),
    from = c(NA_character_, "incomplete"),
    to = c(NA_character_, "incomplete")
  )
  result <- validate_tracer_table(new_tracer_table(data))

  expect_true(any(result$errors$row == 1L & result$errors$field == "Type"))
  expect_equal(nrow(result$warnings), 2L)
  expect_setequal(result$warnings$field, c("From", "To"))
})

test_that("validation does not modify the input and non-edge references are ignored", {
  data <- make_test_table_data("node", "node")
  data$From <- "unused-reference"
  table <- new_tracer_table(data)
  original <- table

  result <- validate_tracer_table(table)

  expect_identical(table, original)
  expect_equal(nrow(result$errors), 0L)
  expect_equal(nrow(result$warnings), 0L)
})

test_that("unknown references remain warnings alongside other content errors", {
  data <- make_test_table_data(
    ids = c("edge", "other-edge"),
    types = c("edge", "edge"),
    from = c("unknown", "edge"),
    to = c("other-edge", "unknown")
  )
  result <- validate_tracer_table(new_tracer_table(data))

  expect_true(any(result$errors$field == "To" & result$errors$row == 1L))
  expect_true(any(result$errors$field == "From" & result$errors$row == 2L))
  expect_equal(nrow(result$warnings), 2L)
})