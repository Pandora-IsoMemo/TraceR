test_that("tracer_table JSON round-trips all columns, order, nulls, and Unicode", {
  table <- createExampleTracerTable()
  table$data$Author[1] <- NA_character_
  table$data[["Optional 5"]][2] <- ""
  table$data$Description[3] <- paste0("Data ", intToUtf8(233), " ", intToUtf8(0x1F680))
  names(table$data)[7:11] <- c("Author name", "Status", "Notes", "Optional 4", "Optional 5")
  table$data <- table$data[c("Notes", "ID", "Label", "Type", "From", "To", "Unique identifier", "Author name", "Status", "Optional 4", "Optional 5")]

  json <- tracer_table_to_json(table)
  restored <- tracer_table_from_json(json)

  expect_type(json, "character")
  expect_identical(restored$data, table$data)
  expect_true(grepl('"Author name":null', json, fixed = TRUE))
  expect_true(grepl(paste0(intToUtf8(233), " ", intToUtf8(0x1F680)), json, fixed = TRUE))
  expect_identical(tracer_table_from_json(charToRaw(json))$data, table$data)
})

test_that("empty tracer_tables retain their custom column schema", {
  table <- new_tracer_table()
  names(table$data)[7:11] <- c("Author", "Status", "Description", "Optional 4", "Optional 5")

  restored <- tracer_table_from_json(tracer_table_to_json(table))

  expect_equal(nrow(restored$data), 0L)
  expect_identical(names(restored$data), names(table$data))
  expect_true(all(vapply(restored$data, is.character, logical(1))))
})

test_that("DataTools-parsed JSON preserves nulls as missing character values", {
  table <- new_tracer_table()
  row <- as.list(rep(NA_character_, ncol(table$data)))
  names(row) <- names(table$data)
  row$ID <- "node-1"
  row$Type <- "node"
  table$data <- as.data.frame(lapply(row, identity), check.names = FALSE, stringsAsFactors = FALSE)

  parsed_by_datatools <- as.list(jsonlite::fromJSON(tracer_table_to_json(table)))
  restored <- tracer_table_from_json(parsed_by_datatools)

  expect_identical(restored$data, table$data)
})

test_that("table change detection compares ordered columns and values only", {
  saved <- createExampleTracerTable()
  current <- new_tracer_table(saved$data)
  rownames(current$data) <- as.character(seq_len(nrow(current$data)))
  expect_true(tracer_table_data_identical(saved, current))

  current$data$Label[1] <- "Changed"
  expect_false(tracer_table_data_identical(saved, current))
  current <- new_tracer_table(saved$data)
  names(current$data)[7:11] <- rev(names(current$data)[7:11])
  expect_false(tracer_table_data_identical(saved, current))
})

test_that("tracer_table JSON rejects malformed and unsupported formats", {
  table_json <- tracer_table_to_json(new_tracer_table())
  unsupported <- jsonlite::fromJSON(table_json, simplifyVector = FALSE)
  unsupported$version <- 2L

  expect_error(tracer_table_from_json("{ malformed"), "parse|lexical|syntax")
  expect_error(tracer_table_from_json(unsupported), "Unsupported tracer_table JSON version")
  expect_error(tracer_table_from_json(list(format = "graph", version = 1L)), "Invalid tracer_table JSON fields")
})

test_that("signed tracer_table JSON uses RSA-SHA256 and verifies exact payload bytes", {
  private_key <- openssl::rsa_keygen(bits = 2048)
  public_key <- openssl:::derive_pubkey(private_key)
  table <- createExampleTracerTable()
  signed_json <- tracer_table_to_signed_json(table, private_key)
  envelope <- jsonlite::fromJSON(signed_json, simplifyVector = FALSE)

  expect_identical(envelope$algorithm, "RSA-SHA256")
  expect_identical(tracer_table_from_signed_json(signed_json, public_key)$data, table$data)

  expect_tampering_rejected <- function(modified_table) {
    modified_envelope <- jsonlite::fromJSON(signed_json, simplifyVector = FALSE)
    modified_envelope$payload <- tracer_table_to_json(modified_table)
    modified_json <- as.character(jsonlite::toJSON(modified_envelope, auto_unbox = TRUE, null = "null"))
    expect_error(tracer_table_from_signed_json(modified_json, public_key), "signature verification failed")
  }

  modified_data <- table
  modified_data$data$Label[1] <- "Modified after signing"
  expect_tampering_rejected(modified_data)

  modified_row_order <- table
  modified_row_order$data <- modified_row_order$data[nrow(modified_row_order$data):1L, , drop = FALSE]
  expect_tampering_rejected(modified_row_order)

  modified_column_name <- table
  names(modified_column_name$data)[7] <- "Changed optional name"
  expect_tampering_rejected(modified_column_name)

  modified_column_order <- table
  modified_column_order$data <- modified_column_order$data[c(2L, 1L, 3:11)]
  expect_tampering_rejected(modified_column_order)

  envelope$algorithm <- "RSA-SHA1"
  legacy_algorithm_json <- as.character(jsonlite::toJSON(envelope, auto_unbox = TRUE, null = "null"))
  expect_error(
    tracer_table_from_signed_json(legacy_algorithm_json, public_key),
    "Unsupported signature algorithm"
  )

  expect_error(tracer_table_from_signed_json(tracer_table_to_json(table), public_key), "Invalid signed tracer_table JSON fields")
  expect_error(tracer_table_from_signed_json(signed_json, NULL), "A public key is required")
})