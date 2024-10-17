test_that("private_key.pem exists in inst/app", {
  file_path <- system.file("app", "public_key.pem", package = "TraceR")
  expect_true(file.exists(file_path), info = "The file private_key.pem should exist in inst/app")
})

test_that("public_key.pem exists in inst/app", {
  file_path <- system.file("app", "private_key.pem", package = "TraceR")
  expect_true(file.exists(file_path), info = "The file private_key.pem should exist in inst/app")
})
