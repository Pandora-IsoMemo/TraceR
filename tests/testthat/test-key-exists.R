test_that("private_key.pem exists in inst/app", {
  file_path <- "../../inst/app/private_key.pem"
  expect_true(file.exists(file_path), info = "The file private_key.pem should exist in inst/app")
})

test_that("public_key.pem exists in inst/app", {
  file_path <- "../../inst/app/public_key.pem"
  expect_true(file.exists(file_path), info = "The file private_key.pem should exist in inst/app")
})
