test_that("Loading inputs", {
  file_name <- 1
  expect_error(load_riv(file_name), "Your argument need to be a character")

  file_name <- "a/b"
  expect_error(load_riv(file_name), "Your input folder doesn't exist")
})
