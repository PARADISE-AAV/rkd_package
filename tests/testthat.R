# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(rkdpipeline)



testthat::test_that("function LoadRKD", {
  file_name=1
  testthat::expect_error(load_rkd(file_name), "Your argument need to be a character")
  
  file_name="a/b"
  testthat::expect_error(load_rkd(file_name), 'Your input folder doesn\'t exist')
})

testthat::test_that("function LoadRKD", {
  
  
}) 