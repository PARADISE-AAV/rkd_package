# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(rkdpipeline)



testthat::test_that("function Inclusion Criteria", {
  rkd=clean_rkd
  output_dir=tempdir()
  testthat::expect_equal(nrow(DemographicFilterRKD(rkd, output_dir))<=nrow(rkd), TRUE)
  testthat::expect_error(DemographicFilterRKD(rkd, output_dir), NA)
  output_dir=1
  testthat::expect_error(DemographicFilterRKD(rkd, output_dir), "The argument output_path need to be a character argument")
})