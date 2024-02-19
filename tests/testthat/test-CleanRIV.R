test_that("Data cleaning", {
  library(lubridate)
  rkd <- as.data.frame(NULL)
  output_path <- 1
  expect_error(clean_riv(rkd, output_path), "Your argument need to be a character")

  rkd <- NULL
  output_path <- getwd()
  expect_error(clean_riv(rkd, output_path), "Your argument need to be a data frame")

  rkd <- as.data.frame(NULL)
  output_path <- "a"
  expect_error(clean_riv(rkd, output_path), "Specified output folder does not exist")

  rkd <- as.data.frame(NULL)
  output_path <- getwd()
  expect_error(clean_riv(rkd, output_path), "You supplied an empty file")

  rkd <- cbind.data.frame(
    RKD.ID = c(1, 1, 2, 2, 3, 3),
    Date.Of.Visit = c(
      "1985/10/01", "1987/12/02", "1998/11/14", "1999/11/19",
      "1987/12/25", "2005/01/03"
    ),
    Date.of.Birth.known.unknown = c("Known", "Known", "Unknown", "Unknown", "Unknown", "Unknown")
  )
  expect_equal(is.Date(rivpipeline:::rkd_parse_dates(rkd)$Date.Of.Visit), TRUE)
  expect_equal(is.Date(rivpipeline:::rkd_parse_dates(rkd)$Date.of.Birth.known.unknown), FALSE)

  rkd2 <- cbind.data.frame(
    RKD.ID = c(1, 1, 2, 2, 3, 3, 4, 5, 6, 7),
    Ethnicity = c(
      "White-Irish",
      "White-Irish",
      "White-French", "White-French",
      "B2 - African",
      "Black",
      "Asian",
      "Mixed-Black-White",
      "NS - Not Stated",
      "Other"
    )
  )
  expect_equal(nrow(table(rivpipeline:::rkd_collapse_ethnicity(rkd2)$Ethnicity)), 6)

  rkd3 <- cbind.data.frame(
    RKD.ID = c(1, 1, 2, 2, 3, 3, "1", 6),
    Date.Of.Visit = c(
      "1985/10/01", "1987/12/02", "1998/11/14", "1999/11/19",
      "1987/12/25", "2005/01/03", "2013/02/05", "2023/05/14"
    ),
    Patient.ID = c(1, 1, 2, 2, 3, 3, 1, 6)
  )
  expect_equal(rivpipeline:::rkd_fix_ids(rkd3)$RKD.ID[7], "1")
})