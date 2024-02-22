test_that("Demographic filters", {
  rkd <- NULL
  output_path <- getwd()
  expect_error(DemographicFilterRIV(rkd), "The argument RKDdata need to be a dataframe argument")

  rkd <- as.data.frame(NULL)
  output_path <- getwd()
  expect_error(DemographicFilterRIV(rkd), "You give an empty files")

  output_path <- getwd()
  rkd <- cbind.data.frame(
    RKD.ID = c(1, 2, 3, 4, 5, 6),
    Small.vessel.vasculitis..ANCA.associated. = c(
      "Granulomatosis with polyangiitis (Wegener) - Orpha:900",
      "Granulomatosis with polyangiitis (Wegener) - Orpha:900",
      "vasculitis", "Microscopic polyangiitis (including renal limited vasculitis) - ORPHA:727",
      "HV",
      "Microscopic polyangiitis (including renal limited vasculitis) - ORPHA:727"
    ),
    Diagnosis.confidence = c("Definite", "Definite", "", "probably", "maybe", "Definite"),
    Small.vessel.vasculitis..Immune.complex. = c("", "", "", "ANTI/GBM", "", ""),
    Secondary.vasculitis = c("No", "Yes", "No", "Yes", "No", "No"),
    Other = c("No", "Yes", "No", "Yes", "No", "No"),
    Medium.vessel.vasculitis = c("", "Kawasaky", "PAN", "", "", ""),
    Large.vessel.vasculitis = c("", "GCA", "Takayazu", "GCA", "", ""),
    Variable.vessel.vasculitis = c("", "", "Behcet", "", "Cogan", ""),
    At.any.point.ANCA.specificity = c("PR3", "MPO", "", "", "", ""),
    Biopsy.performed. = c("Yes", "no", "", "Yes", "No", "Yes"),
    Histologically.confirmed.diagnosis = c("No", "Yes", "No", "Yes", "Yes", "Yes")
  )
  expect_equal(nrow(DemographicFilterRIV(rkd)), 2)
})
