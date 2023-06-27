#' @title ClinicalFilterRKD
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 21-Mar-23
#' Objective: The objective is to filter the RKD data based on clinical criteria like the classification of patient
#'
#'
#' @param RKDdata RKD data from ClassifyRKDCases function or AddBiomarker function
#' @param output_path folder where the filter RKD data will be saved
#' @param algorithm The algorithm used in the ClassifyRKDCases function
#' @return The Redcap data cleaned in your folder and in an object
#' @export
ClinicalFilterRKD <- function(RKDdata, output_path, algorithm) {
  # Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  if (!dir.exists(output_path)) {
    stop("Your output folder don't exist")
  }
  algorithm <- match.arg(algorithm, c('BRelapse', 'Paradise_Encounter'))

  RKD_data <- RKDdata
  if (ncol(RKD_data) == 0 || nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }

  if (algorithm == "BRelapse") {
    Filter_RKD_data <- RKD_data[RKD_data$Relapse != "", ]
  }
  if (algorithm == "Paradise_Encounter") {
    Filter_RKD_data <- RKD_data[RKD_data$Paradise.Encounters == 1, ]
  }

  output_filename <- file.path(output_path,
    paste0("Redcap_clinical_data_filter", Sys.Date(), ".csv"))
  write.csv(Filter_RKD_data, output_filename, row.names = FALSE)
  return(Filter_RKD_data)
}