#' @title ClassifyRKDCases
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 15-Mar-23
#' Objective: The objective is to classify the patient of RKD data based on criteria described in the function apply
#'
#'
#' @param RKDdata RKD data from DemographicFilterRKD function
#' @param ouput_path folder where the Redcap data will be saved
#' @param algorithm function use to classify the RKD patient, the possibility are "BRelapse"
#' @return The Redcap data cleaned in your folder and in an object
#' @export


ClassifyRKDCases = function(RKDdata, output_path, algorithm) {
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument files_name need to be a character argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  if (is.character(algorithm) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  if (algorithm != "BRelapse"
  )
  
  
    RKD_data <- RKDdata
  ###check that you load a real file
  if (ncol(RKD_data) == 0 | nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }
  
  if (algorithm == "BRelapse") {
    Classify_RKD_data <- BRelapseFunction(RKD_data)
  }
  
  
  
  
  files_test <-
    list.files(
      output_path,
      pattern = ".",
      all.files = FALSE,
      recursive = TRUE
    )
  if (identical(files_test, character(0)) == TRUE) {
    stop("Your output folder don't exist")
  }
  setwd(output_path)
  write.csv(
    Classify_RKD_data,
    paste(
      "Redcap_clinical_data_with-classification",
      Sys.Date() ,
      ".csv",
      sep = ""
    ),
    row.names = F
  )
  return(Classify_RKD_data)
  
}