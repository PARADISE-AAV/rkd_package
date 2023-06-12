#' @title ClassifyRKDCases
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 15-Mar-23
#' Objective: The objective is to classify the patient of RKD data based on criteria described in the function apply
#'
#'
#' @param RKDdata RKD data from DemographicFilterRKD function
#' @param ouput_path folder where the Redcap data will be saved
#' @param algorithm function use to classify the RKD patient, the possibility are "BRelapse" or "Paradise_Encounter"
#' @param interval_from_diagnostics the interval from diagnostics for the algorithm Paradise_Encounter by default 6
#' @return The Redcap data cleaned in your folder and in an object 
#' @export


ClassifyRKDCases = function(RKDdata, output_path, algorithm, interval_from_diagnostics=6) {
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  if (is.character(algorithm) == FALSE) {
    stop("The argument algorithm need to be a character argument")
  }
  if (algorithm != "BRelapse" & algorithm != "Paradise_Encounter"){
    stop("The argument algorithm need to be BRelapse or Paradise_Encounter")
  }
  
  
    RKD_data <- RKDdata
  ###check that you load a real file
  if (ncol(RKD_data) == 0 | nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }
  
  if (algorithm == "BRelapse") {
    Classify_RKD_data <- BRelapseFunction(RKD_data)
  }
  if (algorithm == "Paradise_Encounter") {
    Classify_RKD_data <- Paradise_Encounter(RKD_data,interval_from_diagnostics)
  }
  
  
  files_test <-  list.dirs(output_path)
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