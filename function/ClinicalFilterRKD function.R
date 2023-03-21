#' @title ClinicalFilterRKD
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 21-Mar-23
#' Objective: The objective is to filter the RKD data based on clinical criteria like the classification of patient
#'
#'
#' @param RKDdata RKD data from ClassifyRKDCases function or AddBiomarker function
#' @param ouput_path folder where the filter RKD data will be saved
#' @return The Redcap data cleaned in your folder and in an object
#' @export
#' 

ClinicalFilterRKD=function(RKDdata, output_path){
  
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument files_name need to be a character argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  
  RKD_data <- RKDdata
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  
  
}