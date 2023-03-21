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
  
  Filter_RKD_data <- RKD_data
  
  files_test <-  list.files(output_path, pattern = ".", all.files = FALSE, recursive = TRUE)
  if(identical(files_test, character(0)) == TRUE){
    stop("Your output folder don't exist")
  }
  setwd(output_path)
  write.csv(Filter_RKD_data, paste("Redcap_clinical_data_filter", Sys.Date() , ".csv", sep=""), row.names = F)
  return(Filter_RKD_data)
  
}