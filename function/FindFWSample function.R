#' @title FindFWSample
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 22-Mar-23
#' Objective: The objective is to merge the RKD data filter and clean with the clean Freezer work data
#'
#'
#' @param FWdata FW data from CleanFW function
#' @param RKDdata RKD data from ClinicalFilterRKD function
#' @param ouput_path folder where the merged data will be saved
#' @return The Redcap data cleaned in your folder and in an object
#' @export

CleanFW=function(FWdata, RKDdata, output_path){
  
  #Checking the argument
  if (is.data.frame(FWdata) == FALSE) {
    stop("The argument FWdata need to be a dataframe argument")
  }
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument FWdata need to be a dataframe argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  
  RKD_data <- RKDdata
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  FW_data <- FWdata
  if(ncol(FW_data)==0 | nrow(FW_data)==0){
    stop("You give an empty files")
  }
  
}