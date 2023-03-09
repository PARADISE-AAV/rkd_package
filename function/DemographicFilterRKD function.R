#' @title DemographicFilterRKD
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 24-Jan-23
#' Objective: The objective is to clean the RKD data and send the problematic data to the RKD person
#'
#'
#' @param RKDdata RKD data from CleanRKD function
#' @param ouput_path folder where the filter RKD data will be saved
#' @return The Redcap data cleaned in your folder and in an object
#' @export
#' 


DemographicFilterRKD=function(RKDdata, output_path){
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument files_name need to be a character argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  
  RKD_data <- RKDdata
  
  
  
}