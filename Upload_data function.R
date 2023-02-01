#' @title LoadRKD
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 24-Jan-23
#' Objective: The objective is to load the RKD data in a datframe object
#'
#'
#' @param input_path folder where the Redcap data are
#' @param data RKD data used
#' @param ouput_path folder where the Redcap data will be saved
#' @param date date when you load your data
#' @return The Redcap data in your folder and in an object
#' @export


LoadRKD=function(input_path, files, output_path, date){
  ####Test on the argument
  if (is.character(input_path) == FALSE) {
    stop("The argument input_path need to be a character argument")
  }
  if (is.character(files) == FALSE) {
    stop("The argument files need to be a character argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  if (is.character(date) == FALSE) {
    stop("The argument date need to be a character argument")
  }
  
  setwd(input_path)
  RKD_data <- read.csv(files, header = TRUE)
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  
  setwd(output_path)
  write.csv(RKD_data, paste("Redcap_clinical_data_", date, ".csv", sep=""), row.names = F)
  return(RKD_data)
}