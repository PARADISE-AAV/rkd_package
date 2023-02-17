#' @title CleanRKD
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 24-Jan-23
#' Objective: The objective is to clean the RKD data and send the problematic data to the RKD person
#'
#'
#' @param input_path folder where the Redcap data are
#' @param files_name RKD data used
#' @param ouput_path folder where the Redcap data will be saved
#' @param date date when you load your data
#' @return The Redcap data in your folder and in an object
#' @export


CleanRKD=function(input_path, files_name, output_path, date){
  ####Test on the argument
  if (is.character(input_path) == FALSE) {
    stop("The argument input_path need to be a character argument")
  }
  if (is.character(files_name) == FALSE) {
    stop("The argument files_name need to be a character argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  if (is.character(date) == FALSE) {
    stop("The argument date need to be a character argument")
  }
  
  files_test <-  list.files(input_path, pattern = ".", all.files = FALSE, recursive = TRUE)
  if(identical(files_test, character(0)) == TRUE){
    stop("Your input folder don't exist")
  }
  if(length(which(files_test == files_name)) != 1){
    stop("There is no files requested or multiple times this files")
  }
  
  setwd(input_path)
  RKD_data <- read.csv(files_name, header = TRUE)
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  
  
  
  files_test <-  list.files(output_path, pattern = ".", all.files = FALSE, recursive = TRUE)
  if(identical(files_test, character(0)) == TRUE){
    stop("Your output folder don't exist")
  }
  setwd(output_path)
  write.csv(Clean_RKD_data, paste("Redcap_clinical_data_Clean", date, ".csv", sep=""), row.names = F)
  return(RKD_data)
}