#' @title CleanRKD
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 24-Jan-23
#' Objective: The objective is to clean the RKD data and send the problematic data to the RKD person
#'
#'
#' @param RKDdata RKD data from loadRKD function
#' @param ouput_path folder where the Redcap data will be saved
#' @param date date when you load your data
#' @return The Redcap data cleaned in your folder and in an object
#' @export


CleanRKD=function(RKDdata, output_path){
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument files_name need to be a character argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  #extract the folder and the files 
  
  
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  RKD_data$Date.of.Birth=as.Date(RKD_data$Date.of.Birth)
  
  Clean_RKD_data=RKD_data
  
  files_test <-  list.files(output_path, pattern = ".", all.files = FALSE, recursive = TRUE)
  if(identical(files_test, character(0)) == TRUE){
    stop("Your output folder don't exist")
  }
  setwd(output_path)
  write.csv(Clean_RKD_data, paste("Redcap_clinical_data_clean", Sys.Date() , ".csv", sep=""), row.names = F)
  return(Clean_RKD_data)
}