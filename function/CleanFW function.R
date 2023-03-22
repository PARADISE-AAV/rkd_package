#' @title CleanFW
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 22-Mar-23
#' Objective: The objective is to clean the RKD data and send the problematic data to the RKD person
#'
#'
#' @param FWdata FW data from loadFW function
#' @param ouput_path folder where the Redcap data will be saved
#' @return The Redcap data cleaned in your folder and in an object
#' @export


CleanFW=function(FWdata, output_path){
  
  ####Test on the argument
  if (is.data.frame(FWdata) == FALSE) {
    stop("The argument files_name need to be a character argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  
  FW_data <- FWdata
  ###check that you load a real file
  if(ncol(FW_data)==0 | nrow(FW_data)==0){
    stop("You give an empty files")
  }
  
  
  
  Clean_FW_data <- FW_data
  
  files_test <-  list.files(output_path, pattern = ".", all.files = FALSE, recursive = TRUE)
  if(identical(files_test, character(0)) == TRUE){
    stop("Your output folder don't exist")
  }
  setwd(output_path)
  write.csv(Clean_FW_data, paste("Freezerwork_data_clean", Sys.Date() , ".csv", sep=""), row.names = F)
  return(Clean_FW_data)
  
}