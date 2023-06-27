#' @title CleanFW
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 22-Mar-23
#' Objective: The objective is to clean the Freezerwork data
#'
#'
#' @param FWdata FW data from loadFW function
#' @param output_path folder where the Redcap data will be saved
#' @return The Redcap data cleaned in your folder and in an object
#' @export


CleanFW=function(FWdata, output_path){
  
  ####Test on the argument
  if (is.data.frame(FWdata) == FALSE) {
    stop("The argument FWdata need to be a dataframe argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  
  FW_data <- FWdata
  ###check that you load a real file
  if(ncol(FW_data)==0 | nrow(FW_data)==0){
    stop("You give an empty files")
  }
  
  FW_data_filter_RKID <- FW_data[which(is.na(as.numeric(FW_data$Main.Study.ID)) == FALSE), ]
  
  FW_data_filter_amount <- FW_data_filter_RKID[which(FW_data_filter_RKID$Current.Amount > 0),]
  
  FW_data_filter_amount$Date.of.encounter <- as.Date(FW_data_filter_amount$Date.of.encounter, format = '%d/%m/%Y')
  
  Clean_FW_data <- FW_data_filter_amount
  
  files_test <-  list.dirs(output_path)
  if(identical(files_test, character(0)) == TRUE){
    stop("Your output folder don't exist")
  }
  write.csv(Clean_FW_data, paste(output_path, "/Freezerwork_data_clean", Sys.Date() , ".csv", sep=""), row.names = F)
  return(Clean_FW_data)
  
}