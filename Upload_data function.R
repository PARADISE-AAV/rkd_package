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


LoadRKD=function(input_path, data, output_path, date){
  setwd(input_path)
  data <- read.csv(data, header=T)
  return(data)
  setwd(output_path)
  write.csv(paste("Redcap_clinical_data_",date,sep=""), data, row.names = F)
}