#' @title Upload_data
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 24-Jan-23
#' Objective: the Objective is to performed QC1 on the new Biopred data. The QC1 is a test to check that the positive control don't take more the 15% of the reads
#'
#'
#' @param input_path folder where the Redcap data are
#' @param ouput_path folder where the Redcap data will be saved
#' @param date date when you load your data
#' @return The Redcap data in your folder and in an object
#' @export


Upload_data=function(input_path, output_path,date){
  setwd(input_path)
  data <- read.csv("", header=T)
  return(data)
  setwd(output_path)
  write.csv(paste("Redcap_clinical_data_",date,sep=""), data, row.names = F)
}