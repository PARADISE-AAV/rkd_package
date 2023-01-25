#' @title AddBiomarkerRDK
#' @author Yagmur Dogay
#' Version:
#' Date: 25-Jan-23
#' Objective: The objective is to merge biomarker data with RKD data
#' 
#' @param input_path_1 folder where the RKD data is 
#' @param input_path_2 folder where the Biomarker data is
#' @param data_1 RKD data used
#' @param data_2 Biomarker data used
#' @param 
#' @return The merge data


AddBiomarkerRKD <- function(input_path_1, 
                            input_path_2, 
                            data_1, 
                            data_2) {
  setwd(input_path_1)
  data_1 <- read.csv(data_1, header = TRUE)
  setwd(input_path_2)
  data_2 <- read.csv(data_2, header = TRUE)
  data_2$days <- mdy(data_2$Date)
  #data_1$days <- mdy(data_1$days)
  merged_dataframe <- merge(data_1, data_2, by.x=c('record_id', 'date_of_visit'), by.y=c('RKD ID', 'Date of sample'), all=TRUE)
  return (merged_dataframe)
}