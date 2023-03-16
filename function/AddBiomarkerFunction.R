#' @title AddBiomarkerRDK
#' @author Yagmur Dogay
#' Version:
#' Date: 25-Jan-23
#' Objective: The objective is to merge biomarker data with RKD data
#' 
#' @param input_path_1 folder where the RKD data is 
#' @param input_path_2 folder where the Biomarker data is
#' @param data_1 Cleaned RKD data
#' @param data_2 Biomarker data used
#' @param 
#' @return The merge data

library('lubridate')


#' @param data_1 Cleaned RKD data
#' @param data_2 Biomarker data
#' @param column_a1 RKD ID Column name in RKD Data
#' @param column_a2 Date of visit column name in RKD Data
#' @param column_b1 ID Column name in Biomarker Data
#' @param column_b2 Date of sample Column name in Biomarker Data
#' @param script_file_name_1 Name of R script which has CleanRKD Function
#' @param script_file_name_2 Name of R script which has LoadBiomarker Function

AddBiomarkerRKD <- function(data_1, data_2,
                            column_a1, column_a2,
                            column_b1, column_b2,
                            script_file_name_1,
                            script_file_name_2){
  #### Test on the agrument
  if (is.data.frame(data_1)==FALSE) {
    stop('The argument data_1 need to be a dataframe argument')
  }
  if (is.data.frame(data_2)==FALSE) {
    stop('The argument data_1 need to be a dataframe argument')
  }
  if (is.character(column_a1)==FALSE) {
    stop('The argument column_a1 need to be a character argument')
  }
  if (is.character(column_a2)==FALSE) {
    stop('The argument column_a2 need to be a character argument')
  }  
  if (is.character(column_b1)==FALSE) {
    stop('The argument column_a1 need to be a character argument')
  }
  if (is.character(column_b2)==FALSE) {
    stop('The argument column_b2 need to be a character argument')
  }
  
  ### Load Cleaned RKD frame
  ### Note: In order to run the CleanRKD function, you need to pass arguments
  
  source(script_file_name_1)
  r_data <- Lo('')
  
  ### Load
  
  ### Test on date columns in both data
  # Long code
  # column_list <- list(column_a2, column_b2)
  # datas_list <- list(data_1, data_2)
  # for (col in column_list){
  #   for (frame in data_frame_list){
  #     if (col %in% colnames(d)){
  #       #print('yes')
  #       if (is.Date(frame[,col])==FALSE){
  #         stop('Date column need to be in date format')
  #       }
  #       else {
  #         print('Date column is in date format')
  #       }
  #     }
  #   }
  # }

  # Short code for checking date column if it is in date format
  if (is.Date(data_1[,column_a2])==FALSE){
    stop('Date column of data need to be in date format')
  }
  if (is.Date(data_2[,column_b2])==FALSE){
    stop('Date column of data need to be in date format')
  }

  m_df <- merge(data_1, data_2, by.x=c(column_a1,column_a2),
                by.y=c(column_b1,column_b2))
  return(m_df)
}

#AddBiomarkerRKD(data1,data2,'ID','times','P.ID','days')

