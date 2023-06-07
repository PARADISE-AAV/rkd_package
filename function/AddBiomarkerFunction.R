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

AddBiomarkerRKD <- function(data_rkd, data_bio){

  #colnames(data_bio)[colnames(data_bio) == 'Main.Study.ID'] <- 'RKD.ID'
  merged_frame <-fuzzy_inner_join(
    data_rkd, data_bio,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Start.date.of.date.range",
      "Date.Of.Visit" = "End.date.of.date.range"
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) %>%
    select(everything())
  
  
  rownames(merged_frame) <- NULL
  colnames(merged_frame)[colnames(merged_frame) == 'RKD.ID.x'] <- 'RKD.ID'
  colnames(merged_frame)[colnames(merged_frame) == 'RKD.ID.y'] <- 'Biomarker.RKD.ID'
  return(merged_frame)
}


