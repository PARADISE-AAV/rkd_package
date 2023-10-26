#' @title AddBiomarkerRKD
#' @author Yagmur Dogay
#' Version:
#' Date: 25-Jan-23
#' Objective: The objective is to merge biomarker data with RKD data
#' @import dplyr
#' @importFrom rlang .data
AddBiomarkerRKD <- function(data_rkd, data_bio, interval){

  
  data_bio$Start.date.of.date.range <-
    as.Date(data_bio$Date.of.encounter) - interval
  
  data_bio$End.date.of.date.range <-
    as.Date(data_bio$Date.of.encounter) + interval
  
  #colnames(data_bio)[colnames(data_bio) == 'Main.Study.ID'] <- 'RKD.ID'
  merged_frame <- fuzzy_inner_join(
    data_rkd, data_bio,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Start.date.of.date.range",
      "Date.Of.Visit" = "End.date.of.date.range"
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) %>%
    dplyr::select(dplyr::everything())
  
  
  rownames(merged_frame) <- NULL
  colnames(merged_frame)[colnames(merged_frame) == 'RKD.ID.x'] <- 'RKD.ID'
  colnames(merged_frame)[colnames(merged_frame) == 'RKD.ID.y'] <- 'Biomarker.RKD.ID'
  return(merged_frame)
}
