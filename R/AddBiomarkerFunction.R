#' @title Adding Biomarker data to the RIV data
#' @author Yagmur Dogay
#' @description The objective is to merge biomarker data with RKD data
#' @param data_rkd Data frame linking with RKD data coming from \code{\link{ClassifyRKDEncounter}}
#' @param data_bio Data frame linking with the biomarker data coming from \code{\link{LoadBM}}
#' @param interval The interval is a tolerance of number of days that the biomarker date is before and after the encounter date.
#' @returns the merge of Biomarker data and RKD data within an defined interval
#' @import dplyr
#' @importFrom rlang .data
#' @export
AddBiomarkerRKD <- function(data_rkd, data_bio, interval){

  
  data_bio$Start.date.of.date.range <-
    as.Date(data_bio$Date.Of.Visit) - interval
  
  data_bio$End.date.of.date.range <-
    as.Date(data_bio$Date.Of.Visit) + interval
  
  #colnames(data_bio)[colnames(data_bio) == 'Main.Study.ID'] <- 'RKD.ID'
  merged_frame <- fuzzy_full_join(
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
