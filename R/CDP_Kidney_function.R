#' @title CPD Kidney function
#' @author Matthieu COQ
#'
#' @description The Goal is to merge the Continuous medication and general characteristics from \code{\link{SplitRIV}} function
#' 
#' Version: 1.0
#' 
#' Date: 17-Apr-23
#'
#' @param renal Data from renal from \code{\link{CPD_Renal}} function
#' @param merge_data Data from the merge of encounter and General characteristics in the \code{\link{Merge_Encounter_initial}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @importFrom rlang .data
#' @export

CPD_Kidney_function <- function (merge_data, renal, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  stopifnot("Your argument need to be a data frame"=is.data.frame(renal))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  renal_frame <- merge(renal, merge_data[, c("RKD.ID", "Date_Last_Follow_up")], by= "RKD.ID")
  renal_frame <- renal_frame %>% distinct(RKD.ID, Date.of.transplant., .keep_all=TRUE)
  
  renal_frame <- renal_frame %>% 
    dplyr::mutate(Date.of.graft.failure = dplyr::case_when(
      is.na(Date.of.graft.failure) == TRUE ~ Date_Last_Follow_up,
      is.na(Date.of.graft.failure) == FALSE ~ Date.of.graft.failure
    ))
  
  
  merged_frame <-fuzzy_inner_join(
    merge_data, renal_frame,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Date.of.transplant.",
      "Date.Of.Visit" = "Date.of.graft.failure" 
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) %>%
    select(everything())
  
  
  rownames(merged_frame) <- NULL
  
  merged_frame$Dialysis = "Transplant"
  
  colnames(merged_frame)[1] = "RKD.ID"
  
  merge_renal_frame = merge(merge_data, merged_frame[,c("RKD.ID", "Date.Of.Visit", "Dialysis")], by= c("RKD.ID", "Date.Of.Visit"),all.x = TRUE)
  
  merge_renal_frame <- merge_renal_frame %>% 
    dplyr::mutate(Dialysis.1 = dplyr::case_when(
      Dialysis.dependent == "Yes" ~ "On Dialysis",
      Dialysis.dependent == "No" ~ "Off Dialysis",
      Dialysis.dependent == "" ~ ""
    ))
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_kidney_function_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}