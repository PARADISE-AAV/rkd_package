#' @title Merge Encounter and Initial frame
#' @author Matthieu COQ
#'
#' @description The Goal is to merge the Encounter and general characteristics from \code{\link{SplitRIV}} function
#' 
#' Version: 1.0
#' 
#' Date: 14-Mar-23
#' @param Encounter  {"name": "Encounter","desc": "RIV data from \code{\link{SplitRIV}} function","options": (),"type": "file"}
#' @param Initial  {"name": "Initial","desc": "RIV data from \code{\link{SplitRIV}} function","options": (),"type": "file"}
#' @param output_dir  {"name": "output_dir","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#'
#' @details
#' 
#' In this function, we merge the Encounter frame and the general characteristics frame with the RIV.ID. In addition, we calculate th age at the encounter, the interval in number of days between the encounter date and the date of diagnosis. We calculate the date of the last follow up.
#' 
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @importFrom rlang .data
#' @export
Merge_Encounter_initial <- function(Encounter, Initial, output_dir){

  rkd_data= merge(Initial, Encounter, by="RKD.ID")
  
  # Add age variable
  rkd_data <- rkd_data %>%
    dplyr::mutate(Age_Encounters =
                    lubridate::year(.data$Date.Of.Visit) - lubridate::year(.data$Date.of.Birth))
  
  
  
  
  rkd_data$interval_from_diagnosis=as.numeric(rkd_data$Date.Of.Visit-rkd_data$Date.of.diagnosis)
  
  rkd_data=rkd_data %>% 
    dplyr::mutate(last_encounter = dplyr::case_when(
      Status == 'Alive' ~ Date.Of.Visit,
      Status == 'Dead' ~ Date.of.event,
      Status == 'Lost to follow-up' ~ Date.Of.Visit
    )) %>%
    dplyr::group_by(RKD.ID) %>%
    dplyr::mutate(Date_Last_Follow_up = max(last_encounter, na.rm=TRUE))%>%
    dplyr::ungroup()
  
  rkd_data$kidney_involvment_diagnosis= rkd_data$Systems.involved.at.any.point..choice.Kidney.
  
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_clinical_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}


