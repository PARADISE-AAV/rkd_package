#' @title CPD IV Therapy treatment
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD IS Imputation for Treatment On/Off
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param IV_Therapy Data from IVTherapy from \code{\link{CPD_IVTherapy_Treatment}} function
#' @param ConMed Data from Continous medication from \code{\link{CPD_Medication_Treatment}} function
#' @param merged_data Data from the merge of encounter and General characteristics in the \code{\link{Merge_Encounter_initial}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @export

CPD_Treatment_OnOff= function(IV_Therapy, ConMed, merged_data, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merged_data))
  stopifnot("Your argument need to be a data frame"=is.data.frame(IV_Therapy))
  stopifnot("Your argument need to be a data frame"=is.data.frame(ConMed))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  IV_ConMed <- merge(IV_Therapy, ConMed, by = c("RKD.ID", "Date.Of.Visit"), all = TRUE)
  
  data_merged <- merge(merged_data, IV_ConMed, by = c("RKD.ID", "Date.Of.Visit"), all = TRUE)
  
  data_merged$treatment=Map(c,data_merged$IVtherapy, data_merged$Drug)
  
  data_merged <- data_merged %>%
    dplyr::mutate(Step1 = dplyr::case_when(
      Immunosuppressive.status == 'Treatment Naive' | Immunosuppressive.status == 'Discontinuation of immunosuppression > 6 months prior to this encounter' ~ "Off treatment",
      Immunosuppressive.status != 'Treatment Naive' & Immunosuppressive.status != 'Discontinuation of immunosuppression > 6 months prior to this encounter' ~ NA
    ))
  data_merged <- data_merged %>%
    dplyr::mutate(Step2 = dplyr::case_when(
    Current.corticosteroid.dose == "> 20 mg/day" | Current.corticosteroid.dose == "11 - 20 mg/day" ~ "On treatment",
    Current.corticosteroid.dose != "> 20 mg/day" & Current.corticosteroid.dose != "11 - 20 mg/day" ~ NA
  ))
  
  
  
}