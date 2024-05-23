#' @title CPD Treatment On/off
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD Treatment On/Off
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param IV_Therapy Data from CPD IVTherapy for tretament from \code{\link{CPD_IVTherapy_Treatment}} function
#' @param Con_med Date from CPD Continous medication for Treatment  from \code{\link{CPD_Medication_Treatment}} function
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

CDP_Treatment <- function(IV_Therapy, Con_med, merged_data, output_dir){
  
  topifnot("Your argument need to be a data frame"=is.data.frame(merged_data))
  stopifnot("Your argument need to be a data frame"=is.data.frame(IV_Therapy))
  stopifnot("Your argument need to be a data frame"=is.data.frame(Con_med))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  
  
}