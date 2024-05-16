#' @title Clean rare kidney disease data
#' @author Matthieu COQ
#'
#' @description The Goal is to merge the Encounter and general characteristics from \code{\link{SplitRIV}} function
#' 
#' Version: 1.0
#' 
#' Date: 14-Mar-23
#'
#' @param Encounter Encounter data from \code{\link{SplitRIV}} function
#' @param Initial General Characteristics data from \code{\link{SplitRIV}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
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
      Status == 'Lost to follow-up' ~ Date.of..opt.out..or..Lost.to.follow.up.
    )) %>%
    dplyr::group_by(RKD.ID) %>%
    dplyr::mutate(Date_Last_Follow_up = max_if_any(last_encounter))%>%
    dplyr::ungroup()
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_clinical_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}


