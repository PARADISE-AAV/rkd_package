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
  
  # Anti MPO PR3
  rkd_data <- rkd_anti_mpo_pr3(rkd_data)
  
  rkd_data2$interval_from_diagnosis=as.numeric(rkd_data2$Date.Of.Visit-rkd_data2$Date.of.diagnosis)
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_clinical_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}


#' @import dplyr
rkd_anti_mpo_pr3 <- function(data) {
  dplyr::mutate(data, AntiMPO_PR3 = dplyr::case_when(
    is.na(.data$Anti.MPO.level) & is.na(.data$Anti.PR3.level) ~ NA,
    is.na(.data$Anti.MPO.level) & !is.na(.data$Anti.PR3.level) ~ "PR3",
    !is.na(.data$Anti.MPO.level) & is.na(.data$Anti.PR3.level) ~ "MPO",
    .data$Anti.MPO.level > .data$Anti.PR3.level ~ "MPO",
    TRUE ~ "PR3"
  ))
}