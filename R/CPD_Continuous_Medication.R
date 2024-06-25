#' @title CPD Continuous medication
#' @author Matthieu COQ
#'
#' @description The Goal is to merge the Continuous medication and general characteristics from \code{\link{SplitRIV}} function
#' 
#' Version: 1.0
#' 
#' Date: 17-Apr-23
#'
#' @param Medication_data list of data frame from \code{\link{SplitRIV}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @importFrom rlang .data
#' @export
CPD_Continuous_Medication <- function (Medication_data, output_dir){
  stopifnot("Your argument need to be a data frame"=is.list(Medication_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }

  
  rkd_data <- merge(Medication_data$Medication, Medication_data$Initial[,c("RKD.ID", "Date.of.diagnosis")], by="RKD.ID")
  rkd_data$medstart_interval_from_diagnosis <- as.numeric(rkd_data$Start.Date-rkd_data$Date.of.diagnosis)
  rkd_data$medstop_interval_from_diagnosis <- as.numeric(rkd_data$Stop.Date-rkd_data$Date.of.diagnosis)
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_countinuous_medication_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}