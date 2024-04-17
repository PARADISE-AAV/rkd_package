#' @title CPD Continuous medication
#' @author Matthieu COQ
#'
#' @description The Goal is to merge the Continous medication and general characteristics from \code{\link{SplitRIV}} function
#' 
#' Version: 1.0
#' 
#' Date: 17-Apr-23
#'
#' @param Medication_data Continuous medication data from \code{\link{SplitRIV}} function
#' @param Initial_data General Characteristics data from \code{\link{SplitRIV}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @importFrom rlang .data
#' @export
CPD_Continuous_Medication <- function (Medication_data, Initial_data, output_dir){
  stopifnot("Your argument need to be a data frame"=is.data.frame(Medication_data))
  stopifnot("Your argument need to be a data frame"=is.data.frame(Initial_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }

  rkd_data <- merge(Medication_data, Initial_data[,c("RKD.ID", "Date.of.diagnosis")], by="RKD.ID")
  rkd_data$medstart_interval_from_diagnosis <- days(rkd_data$Start.Date)-days(rkd_data$Date.of.diagnosis)
  rkd_data$medstop_interval_from_diagnosis <- days(rkd_data$Stop.Date)-days(rkd_data$Date.of.diagnosis)
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_countinuous_medication_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}