#' @title CPD IV Therapy
#' @author Matthieu COQ
#'
#' @description The Goal is to merge the IV Therapy and general characteristics from \code{\link{SplitRIV}} function
#' 
#' Version: 1.0
#' 
#' Date: 17-Apr-23
#' 
#' @param IVTherapy_data  {"name": "rkd_data","desc": "RIV data from \code{\link{SplitRIV}} function","options": (),"type": "file"}
#' @param output_dir  {"name": "output_dir","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#'
#' @details
#' 
#' In this function, we calculate the interval in number of day between the date of IV Theray and the date of diagnosis.
#' 
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @importFrom rlang .data
#' @export
CPD_IVTherapy_interval <- function (IVTherapy_data, output_dir){
  stopifnot("Your argument need to be a data frame"=is.list(IVTherapy_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  rkd_data <- merge(IVTherapy_data$IVTherapy, IVTherapy_data$Initial[,c("RKD.ID", "Date.of.diagnosis")], by="RKD.ID")
  rkd_data$IVTherapy_interval_from_diagnosis <- as.numeric(rkd_data$Date.of.IV.therapy-rkd_data$Date.of.diagnosis)
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_IVTherapy_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}