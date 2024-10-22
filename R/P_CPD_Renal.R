#' @title CPD Renal Transplant
#' @author Matthieu COQ
#'
#' @description The Goal is to merge the Renal Transplant and general characteristics from \code{\link{SplitRIV}} function
#' 
#' Version: 1.0
#' 
#' Date: 17-Apr-23
#'
#' @param Renal_data list of dataframe from \code{\link{SplitRIV}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @importFrom rlang .data
#' @export
CPD_Renal <- function (Renal_data, output_dir){
  stopifnot("Your argument need to be a data frame"=is.list(Renal_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  rkd_data <- merge(Renal_data$Transplant, Renal_data$Initial[,c("RKD.ID", "Date.of.diagnosis")], by="RKD.ID")
  rkd_data$Renal_interval_from_diagnosis <- as.numeric(rkd_data$Date.of.transplant.-rkd_data$Date.of.diagnosis)
  rkd_data$txfail_interval_from_diagnosis <- as.numeric(rkd_data$Date.of.graft.failure-rkd_data$Date.of.diagnosis)
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_Renal_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}