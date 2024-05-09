#' @title CPD Complication
#' @author Matthieu COQ
#'
#' @description The Goal is to merge the Complication and general characteristics from \code{\link{SplitRIV}} function
#' 
#' Version: 1.0
#' 
#' Date: 17-Apr-23
#'
#' @param Complication_data list of data frame from \code{\link{SplitRIV}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @importFrom rlang .data
#' @export
CPD_Complication <- function (Complication_data, output_dir){
  stopifnot("Your argument need to be a data frame"=is.list(Complication_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  rkd_data <- merge(Complication_data$Complication, Complication_data$Initial[,c("RKD.ID", "Date.of.diagnosis")], by="RKD.ID")
  rkd_data$Complication_interval_from_diagnosis <- days(rkd_data$Date.of.event.1)-days(rkd_data$Date.of.diagnosis)
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_Complication_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}