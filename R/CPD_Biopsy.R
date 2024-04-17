#' @title CPD Biopsy
#' @author Matthieu COQ
#'
#' @description The Goal is to merge the Biopsy and general characteristics from \code{\link{SplitRIV}} function
#' 
#' Version: 1.0
#' 
#' Date: 17-Apr-23
#'
#' @param Biopsy_data Biopsy data from \code{\link{SplitRIV}} function
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
CPD_Biopsy <- function (Biopsy_data, Initial_data, output_dir){
  stopifnot("Your argument need to be a data frame"=is.data.frame(Biopsy_data))
  stopifnot("Your argument need to be a data frame"=is.data.frame(Initial_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  rkd_data <- merge(Biopsy_data, Initial_data[,c("RKD.ID", "Date.of.diagnosis")], by="RKD.ID")
  rkd_data$Biopsy_interval_from_diagnosis <- days(rkd_data$Date.of.biopsy)-days(rkd_data$Date.of.diagnosis)
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_Biopsy_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}