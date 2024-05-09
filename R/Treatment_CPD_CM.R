#' @title CPD Continuous medication treatment
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD Continuous medication for Treatment On/Off
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param Medication Data from Continuous medication from \code{\link{SplitRIV}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' 
#' @import lubridate
#' @export

Treatment_CPD_CM <- function(Medication, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  medication_filter <- Medication[which(Medication$Drug!=""), c("RKD.ID", "Drug", "Dose", "Start.Date", "Stop.Date")]
  
  
  
}