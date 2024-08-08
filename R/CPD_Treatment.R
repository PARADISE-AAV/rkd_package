#' @title CPD IS medication
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD Treatment
#' 
#' Version: 1.0
#' 
#' Date: 9-Aug-24
#'
#' @param treatment_data Data from IVTherapy from \code{\link{CPD_Treatment_OnOff}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @importFrom data.table %like%
#' @export

CPD_Treatment = function(treatment_data, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(treatment_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
}