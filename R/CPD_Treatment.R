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
  treatment_data$treatment = NA
  n=nrow(treatment_data)
  for(i in 1:n){
    if(is.na(treatment_data$IV.therapy.x[i])==F){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$IV.therapy.x[i], sep="")
    }
    if(is.na(treatment_data$IV.therapy.y[i])==F){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$IV.therapy.y[i][i], sep="")
    }
    if(is.na(treatment_data$IV.therapy[i])==F){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$IV.therapy[i][i], sep="")
    }
  }
  
}