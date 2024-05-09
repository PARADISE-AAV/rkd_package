#' @title CPD IS Imputation
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD IS Imputation for Treatment On/Off
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param Encounter Data from Encounter from \code{\link{CPD_Encounter}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' 
#' @import lubridate
#' @export

CPD_IS_Imputation <- function(Encounter, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  n=nrow(Encounter)-1
  for(i in 2:n){
    if((Encounter$RKD.ID[i-1]==Encounter$RKD.ID[i] & Encounter$RKD.ID[i+1]==Encounter$RKD.ID[i]) & Encounter$Immunosuppressive.medication[i]==""
       & Encounter$Immunosuppressive.medication[i-1] == Encounter$Immunosuppressive.medication[i+1]){
      
    }
  }
  
}