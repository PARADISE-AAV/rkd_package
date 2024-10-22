#' @title CPD IS Imputation status
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD IS Imputation status for Treatment On/Off
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param Encounter Data from Encounter from \code{\link{CPD_IS_medication_Imputation}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' 
#' @import lubridate
#' @export

CPD_IS_status_Imputation <- function(Encounter, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(Encounter))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  Encounter$Immunosuppressive.status[which(Encounter$Immunosuppressive.status=="" & Encounter$Immunosuppressive.medication!="" & Encounter$Immunosuppressive.medication!="No")]="Currently on immunosuppression"
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_IS_status_imputation_function_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(Encounter, output_filename, row.names = FALSE)
  return(Encounter)
  
}