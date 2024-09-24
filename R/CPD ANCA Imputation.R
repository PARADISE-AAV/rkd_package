#' @title CPD ANCA Imputation
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD ANCA Imputation
#' 
#' Version: 1.0
#' 
#' Date: 24-September-23
#'
#' @param Encounter Data from Encounter from \code{\link{SplitRIV}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' 
#' @import lubridate
#' @export

CPD_ANCA_Imputation <- function(Encounter, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(Encounter))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  n=nrow(Encounter)
  for(i in 1:n){
    
  } 
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_IS_medication_imputation_function_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(Encounter2, output_filename, row.names = FALSE)
  return(Encounter2)
  
}