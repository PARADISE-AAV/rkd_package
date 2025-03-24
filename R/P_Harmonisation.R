#' @title CPD Harmonisation
#' @author Matthieu COQ
#'
#' @description The Goal is to finalize the harmonisation
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param RIVdata  {"name": "rkd_data","desc": "RIV data from \code{\link{ClassifyRIVEncounter}} function","options": (),"type": "file"}
#' @param output_dir  {"name": "output_dir","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#' @details
#' to be added
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @importFrom data.table %like%
#' @export

CPD_Harmonisation <- function(RIVdata, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(RIVdata))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  # Check that you load a real file
  if (!ncol(RIVdata) || !nrow(RIVdata)) {
    stop("You supplied an empty file")
  }
  
  rkd_data = RIVdata[ ,c(1, 2, 427, 449, 444, 511, 18, 19, 110, 111, 509:514)]
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_clinical_data_harmonized', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}