#' @title This function use the different algorithms to filter the patient of RIV data
#' @author Matthieu COQ
#' @description
#' The objective is to choose the dataframe from \code{\link{SplitRIV}}
#' 
#' Version: 1.0
#' 
#' Date: 03-Jan-24
#'
#' @param RKDdata a list from \code{\link{SplitRIV}} function
#' @param algorithm data frame to choose, the possibility are "Encounter" or "Initial" or "IVTherapy" or "Medication" or "Biopsy" or "Transplant" or "Complication"
#' @return export the dataframe 
#' @details 
#' to be added later
#' 
#' @export

SplitRIV_dataframe <- function(RKDdata, output_dir, algorithm){
  if (is.list(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a list argument")
  }
  
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  algorithm <- match.arg(algorithm, c("Encounter", "Initial" , "IVTherapy" , "Medication" ,"Biopsy" , "Transplant" ,"Complication"))
  
  RKD_data=RKDdata[[which(names(RKDdata)==algorithm)]]
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_Split_frame_',algorithm, "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(RKD_data, output_filename, row.names = FALSE)
  
  return(RKD_data)
}