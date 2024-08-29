#' @title CPD Severity of Definite Relapse
#' @author Matthieu COQ
#'
#' @description to be added
#' 
#' Version: 1.0
#' 
#' Date: 15-Aug-23
#'
#' @param merge_data Data from the merge of encounter and General characteristics in the \code{\link{CPDRelapse}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' @import lubridate
#' @import dplyr
#' @export

CPD_Severity_Relapse <- function(merge_data, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  merge_data <- merge_data %>%
    dplyr::mutate(relapse_severity = dplyr::case_when(
      CPD_relapse == "Definite Relapse" &  ~ "Off treatment",
      Immunosuppressive.medication != "No" & Immunosuppressive.medication != "" ~ "On treatment"
    ))
  
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_ltrot_current_function_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(merge_LTROT, output_filename, row.names = FALSE)
  return(merge_LTROT)
  
}