#' @title CPD treatment discontunition
#' @author Matthieu COQ
#'
#' @description to be added
#' 
#' Version: 1.0
#' 
#' Date: 15-Aug-23
#'
#' @param merge_data Data from the merge of encounter and General characteristics in the \code{\link{CPD_Treatment}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' @import lubridate
#' @import dplyr
#' @export

CPD_treatment_discontunition = function (merge_data, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))

  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  merge_data$Step1_TD = NA
  merge_data$Step2_TD = NA
  merge_data$Step3_TD = NA
  merge_data$Step4_TD = NA
  merge_data$Step5_TD = NA
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_treatment_discontinuation_function_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(merge_LTROT, output_filename, row.names = FALSE)
  return(merge_LTROT)
}