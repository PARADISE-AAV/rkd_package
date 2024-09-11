#' @title CPD Corticosteroids on/off
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

CPD_Corticosteroids_on_off <- function(merge_data, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  
  merge_data <- merge_data %>%
    dplyr::mutate(Step1 = dplyr::case_when(
      Corticosteroids =="Yes" ~ "On",
      Corticosteroids =="No"  ~ "Off"
    ))
  a=as.data.frame(grep("prednisolone", merge_data$treatment))
  b=as.data.frame(which(merge_data$Corticosteroids_On_off==""))
  colnames(a)="x"
  colnames(b)="x"
  c=inner_join(a,b)
  merge_data$Corticosteroids_On_off[ as.numeric(c$x)]="On"
  
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_CPD_Corticosteroids_on_off_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(merge_data, output_filename, row.names = FALSE)
  return(merge_data)
  
}