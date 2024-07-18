#' @title CPD current LTROT
#' @author Matthieu COQ
#'
#' @description to be added
#' 
#' Version: 1.0
#' 
#' Date: 17-Apr-23
#'
#' @param merge_data Data from the merge of encounter and General characteristics in the \code{\link{Merge_Encounter_initial}} function
#' @param output_dir folder where the Redcap data will be saved
#' @param interval Number of month to be off treatment
#' @details
#' to be added
#' @import lubridate
#' @export

CPD_LTROT_current <- function(merge_data, interval, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  stopifnot("Your argument need to be a numeric"=is.numeric(interval))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  n=length(levels(as.factor(merge_data$RKD.ID)))
  for (i in 1:n){
    dat <- merge_data[which(merge_data$RKD.ID == levels(as.factor(merge_data$RKD.ID))[i] ),]
    if(interval(min(dat$Date.Of.Visit), max(dat$Date.Of.Visit)) %/% months(1)>=interval){
      
    }
  }
  
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_ltrot_current_function_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}