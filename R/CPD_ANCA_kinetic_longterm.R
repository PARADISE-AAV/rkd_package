#' @title CPD ANCA kinetic longterm
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD ANCA kinetic longterm
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param merge_data Data from the \code{\link{CPD_ANCA}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @export

CPD_ANCA_kinetics <- function(merge_data,output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  
  merge_data$anca_kinetics_longterm <- "Unknown ANCA level"
  n=nrow(merge_data)
  for(i in 3:n){
    if(merge_data$RKD.ID[i]==merge_data$RKD.ID[i-1] & merge_data$RKD.ID[i]==merge_data$RKD.ID[i-2]){
      if(merge_data$ANCA_Status[i]=="ANCA Positive" & merge_data$ANCA_Status[i-1]=="ANCA Positive" & merge_data$ANCA_Status[i-2]=="ANCA Positive"){
        merge_data$anca_kinetics_longterm[i] <- "persistent positive"
      }
      if(merge_data$ANCA_Status[i]=="ANCA Negative" & merge_data$ANCA_Status[i-1]=="ANCA Negative" & merge_data$ANCA_Status[i-2]=="ANCA Negative"){
        merge_data$anca_kinetics_longterm[i] <- "persistent negative"
      }
      if(merge_data$ANCA_Status[i]!="ANCA Status Unknown" & merge_data$ANCA_Status[i-1]!="ANCA Status Unknown" & merge_data$ANCA_Status[i-2]!="ANCA Status Unknown" & merge_data$anca_kinetics_longterm[i] == "Unknown ANCA level"){
        merge_data$anca_kinetics_longterm[i] <- "Variable ANCA level"
      }
    }
  }
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_ANCA_kinetics longterm_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(merge_data, output_filename, row.names = FALSE)
  return(merge_data)
  
}