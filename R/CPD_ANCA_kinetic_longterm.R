#' @title CPD ANCA kinetic longterm
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD ANCA kinetic longterm
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param merged_data Data from the \code{\link{CPD_ANCA}} function
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
      
    }
  }
  
  
}