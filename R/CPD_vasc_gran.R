#' @title CPD vascvs gran
#' @author Matthieu COQ
#'
#' @description to be added
#' 
#' Version: 1.0
#' 
#' Date: 15-Aug-23
#'
#' @param merge_data Data from the merge of encounter and General characteristics in the \code{\link{CPD_Treatment}} function
#' @details
#' to be added
#' 
#' @import lubridate
#' @import dplyr
#' @export

CPD_vasc_vs_gran <- function(merge_data){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  
  
  n=nrow(merge_data)
  for(i in 1:n){
    
  }
  
  return(merge_data)
}