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
#' @details
#' to be added
#' @import lubridate
#' @import dplyr
#' @export

CPD_treatment_discontunition = function (merge_data){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  
  
  merge_data$Step1_TD = NA
  merge_data$Step2_TD = NA
  merge_data$Step3_TD = NA
  merge_data$Step4_TD = NA
  merge_data$Step5_TD = NA
  
  
  
  
  return(merge_LTROT)
}