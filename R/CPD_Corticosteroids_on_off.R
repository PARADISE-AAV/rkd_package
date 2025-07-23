#' @title CPD Corticosteroids on/off
#' @author Matthieu COQ
#'
#' @description The goal is to performed the CPD Corticosteroids on/off as an imputation of the corticosteroid variable.
#' 
#' Version: 1.0
#' 
#' Date: 15-Aug-23
#'
#' @param merge_data Data from the merge of encounter and General characteristics in the \code{\link{CPD_Treatment}} function
#' @details
#' This function is done to compile the CPD_Corticosteroid_on_off based on the [CPD_Corticosteroid_on_off](https://3.basecamp.com/3790396/buckets/31062049/google_documents/7798594419) documents. 
#' 
#' 
#' @import lubridate
#' @import dplyr
#' @export

CPD_Corticosteroids_on_off <- function(merge_data){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  
  merge_data <- merge_data %>%
    dplyr::mutate(Corticosteroids_On_off = dplyr::case_when(
      Corticosteroids =="Yes" ~ "On",
      Corticosteroids =="No"  ~ "Off",
      Corticosteroids ==""  ~ "",
    ))
  a=as.data.frame(grep("rednisolone", merge_data$treatment))
  b=as.data.frame(which(merge_data$Corticosteroids_On_off==""))
  colnames(a)="x"
  colnames(b)="x"
  c=inner_join(a,b)
  merge_data$Corticosteroids_On_off[ as.numeric(c$x)]="On"
  
  return(merge_data)
  
}