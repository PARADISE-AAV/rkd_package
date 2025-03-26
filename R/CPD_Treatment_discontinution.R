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
  
  rkd$Treatment_Switch = "Switch Status Unknown"
  n=nrow(rkd)
  for( i in 2:n){
    if(rkd$RKD.ID[i] == rkd$RKD.ID[i-1] & as.numeric(rkd$Date.Of.Visit[i] - rkd$Date.Of.Visit[i-1]) <= 550){
      if(rkd$ANCA_Status[i-1] == "ANCA Negative" & rkd$ANCA_Status[i] == "ANCA Negative"){
        rkd$Treatment_Switch[i-1] = "Neg-Neg Switch"
      }
      if(rkd$ANCA_Status[i-1] == "ANCA Negative" & rkd$ANCA_Status[i] == "ANCA Positive"){
        rkd$Treatment_Switch[i-1] = "Neg-Pos Switch"
      }
      if(rkd$ANCA_Status[i-1] == "ANCA Positive" & rkd$ANCA_Status[i] == "ANCA Positive"){
        rkd$Treatment_Switch[i-1] = "Pos-Pos Switch"
      }
      if(rkd$ANCA_Status[i-1] == "ANCA Positive" & rkd$ANCA_Status[i] == "ANCA Negative"){
        rkd$Treatment_Switch[i-1] = "Pos-Neg Switch"
      }
      
    }
    
  }
  
  return(merge_LTROT)
}