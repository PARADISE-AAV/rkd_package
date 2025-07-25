#' @title CPD treatment switch
#' @author Matthieu COQ
#'
#' @description The goal of this function is to perform the CPD_treatment_switch
#' 
#' Version: 1.0
#' 
#' Date: 15-Aug-23
#'
#' @param merge_data Data from the merge of encounter and General characteristics in the \code{\link{CPD_Treatment}} function
#' @details
#' 
#' This function is done to compile the CPD_Treatment_Switch based on the [CPD_Treatment_Switch](https://3.basecamp.com/3790396/buckets/31062049/google_documents/8376976908) document. 
#' 
#' 
#' @import lubridate
#' @import dplyr
#' @export

CPD_treatment_discontunition = function (merge_data){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  
  rkd=merge_data
  rkd$Treatment_Switch = "Switch Status Unknown"
  n=nrow(rkd)
  for( i in 2:n){
    if(rkd$RKD.ID[i] == rkd$RKD.ID[i-1] & as.numeric(rkd$Date.Of.Visit[i] - rkd$Date.Of.Visit[i-1]) <= 550){
      if(rkd$CPD_treatment[i-1] == "Off Treatment" & rkd$CPD_treatment[i] == "Off Treatment"){
        rkd$Treatment_Switch[i-1] = "Off-Off Switch"
      }
      if(rkd$CPD_treatment[i-1] == "Off Treatment" & rkd$CPD_treatment[i] == "On Treatment"){
        rkd$Treatment_Switch[i-1] = "Off-On Switch"
      }
      if(rkd$CPD_treatment[i-1] == "Off Treatment" & rkd$CPD_treatment[i] == "Prednisolone<=10"){
        rkd$Treatment_Switch[i-1] = "Off-Pred<=10 Switch"
      }
      if(rkd$CPD_treatment[i-1] == "On Treatment" & rkd$CPD_treatment[i] == "Off Treatment"){
        rkd$Treatment_Switch[i-1] = "On-Off Switch"
      }
      if(rkd$CPD_treatment[i-1] == "On Treatment" & rkd$CPD_treatment[i] == "Prednisolone<=10"){
        rkd$Treatment_Switch[i-1] = "On-Pred<=10 Switch"
      }
      if(rkd$CPD_treatment[i-1] == "On Treatment" & rkd$CPD_treatment[i] == "On Treatment"){
        rkd$Treatment_Switch[i-1] = "On-On Switch"
      }
      if(rkd$CPD_treatment[i-1] == "Prednisolone<=10" & rkd$CPD_treatment[i] == "Off Treatment"){
        rkd$Treatment_Switch[i-1] = "Pred<=10-Off Switch"
      }
      if(rkd$CPD_treatment[i-1] == "Prednisolone<=10" & rkd$CPD_treatment[i] == "Prednisolone<=10"){
        rkd$Treatment_Switch[i-1] = "Pred<=10-Pred<=10 Switch"
      }
      if(rkd$CPD_treatment[i-1] == "Prednisolone<=10" & rkd$CPD_treatment[i] == "On Treatment"){
        rkd$Treatment_Switch[i-1] = "Pred<=10-On Switch"
      }
      
    }
    
  }
  
  return(rkd)
}