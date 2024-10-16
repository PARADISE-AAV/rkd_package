#' @title CPD ANCA Switch
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD ANCA Switch
#' 
#' Version: 1.0
#' 
#' Date: 17-Apr-23
#'
#' @param merge_data Data after CPD_Relapse from \code{\link{ClassifyRIVEncounter}} function
#' @details
#' to be added
#' 
#' @import lubridate
#' @export

CPD_ANCA <- function(merge_data){
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))

  rkd=merge_data[,c("RKD.ID", "Date.Of.Visit", "CPD_relapse","At.any.point.ANCA.specificity", "Anti.PR3.level", "Anti.MPO.level", "ANCA.IF")]
  
  rkd$ANCA_Levels="Unknown"
  n=nrow(rkd)
  for(i in 1:n){
    if(rkd$At.any.point.ANCA.specificity[i] == "PR3"){
      rkd$ANCA_Levels[i] = rkd$Anti.PR3.level[i]
    }
    if(rkd$At.any.point.ANCA.specificity[i] == "MPO"){
      rkd$ANCA_Levels[i] = rkd$Anti.MPO.level[i]
    }
    if(rkd$At.any.point.ANCA.specificity[i] == "MPO and PR3"){
      rkd$ANCA_Levels[i] = max(rkd$Anti.MPO.level[i], rkd$Anti.PR3.level[i], na.rm=T)
    }
  }
  rkd$ANCA_Status="ANCA Status Unknown"
  n=nrow(rkd)
  for(i in 1:n){
    
    if(( is.na(rkd$ANCA_Levels[i])== FALSE & as.numeric(rkd$ANCA_Levels[i]) > 2 & rkd$ANCA_Levels[i] != "Unknown" ) & (rkd$ANCA.IF[i] == "Atypical" | rkd$ANCA.IF[i]  == "P" | rkd$ANCA.IF[i]  == "C")){
      rkd$ANCA_Status[i] = "ANCA Positive"
    }
    if(( is.na(rkd$ANCA_Levels[i])== FALSE & as.numeric(rkd$ANCA_Levels[i]) > 2 & rkd$ANCA_Levels[i] != "Unknown" ) & (rkd$ANCA.IF[i] == "Negative" )){
      rkd$ANCA_Status[i] = "ANCA Positive"
    }
    if(( is.na(rkd$ANCA_Levels[i])== FALSE & as.numeric(rkd$ANCA_Levels[i]) <= 2 & rkd$ANCA_Levels[i] != "Unknown" ) & rkd$ANCA.IF[i] == "Negative" ){
      rkd$ANCA_Status[i] = "ANCA Negative"
    }
    if(( is.na(rkd$ANCA_Levels[i])== FALSE & as.numeric(rkd$ANCA_Levels[i]) <= 2 & rkd$ANCA_Levels[i] != "Unknown" ) & (rkd$ANCA.IF[i] == "Atypical" | rkd$ANCA.IF[i]  == "P" | rkd$ANCA.IF[i]  == "C")){
      rkd$ANCA_Status[i] = "ANCA Negative"
    }
  }
  rkd$ANCA_Switch = NA
  n=nrow(rkd)
  for( i in 2:n){
    if(rkd$RKD.ID[i] == rkd$RKD.ID[i-1] & interval(rkd$Date.Of.Visit[i-1], rkd$Date.Of.Visit[i]) %/% months(1)<=18 & interval(rkd$Date.Of.Visit[i-1], rkd$Date.Of.Visit[i]) %/% months(1)>=1 &  (is.na(rkd$CPD_relapse[i])== FALSE & rkd$CPD_relapse[i] == "No Relapse") & (is.na(rkd$CPD_relapse[i-1])== FALSE & rkd$CPD_relapse[i-1] == "No Relapse")){
      if(rkd$ANCA_Status[i-1] == "ANCA Negative" & rkd$ANCA_Status[i] == "ANCA Negative"){
        rkd$ANCA_Switch[i] = "Neg-Neg Switch"
      }
      if(rkd$ANCA_Status[i-1] == "ANCA Negative" & rkd$ANCA_Status[i] == "ANCA Positive"){
        rkd$ANCA_Switch[i] = "Neg-Pos Switch"
      }
      if(rkd$ANCA_Status[i-1] == "ANCA Positive" & rkd$ANCA_Status[i] == "ANCA Positive"){
        rkd$ANCA_Switch[i] = "Pos-Pos Switch"
      }
      if(rkd$ANCA_Status[i-1] == "ANCA Positive" & rkd$ANCA_Status[i] == "ANCA Negative"){
        rkd$ANCA_Switch[i] = "Pos-Neg Switch"
      }
      if((rkd$ANCA_Status[i-1] == "ANCA Status Unknown" & rkd$ANCA_Status[i] == "ANCA Negative") | 
         (rkd$ANCA_Status[i-1] == "ANCA Status Unknown" & rkd$ANCA_Status[i] == "ANCA Positive") |
         (rkd$ANCA_Status[i-1] == "ANCA Negative" & rkd$ANCA_Status[i] == "ANCA Status Unknown") | 
         (rkd$ANCA_Status[i-1] == "ANCA Positive" & rkd$ANCA_Status[i] == "ANCA Status Unknown") |
         (rkd$ANCA_Status[i-1] == "ANCA Status Unknown" & rkd$ANCA_Status[i] == "ANCA Status Unknown")
         ){
        rkd$ANCA_Switch[i] = "Switch Status Unknown"
      }
    }
  }
  
  rkd_data=merge(rkd[,c("RKD.ID", "Date.Of.Visit", "ANCA_Levels", "ANCA_Status", "ANCA_Switch")], merge_data, by=c("RKD.ID", "Date.Of.Visit"))
  

  return(rkd_data)
}