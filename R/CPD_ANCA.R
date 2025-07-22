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
#' This function is done to compile the CPD_ANCASwitch based on the [CPD_ANCA_Switch](https://3.basecamp.com/3790396/buckets/31062049/google_documents/8234378355)
#' 
#' @import lubridate
#' @export

CPD_ANCA <- function(merge_data){
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))

  rkd=merge_data[,c("RKD.ID", "Date.Of.Visit", "CPD_relapse","At.any.point.ANCA.specificity", "Anti.PR3.level", "Anti.MPO.level", "ANCA.IF")]
  
  rkd$ANCA_Levels=NA
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
    if(is.na(rkd$ANCA_Levels[i])== FALSE & rkd$ANCA_Levels[i] == -Inf){
      rkd$ANCA_Levels[i]=NA
    }
  }
  
  
  
  rkd$ANCA_Status="ANCA Status Unknown"
  n=nrow(rkd)
  for(i in 1:n){
    
    if( is.na(rkd$ANCA_Levels[i])== FALSE & as.numeric(rkd$ANCA_Levels[i]) > 2){
      rkd$ANCA_Status[i] = "ANCA Positive"
    }else{
      if( is.na(rkd$ANCA_Levels[i])== FALSE & as.numeric(rkd$ANCA_Levels[i]) <= 2){
        rkd$ANCA_Status[i] = "ANCA Negative"
      }else{
        if(rkd$ANCA.IF[i]=="Negative"){
          rkd$ANCA_Status[i] = "ANCA Negative"
        }else{
          if(rkd$ANCA.IF[i]=="P" | rkd$ANCA.IF[i]=="C" | rkd$ANCA.IF[i]=="Atypical"){
            rkd$ANCA_Status[i] = "ANCA Positive"
          }
        }
      }
    }
        
    
  }
  
  n=length(levels(as.factor(rkd$RKD.ID)))
  Encounter2=NULL
  for(i in 1:n){
    dat=rkd[which(rkd$RKD.ID==levels(as.factor(rkd$RKD.ID))[i]),]
    m=nrow(dat)
    if(m>2){
      for(j in 2:m){
        if(dat$ANCA_Status[j]=="ANCA Status Unknown" ){
          k=j
          while((dat$ANCA_Status[k]=="ANCA Status Unknown" ) & k<m){
            k=k+1
          }
          if(as.numeric(dat$Date.Of.Visit[k]-dat$Date.Of.Visit[j-1])<=400 & dat$ANCA_Status[k]==dat$ANCA_Status[j-1]){
            dat$ANCA_Status[j:k-1]=dat$ANCA_Status[j-1]
          }
        }
      }
    }
    Encounter2=rbind(Encounter2,dat)
  } 
  
  rkd=Encounter2
  
  
  rkd$ANCA_Switch = "Switch Status Unknown"
  n=nrow(rkd)
  for( i in 2:n){
    if(rkd$RKD.ID[i] == rkd$RKD.ID[i-1] & as.numeric(rkd$Date.Of.Visit[i] - rkd$Date.Of.Visit[i-1]) <= 550  &  (is.na(rkd$CPD_relapse[i])== FALSE & rkd$CPD_relapse[i] != "Definite Relapse") & (is.na(rkd$CPD_relapse[i-1])== FALSE & rkd$CPD_relapse[i-1] != "Definite Relapse")){
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
      
    }
    if((is.na(rkd$CPD_relapse[i])== FALSE & rkd$CPD_relapse[i] == "Definite Relapse") | (is.na(rkd$CPD_relapse[i-1])== FALSE & rkd$CPD_relapse[i-1] == "Definite Relapse")){
      rkd$ANCA_Switch[i] = "Relapse"
    }
  }
  
  rkd_data=merge(rkd[,c("RKD.ID", "Date.Of.Visit", "ANCA_Levels", "ANCA_Status", "ANCA_Switch")], merge_data, by=c("RKD.ID", "Date.Of.Visit"))
  

  return(rkd_data)
}