#' @title CPD current LTROT
#' @author Matthieu COQ
#'
#' @description to be added
#' 
#' Version: 1.0
#' 
#' Date: 15-Aug-23
#'
#' @param merge_data Data from the merge of encounter and General characteristics in the \code{\link{CPD_Treatment}} function
#' @param interval Number of day to be off treatment
#' @details
#' to be added
#' @import lubridate
#' @import dplyr
#' @export

CPD_LTROT_current <- function(merge_data, interval=730){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  stopifnot("Your argument need to be a numeric"=is.numeric(interval))
  
  
  
  merge_data1=merge_data[which(is.na(merge_data$CPD_relapse)==FALSE),]
  merge_data1$LTROT_current=NA
  merge_data1$LTROT_current_level1=NA
  merge_data1$LTROT_current_level2=NA
  merge_data_LTROT=NULL
  n=length(levels(as.factor(merge_data1$RKD.ID)))
  for (i in 1:n){
    dat <- merge_data1[which(merge_data1$RKD.ID == levels(as.factor(merge_data1$RKD.ID))[i] ),]
    if(as.numeric(difftime(max(dat$Date.Of.Visit), min(dat$Date.Of.Visit)))>=interval & max(dat$interval_from_diagnosis)>=interval){
      dat1 <- dat[which(dat$interval_from_diagnosis >= interval),]
      m=nrow(dat1)
      for(j in 1:m){
        intermax=dat1$interval_from_diagnosis[j]
        dat3 <- dat[which(dat$interval_from_diagnosis<=intermax & dat$interval_from_diagnosis>=intermax-interval),]
        if(dim(table(dat3$CPD_relapse))==1 & dat3$CPD_relapse[1]=="No Relapse" & dim(table(dat3$CPD_treatment))==1 & dat3$CPD_treatment[1]=="Off Treatment"){
            dat3$LTROT_current[nrow(dat3)]="LTROT"
        }
        if(dim(table(dat3$CPD_relapse))==1 & dat3$CPD_relapse[1]=="No Relapse" & dim(table(dat3$CPD_treatment))==1 & dat3$CPD_treatment[1]=="Off Treatment" & dat3$ANCA_Status[nrow(dat3)]=="ANCA Negative"){
          dat3$LTROT_current_level1[nrow(dat3)]="LTROT"
        }
        if(dim(table(dat3$CPD_relapse))==1 & dat3$CPD_relapse[1]=="No Relapse" & dim(table(dat3$CPD_treatment))==1 & dat3$CPD_treatment[1]=="Off Treatment" & dat3$ANCA_Status[nrow(dat3)]=="ANCA Positive"){
          dat3$LTROT_current_level2[nrow(dat3)]="LTROT"
        }
         
        merge_data_LTROT=rbind(merge_data_LTROT,dat3[,c("RKD.ID", "Date.Of.Visit", "LTROT_current", "LTROT_current_level1", "LTROT_current_level2")])
      }
    }
    
  }
  merge_data_LTROT=merge_data_LTROT[!duplicated(merge_data_LTROT[,c(1:2)]),]
  
  merge_LTROT=merge(merge_data, merge_data_LTROT, by = c("RKD.ID", "Date.Of.Visit"), all.x = TRUE)
  
  return(merge_LTROT)
  
}