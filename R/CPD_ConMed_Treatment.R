#' @title CPD IV Therapy treatment
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD IS Imputation for Treatment On/Off
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param Medication Data from Encounter from \code{\link{CPD_IVTherapy}} function
#' @param merged_data Data from the merge of encounter and General characteristics in the \code{\link{Merge_Encounter_initial}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @export

CPD_Medication_Treatment= function(Medication, merged_data, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  stopifnot("Your argument need to be a data frame"=is.data.frame(Medication))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  medication_filter <- Medication[which(Medication$Drug!=""), c("RKD.ID", "Drug", "Dose", "Start.Date", "Stop.Date")]
  
  medication_last <- merge(merged_data[!duplicated(merged_data[,c("RKD.ID", "Date_Last_Follow_up")]),c("RKD.ID", "Date_Last_Follow_up")], medication_filter, by="RKD.ID")
  
  n=length(levels(as.factor(medication_last$RKD.ID)))
  medication_inputed=NULL
  for(i in 1:n){
    dat=medication_last[which(medication_last$RKD.ID ==levels(as.factor(medication_last$RKD.ID))[i] ),]
    dat1=dat[order(c(dat$Start.Date)),]
    dat2=dat1[order(c(dat1$Drug)),]
    for(j in 1:nrow(dat2)){
      if(is.na(dat2$Stop.Date[j])==TRUE & is.na(dat2$Start.Date[j])==FALSE){
          if(j == nrow(dat2) & interval(dat2$Start.Date[j], dat2$Date_Last_Follow_up[j]) %/% months(1)<=6){
            dat2$Stop.Date[j]= dat2$Date_Last_Follow_up[j]
          }else{
            if(dat2$Drug[j] == dat2$Drug[j+1] & interval(dat2$Start.Date[j], dat2$Date_Last_Follow_up[j]) %/% months(1)<=6)
              dat2$Stop.Date[j] = dat2$Start.Date[j+1]
          }
      }
    }
    medication_inputed=rbind(medication_inputed, dat2)
  }
  
}