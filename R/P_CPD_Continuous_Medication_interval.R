#' @title CPD Continuous medication
#' @author Matthieu COQ
#'
#' @description The Goal is to merge the Continuous medication and general characteristics from \code{\link{SplitRIV}} function
#' 
#' Version: 1.0
#' 
#' Date: 17-Apr-23
#' @param Medication_data  {"name": "Medication_data","desc": "RIV data from \code{\link{SplitRIV}} function","options": (),"type": "file"}
#' @param merged_data RIV data from \code{\link{Merge_Encounter_initial}} function
#' @param output_dir  {"name": "output_dir","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#' @details
#' 
#' The function calculated the interval in days between the diagnosis and the start data and stop date of continuous medication. The dose is recalculated to have a daily dose. 
#' 
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @importFrom rlang .data
#' @export

CPD_Continuous_Medication_interval <- function (Medication_data, merged_data ,output_dir){
  stopifnot("Your argument need to be a data frame"=is.list(Medication_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }

  
  rkd_data <- merge(Medication_data$Medication, Medication_data$Initial[,c("RKD.ID", "Date.of.diagnosis")], by="RKD.ID")
  rkd_data$medstart_interval_from_diagnosis <- as.numeric(rkd_data$Start.Date-rkd_data$Date.of.diagnosis)
  rkd_data$medstop_interval_from_diagnosis <- as.numeric(rkd_data$Stop.Date-rkd_data$Date.of.diagnosis)
  
  n=nrow(rkd_data)
  for(i in 1:n){
    if(rkd_data$Frequency[i]=="" | rkd_data$Frequency[i]=="Daily" | rkd_data$Frequency[i]=="Once daily" | rkd_data$Frequency[i]=="Other"){
      rkd_data$Dose[i]=rkd_data$Dose[i]
    }
    if(rkd_data$Frequency[i]=="Twice daily"){
      rkd_data$Dose[i]=2*rkd_data$Dose[i]
    }
    if(rkd_data$Frequency[i]=="Three times daily"){
      rkd_data$Dose[i]=3*rkd_data$Dose[i]
    }
    if(rkd_data$Frequency[i]=="Four times daily"){
      rkd_data$Dose[i]=4*rkd_data$Dose[i]
    }
    if(rkd_data$Frequency[i]=="Five times daily"){
      rkd_data$Dose[i]=5*rkd_data$Dose[i]
    }
    if(rkd_data$Frequency[i]=="Once weekly"){
      rkd_data$Dose[i]=1/7*rkd_data$Dose[i]
    }
    if(rkd_data$Frequency[i]=="Twice weekly"){
      rkd_data$Dose[i]=2/7*rkd_data$Dose[i]
    }
    if(rkd_data$Frequency[i]=="Three times weekly"){
      rkd_data$Dose[i]=3/7*rkd_data$Dose[i]
    }
    if(rkd_data$Frequency[i]=="Annually"){
      rkd_data$Dose[i]=1/365*rkd_data$Dose[i]
    }
    if(rkd_data$Frequency[i]=="Once monthly"){
      rkd_data$Dose[i]=1/30*rkd_data$Dose[i]
    }
    if(rkd_data$Frequency[i]=="Twice monthly"){
      rkd_data$Dose[i]=2/30*rkd_data$Dose[i]
    }
    if(rkd_data$Frequency[i]=="Once every 6 months"){
      rkd_data$Dose[i]=2/365*rkd_data$Dose[i]
    }
  }
  
  Medication1= rkd_data[-which(rkd_data$Drug=="Other" & rkd_data$Drug..ATC==""),]
  
  medication_filter <- Medication1[which(Medication1$Drug!=""), c("RKD.ID", "Drug", "Dose", "Unit.of.Doses", "Start.Date", "Stop.Date")]
  
  medication_last <- merge(merged_data[!duplicated(merged_data[,c("RKD.ID", "Date_Last_Follow_up")]),c("RKD.ID", "Date_Last_Follow_up")], medication_filter, by="RKD.ID")
  n=length(levels(as.factor(medication_last$RKD.ID)))
  medication_inputed=NULL
  for(i in 1:n){
    dat=medication_last[which(medication_last$RKD.ID ==levels(as.factor(medication_last$RKD.ID))[i] ),]
    dat1=dat[order(c(dat$Start.Date)),]
    dat2=dat1[order(c(dat1$Drug)),]
    for(j in 1:nrow(dat2)){
      if(is.na(dat2$Stop.Date[j])==TRUE & is.na(dat2$Start.Date[j])==FALSE & is.na(dat2$Date_Last_Follow_up[j])==FALSE){
        if(j == nrow(dat2) & interval(dat2$Start.Date[j], dat2$Date_Last_Follow_up[j]) / months(1)<=6){
          dat2$Stop.Date[j]= dat2$Date_Last_Follow_up[j]
        }else{
          if(dat2$Drug[j] == dat2$Drug[j+1] & interval(dat2$Start.Date[j], dat2$Date_Last_Follow_up[j]) / months(1)<=6)
            dat2$Stop.Date[j] = dat2$Start.Date[j+1]
        }
      }
    }
    medication_inputed=rbind(medication_inputed, dat2)
  }
  
  n=legth(medication_inputed$Unit.of.Doses)
  for(i in 1:n){
    if(is.na(medication_inputed$Unit.of.Doses[i]) == FALSE & medication_inputed$Unit.of.Doses[i] == "g"){
      medication_inputed$Dose[i]=medication_inputed$Dose[i]*1000
    } 
  }
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_countinuous_medication_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}