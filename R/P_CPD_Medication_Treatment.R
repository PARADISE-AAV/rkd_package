#' @title CPD IV Therapy treatment
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD IS Imputation for Treatment On/Off
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#' @param Medication  {"name": "Medication","desc": "RIV data from \code{\link{CPD_Continuous_Medication_interval}} function","options": (),"type": "file"}
#' @param merged_data  {"name": "rkd_data","desc": "RIV data from \code{\link{Merge_Encounter_initial}} function","options": (),"type": "file"}
#' @param output_dir  {"name": "output_dir","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#'
#' @details
#' to be added
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @export

CPD_Medication_Treatment= function(Medication, merged_data, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merged_data))
  stopifnot("Your argument need to be a data frame"=is.data.frame(Medication))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  Medication1= Medication[-which(Medication$Drug=="Other" & Medication$Drug..ATC==""),]
  
  medication_filter <- Medication1[which(Medication1$Drug!=""), c("RKD.ID", "Drug", "Dose", "Start.Date", "Stop.Date")]
  
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
  
  
  medication_inputed$Start.date.of.date.range <-
    as.Date(medication_inputed$Start.Date)
  
  medication_inputed$End.date.of.date.range <-
    as.Date(medication_inputed$Stop.Date)
  
  
  merged_frame <-fuzzy_inner_join(
    merged_data[, c("RKD.ID", "Date.Of.Visit")], medication_inputed,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Start.date.of.date.range",
      "Date.Of.Visit" = "End.date.of.date.range"
    ),
    match_fun = list(`==`, `>`, `<=`)
  ) %>%
    select(everything())
  
  rownames(merged_frame) <- NULL
  
  colnames(merged_frame)[1] <- "RKD.ID"
  
  drug=levels(as.factor(merged_frame$Drug))
  drug_all=c(drug, c("Avacopan (C5aR inhibitor)", "Azathioprine - UATC/L04AX01", "Cyclophosphamide - UATC/L01AA01",
                 "Methotrexate - UATC/L01BA01", "Mycophenolate mofetil - UATC/L04AA06", "Other", "Prednisolone - UATC/H02AB06"))
  drug=drug_all[!duplicated(drug_all)]
  
  n <- length(drug)
  merged_frame_drug <- merged_frame[, c("RKD.ID", "Date.Of.Visit")]
  for (i in 1:n){
    dat <- merged_frame[which(merged_frame$Drug == drug[i]), c("RKD.ID", "Date.Of.Visit", "Drug", "Dose")]
    colnames(dat)[3:4] <- paste(colnames(dat)[3:4], drug[i], sep="_")
    merged_frame_drug <- merge(merged_frame_drug, dat, by = c("RKD.ID", "Date.Of.Visit"), all.x = TRUE)
    
  }
  
  merged_frame_all <- merged_frame_drug 
  
  merged_frame_all <- merged_frame_all[order(merged_frame_all$RKD.ID, merged_frame_all$Date.Of.Visit, merged_frame_all$`Dose_Prednisolone - UATC/H02AB06`),]
  
  merged_frame_unique <- merged_frame_all[!duplicated(merged_frame_all[,c(1:2)],fromLast = T), ]
  
  return(merged_frame_unique)
  
}