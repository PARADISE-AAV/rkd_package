#' @title CPD Medication treatment
#' @author Matthieu COQ
#'
#' @description The Goal is to prepare the continuous medication frame for Treatment On/Off
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#' @param Medication  {"name": "Medication","desc": "RIV data from \code{\link{CPD_Continuous_Medication_interval}} function","options": (),"type": "file"}
#' @param merged_data  {"name": "merged_data","desc": "RIV data from \code{\link{Merge_Encounter_initial}} function","options": (),"type": "file"}
#' @param output_dir  {"name": "output_dir","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#'
#' @details
#' 
#' This function is to merge the encounter frame and the Continuous Medication frame with the goal to know if an encounter is under a medication. More information are in the folowing document, [CPD_Treatment](https://3.basecamp.com/3790396/buckets/31062049/cloud_files/7631150656
#' 
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
  
  Medication1= Medication
  
  medication_inputed <- Medication1[which(Medication1$Drug!=""), c("RKD.ID", "Drug", "Dose", "Start.Date", "Stop.Date")]
  
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