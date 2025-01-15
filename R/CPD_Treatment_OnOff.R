#' @title CPD IV Therapy treatment
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD IS Imputation for Treatment On/Off
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param IV_Therapy Data from IVTherapy from \code{\link{CPD_IVTherapy_Treatment}} function
#' @param ConMed Data from Continous medication from \code{\link{CPD_Medication_Treatment}} function
#' @param merged_data Data from the merge of encounter and General characteristics in the \code{\link{Merge_Encounter_initial}} function
#' @details
#' to be added
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @importFrom data.table %like%
#' @export

CPD_Treatment_OnOff= function(IV_Therapy, ConMed, merged_data){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merged_data))
  stopifnot("Your argument need to be a data frame"=is.data.frame(IV_Therapy))
  stopifnot("Your argument need to be a data frame"=is.data.frame(ConMed))

  
  
  
  IV_ConMed <- merge(IV_Therapy, ConMed, by = c("RKD.ID", "Date.Of.Visit"), all = TRUE)
  
  n=nrow(IV_ConMed)
  for(i in 1:n){
    if(is.null(IV_ConMed$IVtherapy[[i]])==TRUE){
      IV_ConMed$IVtherapy[i]=NA
    }
  }
  
  data_merged <- merge(merged_data, IV_ConMed, by = c("RKD.ID", "Date.Of.Visit"), all.x = TRUE)
  
  
  data_merged <- data_merged %>%
    dplyr::mutate(Step1 = dplyr::case_when(
      Immunosuppressive.status == 'Treatment Naive' & Interval.from.diagnosis..months. <= 12 ~ "Off treatment",
      Immunosuppressive.status != 'Treatment Naive' | Interval.from.diagnosis..months. > 12  ~ NA
    ))
  data_merged <- data_merged %>%
    dplyr::mutate(Step2 = dplyr::case_when(
    Current.corticosteroid.dose == "> 20 mg/day" | Current.corticosteroid.dose == "11 - 20 mg/day" ~ "On treatment",
    Current.corticosteroid.dose != "> 20 mg/day" & Current.corticosteroid.dose != "11 - 20 mg/day" ~ NA
  ))
  
  data_merged$Step3 <- NA
  n=nrow(data_merged)
  for (i in 1:n) {
    if(is.na(data_merged$IVtherapy[i]) == FALSE){
      data_merged$Step3[i] ="On treatment"
    }
  }
  
  data_merged <- data_merged %>%
    dplyr::mutate(Step4 = dplyr::case_when(
      Immunosuppressive.status == "Currently on immunosuppression" ~ "On treatment",
      Immunosuppressive.status == "Discontinuation of immunosuppression > 6 months prior to this encounter" | Immunosuppressive.status == "Discontinuation of immunosuppression within 6 months prior to this encounter" ~ "Off treatment"
    ))
  data_merged <- data_merged %>%
    dplyr::mutate(Step5 = dplyr::case_when(
      Immunosuppressive.medication != "No" & Immunosuppressive.medication != "" ~ "On treatment"
    ))
  
  data_merged$Step6 <- NA
  n=nrow(data_merged)
  for (i in 1:n){
    if(is.na(data_merged$`Dose_Prednisolone - UATC/H02AB06`[i]) == FALSE & data_merged$`Dose_Prednisolone - UATC/H02AB06`[i] <= 10){
      data_merged$Step6[i]="Prednisolone<=10"
    }
    if(is.na(data_merged$`Dose_Prednisolone - UATC/H02AB06`[i]) == FALSE & data_merged$`Dose_Prednisolone - UATC/H02AB06`[i] > 10){
      data_merged$Step6[i]="On treatment"
    }
    if((is.na(data_merged$`Drug_Avacopan (C5aR inhibitor)`[i]) == FALSE | is.na(data_merged$`Drug_Azathioprine - UATC/L04AX01`[i]) == FALSE | is.na(data_merged$`Drug_Cyclophosphamide - UATC/L01AA01`[i]) == FALSE 
       | is.na(data_merged$`Drug_Methotrexate - UATC/L01BA01`[i]) == FALSE | is.na(data_merged$`Drug_Mycophenolate mofetil - UATC/L04AA06`[i]) == FALSE 
       | is.na(data_merged$`Drug_Other`[i]) == FALSE)){
      data_merged$Step6[i]="On treatment"
    }
    if(is.na(data_merged$`Drug_Avacopan (C5aR inhibitor)`[i]) == TRUE & is.na(data_merged$`Drug_Azathioprine - UATC/L04AX01`[i]) == TRUE & is.na(data_merged$`Drug_Cyclophosphamide - UATC/L01AA01`[i]) == TRUE 
       & is.na(data_merged$`Drug_Methotrexate - UATC/L01BA01`[i]) == TRUE & is.na(data_merged$`Drug_Mycophenolate mofetil - UATC/L04AA06`[i]) == TRUE 
       & is.na(data_merged$`Drug_Other`[i]) == TRUE & is.na(data_merged$`Dose_Prednisolone - UATC/H02AB06`[i]) == TRUE){
      data_merged$Step6[i]="Treatment Status Unknown"
    }
  }
  
  data_merged$Step7 <- NA
  n=nrow(data_merged)
  for (i in 1:n){
    if(data_merged$Corticosteroids[i]=="No" & data_merged$Immunosuppressive.medication[i]=="No"){
      data_merged$Step7[i]="Off Treatment"
    }
  }
  
  data_merged$CPD_treatment <- "Manual review"
  n=nrow(data_merged)
  for (i in 1:n){
    if(is.na(data_merged$Step3[i]) == FALSE){
      data_merged$CPD_treatment[i]="On Treatment"
    }else{
      if(is.na(data_merged$Step1[i]) == FALSE   & (is.na(data_merged$Step5[i])==T) & data_merged$Step6[i] == "Treatment Status Unknown" ){
        data_merged$CPD_treatment[i]="Off Treatment"
      }
      if(is.na(data_merged$Step1[i]) == TRUE  &  (is.na(data_merged$Step4[i])==FALSE & data_merged$Step4[i] == "On treatment") & data_merged$Step6[i] == "On treatment"){
        data_merged$CPD_treatment[i]="On Treatment"
      }
      if (is.na(data_merged$Step1[i]) == TRUE & is.na(data_merged$Step2[i]) == TRUE &  (is.na(data_merged$Step4[i])==TRUE ) & (is.na(data_merged$Step5[i])==TRUE) & data_merged$Step6[i] == "Treatment Status Unknown"){
        data_merged$CPD_treatment[i]="Treatment Status Unknown"
      }
      if (is.na(data_merged$Step1[i]) == TRUE & is.na(data_merged$Step2[i]) == TRUE &  (is.na(data_merged$Step4[i])==TRUE | data_merged$Step4[i] == "Off treatment") & (is.na(data_merged$Step5[i])==TRUE | data_merged$Step5[i] == "Off treatment") & data_merged$Step6[i] == "Prednisolone<=10" & is.na(data_merged$Step7[i]) == TRUE){
        data_merged$CPD_treatment[i]="Prednisolone<=10"
      }
      if(is.na(data_merged$Step1[i]) == TRUE   & (is.na(data_merged$Step4[i])==TRUE | data_merged$Step4[i] == "Off treatment") & data_merged$Step6[i] == "Treatment Status Unknown" & is.na(data_merged$Step7[i]) == FALSE){
        data_merged$CPD_treatment[i]="Off Treatment"
      }
      if(is.na(data_merged$Step1[i]) == TRUE & is.na(data_merged$Step2[i]) == TRUE  & (is.na(data_merged$Step4[i])==FALSE & data_merged$Step4[i] == "Off treatment") & (is.na(data_merged$Step5[i])==TRUE ) & data_merged$Step6[i] == "Treatment Status Unknown" ){
        data_merged$CPD_treatment[i]="Off Treatment"
      }
      if(is.na(data_merged$Step1[i]) == TRUE   & (is.na(data_merged$Step4[i])==FALSE & data_merged$Step4[i] == "On treatment")  & (data_merged$Step6[i] == "Treatment Status Unknown" | data_merged$Step6[i] == "On Treatment") & is.na(data_merged$Step7[i]) == TRUE ){
        data_merged$CPD_treatment[i]="On Treatment"
      }
    }
    
  }
  
  data_merged$IVtherapy=as.character(data_merged$IVtherapy)
  
  
  
  return(data_merged)
  
  
}