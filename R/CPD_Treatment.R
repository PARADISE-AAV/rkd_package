#' @title CPD IS medication
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD Treatment
#' 
#' Version: 1.0
#' 
#' Date: 9-Aug-24
#'
#' @param treatment_data Data from IVTherapy from \code{\link{CPD_Treatment_OnOff}} function
#' @details
#' to be added
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @importFrom data.table %like%
#' @export

CPD_Treatment = function(treatment_data){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(treatment_data))
  
  
  treatment_data$treatment = NA
  n=nrow(treatment_data)
  for(i in 1:n){
    if(is.na(treatment_data$IV.therapy.x[i])==FALSE){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$IV.therapy.x[i], sep="")
    }
    if(is.na(treatment_data$IV.therapy.y[i])==FALSE){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$IV.therapy.y[i], sep="")
    }
    if(is.na(treatment_data$IV.therapy[i])==FALSE){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$IV.therapy[i], sep="")
    }
    if(is.na(treatment_data$`Drug_Avacopan (C5aR inhibitor)`[i])==FALSE){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$`Drug_Avacopan (C5aR inhibitor)`[i], sep="")
    }
    if(is.na(treatment_data$`Drug_Azathioprine - UATC/L04AX01`[i])==FALSE){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$`Drug_Azathioprine - UATC/L04AX01`[i], sep="")
    }
    if(is.na(treatment_data$`Drug_Cyclophosphamide - UATC/L01AA01`[i])==FALSE){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$`Drug_Cyclophosphamide - UATC/L01AA01`[i], sep="")
    }
    if(is.na(treatment_data$`Drug_Methotrexate - UATC/L01BA01`[i])==FALSE){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$`Drug_Methotrexate - UATC/L01BA01`[i], sep="")
    }
    if(is.na(treatment_data$`Drug_Mycophenolate mofetil - UATC/L04AA06`[i])==FALSE){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$`Drug_Mycophenolate mofetil - UATC/L04AA06`[i], sep="")
    }
    if(is.na(treatment_data$`Drug_Prednisolone - UATC/H02AB06`[i])==FALSE){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$`Drug_Prednisolone - UATC/H02AB06`[i], sep="")
    }
    if(is.na(treatment_data$`Drug_Other`[i])==FALSE){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$`Drug_Other`[i], sep="")
    }
    if(treatment_data$Immunosuppressive.medication[i]!="" & treatment_data$Immunosuppressive.medication[i]!="No"){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$Immunosuppressive.medication[i], sep="")
    }
    if(treatment_data$Additional.Immunosuppressive.medication[i]!="" & treatment_data$Additional.Immunosuppressive.medication[i]!="No"){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$Additional.Immunosuppressive.medication[i], sep="")
    }
    if(treatment_data$Additional.Immunosuppressive.medication.1[i]!="" & treatment_data$Additional.Immunosuppressive.medication.1[i]!="No"){
      treatment_data$treatment[i] = paste(treatment_data$treatment[i], ", ", treatment_data$Additional.Immunosuppressive.medication.1[i], sep="")
    }
    
  }
  

  return(treatment_data)
  
}