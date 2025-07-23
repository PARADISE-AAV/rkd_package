#' @title CPD IS medication
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD IS medication
#' 
#' Version: 1.0
#' 
#' Date: 9-Aug-24
#'
#' @param treatment_data Data from CPD_treatment from \code{\link{CPD_Treatment}} function
#' @details
#' To perform the CPD IS Medication, we take the list from \code{\link{CPD_Treatment}} function and extract each medication based on code overview.
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @importFrom data.table %like%
#' @export

CPD_immunosup_med <- function(treatment_data){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(treatment_data))
  
  
  treatment_data$immunosup_med_1="No"
  treatment_data$immunosup_med_1[grep("Cyclo", treatment_data$treatment)]="Yes"
  
  treatment_data$immunosup_med_2="No"
  treatment_data$immunosup_med_2[grep("Mycophenolate", treatment_data$treatment)]="Yes"
  
  treatment_data$immunosup_med_3="No"
  treatment_data$immunosup_med_3[grep("Azathioprine", treatment_data$treatment)]="Yes"
  
  treatment_data$immunosup_med_4="No"
  treatment_data$immunosup_med_4[grep("Methotrexate", treatment_data$treatment)]="Yes"
  
  treatment_data$immunosup_med_5="No"
  treatment_data$immunosup_med_5[grep("Leflunomide", treatment_data$treatment)]="Yes"
  
  treatment_data$immunosup_med_6="No"
  treatment_data$immunosup_med_6[grep("Rituximab", treatment_data$treatment)]="Yes"
  
  treatment_data$immunosup_med_7="No"
  treatment_data$immunosup_med_7[grep("Ustekinumab", treatment_data$treatment)]="Yes"
  
  treatment_data$immunosup_med_8="No"
  treatment_data$immunosup_med_8[grep("Tacrolimus", treatment_data$treatment)]="Yes"
  
  treatment_data$immunosup_med_9="No"
  treatment_data$immunosup_med_9[grep("Mepolizumab", treatment_data$treatment)]="Yes"
  
  treatment_data$immunosup_med_10="No"
  treatment_data$immunosup_med_10[grep("Avacopan", treatment_data$treatment)]="Yes"
  
  treatment_data$immunosup_med_11="No"
  treatment_data$immunosup_med_11[grep("Other", treatment_data$treatment)]="Yes"
  
  treatment_data$immunosup_med_12="No"
  treatment_data$immunosup_med_12[which(is.na(treatment_data$treatment)==TRUE)]="Yes"
  
  
   return(treatment_data)
  
}