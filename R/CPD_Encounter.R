#' @title CPD Encounter
#' @author Matthieu COQ
#'
#' @description The Goal is to get modification done before the merge with general characteristics from \code{\link{SplitRIV}} function
#' 
#' Version: 1.0
#' 
#' Date: 18-Apr-23
#'
#' @param Encounter_data General Characteristics data from \code{\link{SplitRIV}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @importFrom rlang .data
#' @export
CPD_Encounter <- function (Encounter_data, output_dir){
  stopifnot("Your argument need to be a data frame"=is.data.frame(Encounter_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  rkd_data <- Encounter_data
  for (i in 1:16){
    a=paste("immunosup_med_",i,sep="")
    rkd_data$b=NA
    colnames(rkd_data)[which(colnames(rkd_data)=="b")]=a
  }
  n=nrow(rkd_data)
  for(i in 1:n){
    if(rkd_data$Immunosuppressive.medication[i]=="Daily oral cyclophosphamide - UATC/LC01AA01"){
      rkd_data$immunosup_med_1[i]="Yes"
    }else{
      rkd_data$immunosup_med_1[i]="No"
    }
    if(rkd_data$Immunosuppressive.medication[i]=="Mycophenolate mofetil - UATC/L04AA06"){
      rkd_data$immunosup_med_2[i]="Yes"
    }else{
      rkd_data$immunosup_med_2[i]="No"
    }
    if(rkd_data$Immunosuppressive.medication[i]=="Azathioprine - UATC/L04AX01"){
      rkd_data$immunosup_med_3[i]="Yes"
    }else{
      rkd_data$immunosup_med_3[i]="No"
    }
    if(rkd_data$Immunosuppressive.medication[i]=="Methotrexate - UATC/L01BA00"){
      rkd_data$immunosup_med_4[i]="Yes"
    }else{
      rkd_data$immunosup_med_4[i]="No"
    }
    if(rkd_data$Immunosuppressive.medication[i]=="Leflunomide - UATC/L04AA13"){
      rkd_data$immunosup_med_5[i]="Yes"
    }else{
      rkd_data$immunosup_med_5[i]="No"
    }
    if(rkd_data$Immunosuppressive.medication[i]=="Other (ATC ontology)"){
      rkd_data$immunosup_med_6[i]="Yes"
    }else{
      rkd_data$immunosup_med_6[i]="No"
    }
    if(rkd_data$Immunosuppressive.medication[i]=="No"){
      rkd_data$immunosup_med_7[i]="Yes"
    }else{
      rkd_data$immunosup_med_7[i]="No"
    }
    if(rkd_data$Immunosuppressive.medication[i]=="IV Cyclophosphamide - UATC/L01AA01"){
      rkd_data$immunosup_med_8[i]="Yes"
    }else{
      rkd_data$immunosup_med_8[i]="No"
    }
    if(rkd_data$Immunosuppressive.medication[i]=="Mabthera: Rituximab - UATC/L01XCO2"){
      rkd_data$immunosup_med_9[i]="Yes"
    }else{
      rkd_data$immunosup_med_9[i]="No"
    }
    if(rkd_data$Immunosuppressive.medication[i]=="Ustekinumab - UATC/L04AC05"){
      rkd_data$immunosup_med_10[i]="Yes"
    }else{
      rkd_data$immunosup_med_10[i]="No"
    }
    if(rkd_data$Immunosuppressive.medication[i]=="Tacrolimus (including Advagraf, Prograf, etc.) - UATC/L04AD02"){
      rkd_data$immunosup_med_11[i]="Yes"
    }else{
      rkd_data$immunosup_med_11[i]="No"
    }
    if(rkd_data$Immunosuppressive.medication[i]=="Mepolizumab - UATC/R03DX09"){
      rkd_data$immunosup_med_12[i]="Yes"
    }else{
      rkd_data$immunosup_med_12[i]="No"
    }
    if(rkd_data$Immunosuppressive.medication[i]=="Methotrexate - UATC/L01BA01"){
      rkd_data$immunosup_med_13[i]="Yes"
    }else{
      rkd_data$immunosup_med_13[i]="No"
    }
    if(rkd_data$Immunosuppressive.medication[i]=="Truxima: Rituximab - UATC/L01XCO2"){
      rkd_data$immunosup_med_14[i]="Yes"
    }else{
      rkd_data$immunosup_med_14[i]="No"
    }
    
  }
  
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_Encounter_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}