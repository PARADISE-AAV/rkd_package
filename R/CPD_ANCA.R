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
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' 
#' @import lubridate
#' @export

CPD_ANCA <- function(merge_data, output_dir){
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  rkd=merge_data[,c("RKD.ID", "Date.Of.Visit", "CPD_relapse","At.any.point.ANCA.specificity", "Anti.PR3.level", "Anti.MPO.level", "ANCA.IF")]
  
  rkd$ANCA_Levels="Unknown"
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
  }
  rkd$ANCA_Statuts="ANCA Status Unknown"
  n=nrow(rkd)
  for(i in 1:n){
    if(( is.na(rkd$ANCA_Levels[i])== FALSE & rkd$ANCA_Levels[i] > 2) | rkd$ANCA.IF[i] == "Atypical" | rkd$ANCA.IF[i]  == "P" | rkd$ANCA.IF[i]  == "C"){
      rkd$ANCA_Statuts[i] = "ANCA Positive"
    }
    if(( is.na(rkd$ANCA_Levels[i])== FALSE & rkd$ANCA_Levels[i] <= 2) | rkd$ANCA.IF[i] == "Negative" ){
      rkd$ANCA_Statuts[i] = "ANCA Negative"
    }
    
  }
  rkd$ANCA_Switch = NA
  n=nrow(rkd)
  for( i in 2:n){
    if(rkd$RKD.ID[i] == rkd$RKD.ID[i-1] & interval(rkd$Date.Of.Visit[i-1], rkd$Date.Of.Visit[i]) %/% months(1)<=18 &  rkd$CPD_relapse[i] == "No Relapse" & rkd$CPD_relapse[i-1] == "No Relapse"){
      if(rkd$ANCA_Statuts[i-1] == "ANCA Negative" & rkd$ANCA_Statuts[i] == "ANCA Negative"){
        rkd$ANCA_Switch[i] = "Neg-Neg Switch"
      }
      if(rkd$ANCA_Statuts[i-1] == "ANCA Negative" & rkd$ANCA_Statuts[i] == "ANCA Positive"){
        rkd$ANCA_Switch[i] = "Neg-Pos Switch"
      }
      if(rkd$ANCA_Statuts[i-1] == "ANCA Positive" & rkd$ANCA_Statuts[i] == "ANCA Positive"){
        rkd$ANCA_Switch[i] = "Pos-Pos Switch"
      }
      if(rkd$ANCA_Statuts[i-1] == "ANCA Positive" & rkd$ANCA_Statuts[i] == "ANCA Negative"){
        rkd$ANCA_Switch[i] = "Pos-Neg Switch"
      }
      if(rkd$ANCA_Statuts[i-1] == "ANCA Status Unknown" & rkd$ANCA_Statuts[i] == "ANCA Status Unknown"){
        rkd$ANCA_Switch[i] = "Switch statuts Unknown"
      }
    }
  }
  
  rkd_data=merge(merge_data, rkd, by=c("RKD.ID", "Date.Of.Visit"))
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_Encounter_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
}