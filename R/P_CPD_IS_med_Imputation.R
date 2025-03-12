#' @title CPD IS Imputation
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD IS Imputation for Treatment On/Off
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#' @param Encounter {"name": "rkd_data","desc": "RIV data from \code{\link{CPD_Encounter}} function","options": (),"type": "file"}
#' @param output_dir  {"name": "output_dir","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#'
#' @details
#' to be added
#' 
#' @import lubridate
#' @export

CPD_IS_medication_Imputation <- function(Encounter, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(Encounter))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  n=length(levels(as.factor(Encounter$RKD.ID)))
  Encounter2=NULL
  for(i in 1:n){
    dat=Encounter[which(Encounter$RKD.ID==levels(as.factor(Encounter$RKD.ID))[i]),]
    m=nrow(dat)
    if(m>2){
      for(j in 2:m){
        if(dat$Immunosuppressive.medication[j]==""){
          k=j
          while(dat$Immunosuppressive.medication[k]=="" & k<m){
            k=k+1
          }
          if(as.numeric(dat$Date.Of.Visit[k]-dat$Date.Of.Visit[j-1])<=730 & dat$Immunosuppressive.medication[k]==dat$Immunosuppressive.medication[j-1]){
            dat$Immunosuppressive.medication[j:k-1]=dat$Immunosuppressive.medication[j-1]
          }
        }
      }
    }
    Encounter2=rbind(Encounter2,dat)
  } 
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_IS_medication_imputation_function_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(Encounter2, output_filename, row.names = FALSE)
  return(Encounter2)
  
}