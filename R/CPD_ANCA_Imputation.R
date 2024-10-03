#' @title CPD ANCA Imputation
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD ANCA Imputation
#' 
#' Version: 1.0
#' 
#' Date: 24-September-23
#'
#' @param Encounter Data from Encounter from \code{\link{SplitRIV}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' 
#' @import lubridate
#' @export

CPD_ANCA_Imputation <- function(Encounter, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(Encounter))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  n=nrow(Encounter)
  for(i in 2:n){
    if((Encounter$ANCA.IF[i]=="" | Encounter$ANCA.IF[i]=="Not tested") & Encounter$ANCA.IF[i-1]==Encounter$ANCA.IF[i+1] & Encounter$RKD.ID[i-1]==Encounter$RKD.ID[i+1] & as.numeric(Encounter$Date.Of.Visit[i+1]-Encounter$Date.Of.Visit[i-1])<=400){
      Encounter$ANCA.IF[i]=Encounter$ANCA.IF[i-1]
    }
  }
  
  for(i in 1:n){
    if(Encounter$ANCA.IF[i]=="Negative" & is.na(Encounter$Anti.MPO.level[i])==TRUE){
      Encounter$Anti.MPO.level[i]=1
    }
    if(Encounter$ANCA.IF[i]=="Negative" & is.na(Encounter$Anti.PR3.level[i])==TRUE){
      Encounter$Anti.PR3.level[i]=1
    }
  }
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_ANCA_imputation_function_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(Encounter, output_filename, row.names = FALSE)
  return(Encounter)
  
}