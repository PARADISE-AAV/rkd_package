#' @title CPD ANCA Imputation
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD ANCA Imputation
#' 
#' Version: 1.0
#' 
#' Date: 24-September-23
#'
#' @param Encounter  {"name": "Encounter","desc": "RIV data from \code{\link{SplitRIV}} function","options": (),"type": "file"}
#' @param output_dir  {"name": "output_dir","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#' @details
#' 
#' This function is done to compile the CPD_ANCA_imputation based on the [CPD_ANCA_imputation](https://3.basecamp.com/3790396/buckets/31062049/google_documents/8131573548) document. 
#' 
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
  
  
  n=length(levels(as.factor(Encounter$RKD.ID)))
  Encounter2=NULL
  for(i in 1:n){
    dat=Encounter[which(Encounter$RKD.ID==levels(as.factor(Encounter$RKD.ID))[i]),]
    m=nrow(dat)
    if(m>2){
      for(j in 2:m){
        if(dat$ANCA.IF[j]=="" | dat$ANCA.IF[j]=="Not tested"){
          k=j
          while((dat$ANCA.IF[k]=="" | dat$ANCA.IF[k]=="Not tested") & k<m){
            k=k+1
          }
          if(as.numeric(dat$Date.Of.Visit[k]-dat$Date.Of.Visit[j-1])<=400 & dat$ANCA.IF[k]==dat$ANCA.IF[j-1]){
            dat$ANCA.IF[j:k-1]=dat$ANCA.IF[j-1]
          }
        }
      }
    }
    Encounter2=rbind(Encounter2,dat)
  } 
  
  
  Encounter2$Anti.MPO.level[which(Encounter2$ANCA.IF=="Negative" & is.na(Encounter2$Anti.MPO.level)==TRUE)]=1
  Encounter2$Anti.MPO.level[which(Encounter2$ANCA.IF=="Negative" & is.na(Encounter2$Anti.PR3.level)==TRUE)]=1
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_ANCA_imputation_function_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(Encounter2, output_filename, row.names = FALSE)
  return(Encounter2)
  
}