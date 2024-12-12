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
  
  
  n=length(levels(as.factor(Encounter$RKD.ID)))
  Encounter2=NULL
  for(i in 1:n){
    dat=Encounter[which(Encounter$RKD.ID==levels(as.factor(Encounter$RKD.ID))[i]),]
    m=nrow(dat)
    if(m>2){
      for(j in 2:m){
        if(dat$ANCA.IF[j]=="" | dat$ANCA.IF[j]=="Not tested"){
          k=j
          while((dat$ANCA.IF[j]=="" | dat$ANCA.IF[j]=="Not tested") & k<m){
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
  
  
  for(i in 1:n){
    if(Encounter2$ANCA.IF[i]=="Negative" & is.na(Encounter2$Anti.MPO.level[i])==TRUE){
      Encounter2$Anti.MPO.level[i]=1
    }
    if(Encounter2$ANCA.IF[i]=="Negative" & is.na(Encounter2$Anti.PR3.level[i])==TRUE){
      Encounter2$Anti.PR3.level[i]=1
    }
  }
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_ANCA_imputation_function_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(Encounter2, output_filename, row.names = FALSE)
  return(Encounter2)
  
}