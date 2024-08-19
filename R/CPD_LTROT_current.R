#' @title CPD current LTROT
#' @author Matthieu COQ
#'
#' @description to be added
#' 
#' Version: 1.0
#' 
#' Date: 15-Aug-23
#'
#' @param merge_data Data from the merge of encounter and General characteristics in the \code{\link{CPD_Treatment}} function
#' @param output_dir folder where the Redcap data will be saved
#' @param interval Number of day to be off treatment
#' @details
#' to be added
#' @import lubridate
#' @import dplyr
#' @export

CPD_LTROT_current <- function(merge_data, interval, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  stopifnot("Your argument need to be a numeric"=is.numeric(interval))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  merge_data1=merge_data[which(is.na(merge_data$CPD_relapse)==FALSE),]
  merge_data1$LTROT_current=NA
  merge_data_LTROT=NULL
  n=length(levels(as.factor(merge_data1$RKD.ID)))
  for (i in 1:n){
    dat <- merge_data1[which(merge_data1$RKD.ID == levels(as.factor(merge_data1$RKD.ID))[i] ),]
    if(as.numeric(difftime(max(dat$Date.Of.Visit), min(dat$Date.Of.Visit)))>=interval){
      m=nrow(dat)
      for(j in 1:m){
        intermax=dat$Date.Of.Visit[j]+interval
        dat1 <- dat[which(dat$Date.Of.Visit>=dat$Date.Of.Visit[j] & dat$Date.Of.Visit<=intermax),]
        if(dim(table(dat1$CPD_relapse))==1 & dat1$CPD_relapse[1]=="No Relapse" & dim(table(dat1$CPD_treatment))==1 & dat1$CPD_treatment[1]=="Off Treatment"){
          dat1$LTROT_current[nrow(dat1)]="LTROT"
        }
        merge_data_LTROT=rbind(merge_data_LTROT,dat1[,c("RKD.ID", "Date.Of.Visit", "LTROT_current")])
      }
    }
    
  }
  merge_data_LTROT=merge_data_LTROT[!duplicated(merge_data_LTROT[,c(1:2)]),]
  
  merge_LTROT=merge(merge_data, merge_data_LTROT, by = c("RKD.ID", "Date.Of.Visit"), all.x = TRUE)
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_ltrot_current_function_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(merge_LTROT, output_filename, row.names = FALSE)
  return(merge_LTROT)
  
}