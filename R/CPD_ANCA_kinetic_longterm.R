#' @title CPD ANCA kinetic longterm
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD ANCA kinetic long term
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param merge_data Data from the \code{\link{CPD_ANCA}} function
#' @details
#' The CPD ANCA kinetics long term is what is the kinetic of ANCA in long term. For this, we look at what is the ANCA Status  during the 2 last encounters. 
#' * If the 3 encounter have a ANCA Status positive, the CPD is persistent positive 
#' * If the 3 encounter have a ANCA Status negative, the CPD is persistent negative 
#' * If the 3 encounter have a ANCA Status positive and negative, the CPD is Variable ANCA level 
#' * If at least one encounter have a ANCA Status unknown, the CPD is  Unknown ANCA level
#' 
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @export

CPD_ANCA_kinetics <- function(merge_data){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  
  merge_data$anca_kinetics_longterm <- "Unknown ANCA level"
  n=nrow(merge_data)
  for(i in 3:n){
    if(merge_data$RKD.ID[i]==merge_data$RKD.ID[i-1] & merge_data$RKD.ID[i]==merge_data$RKD.ID[i-2]){
      if(merge_data$ANCA_Status[i]=="ANCA Positive" & merge_data$ANCA_Status[i-1]=="ANCA Positive" & merge_data$ANCA_Status[i-2]=="ANCA Positive"){
        merge_data$anca_kinetics_longterm[i] <- "persistent positive"
      }
      if(merge_data$ANCA_Status[i]=="ANCA Negative" & merge_data$ANCA_Status[i-1]=="ANCA Negative" & merge_data$ANCA_Status[i-2]=="ANCA Negative"){
        merge_data$anca_kinetics_longterm[i] <- "persistent negative"
      }
      if(merge_data$ANCA_Status[i]!="ANCA Status Unknown" & merge_data$ANCA_Status[i-1]!="ANCA Status Unknown" & merge_data$ANCA_Status[i-2]!="ANCA Status Unknown" & merge_data$anca_kinetics_longterm[i] == "Unknown ANCA level"){
        merge_data$anca_kinetics_longterm[i] <- "Variable ANCA level"
      }
    }
  }
  
  
  return(merge_data)
  
}