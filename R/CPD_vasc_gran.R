#' @title CPD vascvs gran
#' @author Matthieu COQ
#'
#' @description to be added
#' 
#' Version: 1.0
#' 
#' Date: 15-Aug-23
#'
#' @param merge_data Data from the merge of encounter and General characteristics in the \code{\link{CPD_Treatment}} function
#' @details
#' to be added
#' 
#' @import lubridate
#' @import dplyr
#' @export

CPD_vasc_vs_gran <- function(merge_data){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  
  merge_data$CPD_vasc_gran="Unknown"
  n=nrow(merge_data)
  for(i in 1:n){
    
    if(merge_data$CPD_relapse[i]=="Definite Relapse"){
      
      if(merge_data$ENT...yes..choice.Bloody.nasal.discharge...crusts...ulcers...granulomata.[i] == "Checked" | 
         merge_data$ENT...yes..choice.Paranasal.sinus.involvement.[i] == "Checked" |
         merge_data$ENT...yes..choice.Subglottic.stenosis.[i] == "Checked" |
         merge_data$ENT...yes..choice.Conductive.hearing.loss.[i] == "Checked" |
         merge_data$ENT...yes..choice.Sensorineural.hearing.loss.[i] == "Checked" |
         merge_data$Chest...yes..choice.Endobronchial.involvement.[i] == "Checked" |
         merge_data$Chest...yes..choice.Nodules.or.cavities.[i] == "Checked" |
         merge_data$Nervous.system...yes..choice.Meningitis.[i] == "Checked" |
         merge_data$Mucous.membranes...eyes...yes..choice.Significant.proptosis.[i] == "Checked"
      ){
        merge_data$CPD_vasc_gran[i] = "Granulomatous"
      }
      
      if(
        
      ){
        merge_data$CPD_vasc_gran[i] = "Vasculitic"
      }
      
    }
    
  }
  
  return(merge_data)
}