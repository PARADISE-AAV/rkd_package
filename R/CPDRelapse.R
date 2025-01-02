#' @title CPD Relapse
#' @author Matthieu COQ/Jennifer Scott
#' @description The objective is to apply CPD Relapse approach on RIV data
#'
#'
#' @param RKDdata Data frame with the RKD data
#' @param interval_from_diagnostics the interval from diagnostics for the algorithm Paradise_Encounter by default 6
#' @details The CPD relapse function have the objective to tell if an Encounter is in Relapse or not
#' The algorithm is described in the following picture: \href{https://3.basecamp.com/3790396/buckets/31062049/uploads/6740235559}{Workflow of the CPD Relapse}
#' 
#' There is 4 sub-function
#' * \code{\link{prep1}} and \code{\link{prep2}} are function to prepare the data for the modelling and the classification
#' * \code{\link{cpd_relapse}} is the function to apply the model described in Jennifer Scott article
#' * \code{\link{Relapse_final}} is the function where the last rules of the step1 and step2 are done
#' * The step3 of the function is done in this function.
#' 
#' @return The RIV data with the relapse category
#' @export

CPDRelapse <- function(RKDdata, interval_from_diagnostics = 6){
  
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  prep1_data <- prep1(RKD_data)
  prep2_data <- prep2(prep1_data, interval_from_diagnostics)
  final_data <- cpd_relapse(prep2_data, interval_from_diagnostics)
  relapse_final_data <- Relapse_final(final_data)
  
  CPD_Relapse_RKD_data <- merge(RKD_data, relapse_final_data, by.x = c("RKD.ID", "Date.Of.Visit"), by.y = c("ID", "Date.Of.Visit"),all.x=TRUE)
  
  n=nrow(CPD_Relapse_RKD_data)
  CPD_Relapse_RKD_data$CPD_relapse=NA
  for( i in 1:n){
    if(is.na(CPD_Relapse_RKD_data$rel.JS.OVERALL[i])==FALSE){
      if(CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="High Probability" | CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="Definite" | CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="1"){
        CPD_Relapse_RKD_data$CPD_relapse[i]="Definite Relapse"
      }
      if(CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="No" | CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="0"){
        CPD_Relapse_RKD_data$CPD_relapse[i]="No Relapse"
      }
      if(CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="Possible"){
        CPD_Relapse_RKD_data$CPD_relapse[i]="Possible Relapse"
      }
      if(CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="Relapse within 90d, so exclude"){
        CPD_Relapse_RKD_data$CPD_relapse[i]="Recent relapse"
      }
      if(CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="Manual review" & CPD_Relapse_RKD_data$Nature.of.confirmed.relapse[i]== "None"){
        CPD_Relapse_RKD_data$CPD_relapse[i]="No Relapse"
      }
      if(CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="Manual review" & (CPD_Relapse_RKD_data$Nature.of.confirmed.relapse[i]!= "None" & CPD_Relapse_RKD_data$Nature.of.confirmed.relapse[i]!= "") & CPD_Relapse_RKD_data$Disease.activity.since.last.return[i]=="Active"){
        CPD_Relapse_RKD_data$CPD_relapse[i]="Definite Relapse"
      }
      if(CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="Manual review" & (CPD_Relapse_RKD_data$Nature.of.confirmed.relapse[i]!= "None" & CPD_Relapse_RKD_data$Nature.of.confirmed.relapse[i]!= "") & CPD_Relapse_RKD_data$Disease.activity.since.last.return[i]=="Low disease activity"){
        CPD_Relapse_RKD_data$CPD_relapse[i]="Possible Relapse"
      }
      if(CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="Manual review" & (CPD_Relapse_RKD_data$Nature.of.confirmed.relapse[i]!= "None" & CPD_Relapse_RKD_data$Nature.of.confirmed.relapse[i]!= "") & CPD_Relapse_RKD_data$Disease.activity.since.last.return[i]=="Remission"){
        CPD_Relapse_RKD_data$CPD_relapse[i]="Manual review"
      }
      if(CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="Manual review" & CPD_Relapse_RKD_data$Nature.of.confirmed.relapse[i]== ""){
        CPD_Relapse_RKD_data$CPD_relapse[i]="Manual review"
      }
      
    }
    if(is.na(CPD_Relapse_RKD_data$interval_from_diagnosis[i])==FALSE & CPD_Relapse_RKD_data$interval_from_diagnosis[i] < 180 ){
      CPD_Relapse_RKD_data$CPD_relapse[i]="Under 6 months"
    }
  }
  
  return(CPD_Relapse_RKD_data)
  
}