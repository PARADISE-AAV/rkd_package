#' @title CPD_Relapse
#' @author Matthieu COQ/Jennifer Scott
#' @Version: 1.0
#' @Date: 07-Jul-23
#' @Objective: The objective is to apply CPD Relapse approach for relapse on RKD data
#'
#'
#' @param RKDdata Data frame with the RKD data in
#' @details
#' 
#' 
#' 
#' @return The RKD data with the relapse category
#' @export

JenRelapse <- function(RKDdata){
  
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
  prep2_data <- prep2(prep1_data)
  final_data <- cpd.relapse(prep2_data)
  relapse_final_data <- Relapse_final(final_data)
  
  CPD_Relapse_RKD_data <- merge(RKD_data, relapse_final_data, by.x = c("RKD.ID", "Date.Of.Visit"), by.y = c("ID", "Date.Of.Visit"))
  
  n=nrow(CPD_Relapse_RKD_data)
  CPD_Relapse_RKD_data$CPD_relapse=NULL
  for( i in 1:n){
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
      CPD_Relapse_RKD_data$CPD_relapse[i]="Exclude this encounter"
    }
    if(CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="Manual review" & CPD_Relapse_RKD_data$Nature.of.confirmed.relapse[i]== "None"){
      CPD_Relapse_RKD_data$CPD_relapse[i]="No Relapse"
    }
    if(CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="Manual review" & (CPD_Relapse_RKD_data$Nature.of.confirmed.relapse[i]!= "None" & CPD_Relapse_RKD_data$Nature.of.confirmed.relapse[i]!= "") & CPD_Relapse_RKD_data$Do.you.think.Vasculitis.is.relapsing.in.this.encounter[i]=="High Probability"){
      CPD_Relapse_RKD_data$CPD_relapse[i]="Definite Relapse"
    }
    if(CPD_Relapse_RKD_data$rel.JS.OVERALL[i]=="Manual review" & (CPD_Relapse_RKD_data$Nature.of.confirmed.relapse[i]!= "None" & CPD_Relapse_RKD_data$Nature.of.confirmed.relapse[i]!= "") & CPD_Relapse_RKD_data$Do.you.think.Vasculitis.is.relapsing.in.this.encounter[i]=="Possibly"){
      CPD_Relapse_RKD_data$CPD_relapse[i]="Possible Relapse"
    }
  }
  
  return(CPD_Relapse_RKD_data)
  
}