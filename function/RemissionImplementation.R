#' @title RemissionImplementation
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 06-Jun-23
#' Objective: The objective is to correct the remission label in the variable Disease.activity.since.last.return
#'
#'
#' @param RKDdata RKD data
#' @return The Redcap data witht he remission labels corected
#' @export
#' 

RemissionImplementation=function(RKDdata){
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  RKD_data_nonmissing <- RKD_data[which(RKD_data$Disease.activity.since.last.return != ""), ]
  RKD_data_missing <- RKD_data[which(RKD_data$Disease.activity.since.last.return == ""), ]
  
  n <- nrow(RKD_data_missing)
  
  for (i in 1:n){
    if(is.na(RKD_data_missing$BVAS.score..calculator.[i]) == FALSE & RKD_data_missing$BVAS.score..calculator.[i] == 0){
      RKD_data_missing$Disease.activity.since.last.return[i] <- "Remission"
    }else{
      if(is.na(RKD_data_missing$BVAS.score..calculator.[i]) == TRUE){
        if(RKD_data_missing$Adjudicated.probability.of.relapse[i] == "No"){
          RKD_data_missing$Disease.activity.since.last.return[i] <- "Remission"
        }else{
          if(RKD_data_missing$Adjudicated.probability.of.relapse[i] == ""){
            if(RKD_data_missing$Immunosuppressive.medication.in.response.to.this.encounter[i] == "Immunosuppression reduced"){
              RKD_data_missing$Disease.activity.since.last.return[i] <- "Remission"
            }else{
              if(RKD_data_missing$Immunosuppressive.medication.in.response.to.this.encounter[i] == ""){
                if(RKD_data_missing$Suggestive.bloods.OR.urine.tests..excluding.ANCA.[i] == "Blood/urine tests are not suggestive of relapse"){
                  RKD_data_missing$Disease.activity.since.last.return[i] <- "Remission"
                }
              }
            }
          }
        }
      }
    }
  }
  RKD_data_correct <- rbind(RKD_data_nonmissing, RKD_data_missing)
  
  return(RKD_data_correct)
}