#' @title Treatment_On_Off
#' @author Matthieu COQ/Angel george
#' @Version: 1.0
#' @Date: 11-Jul-23
#' @Objective: The objective is to tell if a encounter is on or off treatment
#'
#'
#' @param RKDdata Data frame with the RKD data in
#' @return The RKD data with the infrmation if a encounter is on or off treatment
#' @export

Treatment_On_Off <- function(RKDdata){
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  RKD_data$treatment.on.off <- NULL
  
  RKD_data$Immunosuppressive.status[is.na(RKD_data$Immunosuppressive.status)==T] <- ""
  RKD_data$Immunosuppressive.medication[is.na(RKD_data$Immunosuppressive.medication)==T] <- ""
  RKD_data$Current.corticosteroid.dose[is.na(RKD_data$Current.corticosteroid.dose)==T] <- ""
  RKD_data$Corticosteroids[is.na(RKD_data$Corticosteroids)==T] <- ""
  
  n <- nrow(RKD_data)
  for (i in 1:n){
    if(RKD_data$Immunosuppressive.status[i] == "Currently on immunosuppression" | RKD_data$Immunosuppressive.status[i] == "Discontinuation of immunosuppression within 6 months prior to this encounter"){
      RKD_data$treatment.on.off[i] <- "On Treatment"
    }else{
      if(RKD_data$Immunosuppressive.status[i] == "Treatment Naïve" | RKD_data$Immunosuppressive.status[i] == "Discontinuation of immunosuppression > 6 months prior to this encounter"){
        RKD_data$treatment.on.off[i] <- "Not On Treatment"
      }else{
        if(RKD_data$Immunosuppressive.medication[i] != "No" & RKD_data$Immunosuppressive.medication[i] != ""){
          RKD_data$treatment.on.off[i] <- "On Treatment"
        }else{
          if((RKD_data$Immunosuppressive.medication[i] == "No" | RKD_data$Immunosuppressive.medication[i] == "") & RKD_data$Corticosteroids[i] == "Yes" &
             (RKD_data$Current.corticosteroid.dose[i] == "11 - 20 mg/day" | RKD_data$Current.corticosteroid.dose[i] == "> 20 mg/day")){
            RKD_data$treatment.on.off[i] <- "On Treatment"
          }else{
            if(RKD_data$Immunosuppressive.medication[i] == "No" & RKD_data$Immunosuppressive.medication[i] == "No"){
              RKD_data$treatment.on.off[i] <- "Not On Treatment"
            }else{
              RKD_data$treatment.on.off[i] <- ""
            }
            
          }
        }
      }
    }
  }
  return(RKD_data)
}