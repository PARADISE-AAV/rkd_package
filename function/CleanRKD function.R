#' @title CleanRKD
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 24-Jan-23
#' Objective: The objective is to clean the RKD data and send the problematic data to the RKD person
#'
#'
#' @param RKDdata RKD data from loadRKD function
#' @param ouput_path folder where the Redcap data will be saved
#' @return The Redcap data cleaned in your folder and in an object
#' @details The function change the Date variable with the format "%Y-%m-%d". 
#' @details The function reduce the ethnicity to 6 group where different subgroup are regrouped.
#' @details The function clean some RKD.ID trouble to be sure that we have no problem when we do any merge with other dataset 
#' @details The function select the variable present only for the Encounters data and the Initial data (demographics, diagnostics and all other exams performed at the moment of the diagnostics)and merge the Encounter dtaa and Initail data that the initial data are replicate for each Encounter
#' @details The function create the following variable Age of the Encounter and ANCA titration (Anti MPO or Anti PR3 or NA) 
#' @export


CleanRKD=function(RKDdata, output_path){
  library(lubridate)
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  #extract the folder and the files 
  
  
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  a <- grep("Date",colnames(RKD_data))
  a <- a[-grep("known.unknown",colnames(RKD_data)[a])]
  for(i in a){
    RKD_data[,i] <- as.Date(RKD_data[,i])
  }
  
  
  
  ###Reduce ethnicity
  
  ethnicity <- NULL
  n <- nrow(RKD_data)
  for (i in 1:n){
    if((RKD_data$Ethnicity[i]) == ""){
      ethnicity <- c(ethnicity, "")
    }else{
      if(length(grep("W2",RKD_data$Ethnicity[i])) == 1  | length(grep("W1",RKD_data$Ethnicity[i])) == 1 | length(grep("W9",RKD_data$Ethnicity[i])) == 1){
        ethnicity <- c(ethnicity, "White")
      }
      if(length(grep("A2",RKD_data$Ethnicity[i])) == 1 | length(grep("A1",RKD_data$Ethnicity[i])) == 1 | length(grep("A9",RKD_data$Ethnicity[i])) == 1 | length(grep("A3",RKD_data$Ethnicity[i])) == 1){
        ethnicity <- c(ethnicity, "Asian")
      }
      if(length(grep("B2",RKD_data$Ethnicity[i])) == 1 | length(grep("B1",RKD_data$Ethnicity[i])) == 1 | length(grep("B9",RKD_data$Ethnicity[i])) == 1){
        ethnicity <- c(ethnicity, "Black")
      }
      if(length(grep("M2",RKD_data$Ethnicity[i])) == 1 | length(grep("M1",RKD_data$Ethnicity[i])) == 1 | length(grep("M9",RKD_data$Ethnicity[i])) == 1 | length(grep("M3",RKD_data$Ethnicity[i])) == 1){
        ethnicity <- c(ethnicity, "Mixed ethnicity")
      }
      if(length(grep("O1",RKD_data$Ethnicity[i])) == 1 | length(grep("O9",RKD_data$Ethnicity[i])) == 1){
        ethnicity <- c(ethnicity, "Other")
      }
      if(length(grep("NS",RKD_data$Ethnicity[i])) == 1){
        ethnicity <- c(ethnicity, "Not Stated")
      }
    }
  }
  RKD_data$Ethnicity <- ethnicity
  
  
  ethnicity <- NULL
  n <- nrow(RKD_data)
  for (i in 1:n){
    if((RKD_data$Ethnicity.of.mother[i]) == ""){
      ethnicity <- c(ethnicity, "")
    }else{
      if(length(grep("W2",RKD_data$Ethnicity.of.mother[i])) == 1  | length(grep("W1",RKD_data$Ethnicity.of.mother[i])) == 1 | length(grep("W9",RKD_data$Ethnicity.of.mother[i])) == 1){
        ethnicity <- c(ethnicity, "White")
      }
      if(length(grep("A2",RKD_data$Ethnicity.of.mother[i])) == 1 | length(grep("A1",RKD_data$Ethnicity.of.mother[i])) == 1 | length(grep("A9",RKD_data$Ethnicity.of.mother[i])) == 1 | length(grep("A3",RKD_data$Ethnicity.of.mother[i])) == 1){
        ethnicity <- c(ethnicity, "Asian")
      }
      if(length(grep("B2",RKD_data$Ethnicity.of.mother[i])) == 1 | length(grep("B1",RKD_data$Ethnicity.of.mother[i])) == 1 | length(grep("B9",RKD_data$Ethnicity.of.mother[i])) == 1){
        ethnicity <- c(ethnicity, "Black")
      }
      if(length(grep("M2",RKD_data$Ethnicity.of.mother[i])) == 1 | length(grep("M1",RKD_data$Ethnicity.of.mother[i])) == 1 | length(grep("M9",RKD_data$Ethnicity.of.mother[i])) == 1 | length(grep("M3",RKD_data$Ethnicity.of.mother[i])) == 1){
        ethnicity <- c(ethnicity, "Mixed ethnicity")
      }
      if(length(grep("O1",RKD_data$Ethnicity.of.mother[i])) == 1 | length(grep("O9",RKD_data$Ethnicity.of.mother[i])) == 1){
        ethnicity <- c(ethnicity, "Other")
      }
      if(length(grep("NS",RKD_data$Ethnicity.of.mother[i])) == 1){
        ethnicity <- c(ethnicity, "Not Stated")
        }
      }
    }
  RKD_data$Ethnicity.of.mother <- ethnicity
  
  ethnicity <- NULL
  n <- nrow(RKD_data)
  for (i in 1:n){
    if((RKD_data$Ethnicity.of.father[i]) == ""){
      ethnicity <- c(ethnicity, "")
    }else{
      if(length(grep("W2",RKD_data$Ethnicity.of.father[i])) == 1  | length(grep("W1",RKD_data$Ethnicity.of.father[i])) == 1 | length(grep("W9",RKD_data$Ethnicity.of.father[i])) == 1){
        ethnicity <- c(ethnicity, "White")
      }
      if(length(grep("A2",RKD_data$Ethnicity.of.father[i])) == 1 | length(grep("A1",RKD_data$Ethnicity.of.father[i])) == 1 | length(grep("A9",RKD_data$Ethnicity.of.father[i])) == 1 | length(grep("A3",RKD_data$Ethnicity.of.father[i])) == 1){
        ethnicity <- c(ethnicity, "Asian")
      }
      if(length(grep("B2",RKD_data$Ethnicity.of.father[i])) == 1 | length(grep("B1",RKD_data$Ethnicity.of.father[i])) == 1 | length(grep("B9",RKD_data$Ethnicity.of.father[i])) == 1){
        ethnicity <- c(ethnicity, "Black")
      }
      if(length(grep("M2",RKD_data$Ethnicity.of.father[i])) == 1 | length(grep("M1",RKD_data$Ethnicity.of.father[i])) == 1 | length(grep("M9",RKD_data$Ethnicity.of.father[i])) == 1 | length(grep("M3",RKD_data$Ethnicity.of.father[i])) == 1){
        ethnicity <- c(ethnicity, "Mixed ethnicity")
      }
      if(length(grep("O1",RKD_data$Ethnicity.of.father[i])) == 1 | length(grep("O9",RKD_data$Ethnicity.of.father[i])) == 1){
        ethnicity <- c(ethnicity, "Other")
      }
      if(length(grep("NS",RKD_data$Ethnicity.of.father[i])) == 1){
        ethnicity <- c(ethnicity, "Not Stated")
      }
    }
  }
  RKD_data$Ethnicity.of.father <- ethnicity
  
  ###solve the problem of the RKD.ID wrong
  
  RKD_data$RKD.ID <- as.factor(RKD_data$RKD.ID)
  if(length(grep("-",RKD_data$RKD.ID))>0){
    RKD_data_RKDID_PB <- RKD_data[grep("-",RKD_data$RKD.ID),]
    RKD_data_RKDID_PB$RKD.ID <- droplevels(RKD_data_RKDID_PB$RKD.ID)
    n <- length(levels(RKD_data_RKDID_PB$RKD.ID))
    RKD_data_RKDID_PB_2 <- NULL
    for ( i in 1:n){
      dat <- RKD_data_RKDID_PB[which(RKD_data_RKDID_PB$RKD.ID==levels(RKD_data_RKDID_PB$RKD.ID)[i]),]
      dat$RKD.ID <- rep(dat$Patient.Id[1],nrow(dat))
      RKD_data_RKDID_PB_2 <- rbind(RKD_data_RKDID_PB_2,dat)
    }
    
    RKD_data_RKDID_NOPB <- RKD_data[-grep("-",RKD_data$RKD.ID),]
    
    RKD_data <- rbind(RKD_data_RKDID_NOPB,RKD_data_RKDID_PB_2)
  }
 
 
  RKD_Encounter <- RKD_data[which(RKD_data$Repeat.Instrument == "Encounters"),]
  RKD_Initial <- RKD_data[which(RKD_data$Repeat.Instrument == "" & RKD_data$Type.of.Patient!= ""),]
  
  
  ####select the variable of the Encounters
  
  a_Encounters <- NULL
  b_Encounters <- NULL
  for (i in 5:ncol(RKD_Encounter)) {
    # for-loop over columns
    na_values <- length(which(is.na(RKD_Encounter[,i]) == TRUE))
    
    if (na_values == nrow(RKD_Encounter )) {
      a_Encounters <- c(a_Encounters, colnames(RKD_Encounter)[i])
    }
    else {
      if(length(levels(as.factor(RKD_Encounter[,i])))== 1 & levels(as.factor(RKD_Encounter[,i]))[1] == ""){
        a_Encounters <- c(a_Encounters, colnames(RKD_Encounter)[i])
      }else{
        b_Encounters <- c(b_Encounters, colnames(RKD_Encounter)[i])
      }
      
    }
  }
  
  
  newdata <- RKD_Encounter[,1:4]
  c=colnames(RKD_Encounter)
  for( j in 1:length(b_Encounters)) {
    newdata <- cbind(newdata, RKD_Encounter[,which(c == b_Encounters[j])])
  }
  colnames(newdata)[-c(1:4)] <- b_Encounters
  RKD_Encounter_filter <- newdata
  
  a_Initials <- NULL
  b_Initials <- NULL
  for (i in 5:ncol(RKD_Initial)) {
    # for-loop over columns
    na_values <- length(which(is.na(RKD_Initial[,i]) == TRUE))
    
    if (na_values == nrow(RKD_Initial )) {
      a_Initials <- c(a_Initials, colnames(RKD_Initial)[i])
    }
    else {
      if(length(levels(as.factor(RKD_Initial[,i]))) == 1 & levels(as.factor(RKD_Initial[,i]))[1] == ""){
        a_Initials <- c(a_Initials, colnames(RKD_Initial)[i])
      }else{
        b_Initials <- c(b_Initials, colnames(RKD_Initial)[i])
      }
      
    }
  }

  
  newdata <- RKD_Initial[,1:4]
  c=colnames(RKD_Initial)
  for( j in 1:length(b_Initials)) {
    newdata <- cbind(newdata, RKD_Initial[,which(c == b_Initials[j])])
  }
  colnames(newdata)[-c(1:4)] <- b_Initials
  RKD_Initial_filter <- newdata
  
  RKD_data_filter <- merge(RKD_Encounter_filter, RKD_Initial_filter[,-c(2:4)], by = "RKD.ID")
  
  RKD_data_filter$Age_Encounters <- year(RKD_data_filter$Date.Of.Visit)- year(RKD_data_filter$Date.of.Birth)
  
  RKD_data_filter$AntiMPO_PR3 <- NA
  m <- nrow(RKD_data_filter)
  for(i in 1:m){
    if (is.na(RKD_data_filter$Anti.MPO.level[i]) == TRUE & is.na(RKD_data_filter$Anti.PR3.level[i]) == TRUE){
      RKD_data_filter$AntiMPO_PR3[i] <- NA
    }else{
      if(is.na(RKD_data_filter$Anti.MPO.level[i]) == TRUE & is.na(RKD_data_filter$Anti.PR3.level[i]) == FALSE){
        RKD_data_filter$AntiMPO_PR3[i] <- "PR3"
      }else{
        if(is.na(RKD_data_filter$Anti.MPO.level[i]) == FALSE & is.na(RKD_data_filter$Anti.PR3.level[i]) == TRUE){
          RKD_data_filter$AntiMPO_PR3[i] <- "MPO"
        }else{
          if(RKD_data_filter$Anti.MPO.level[i] > RKD_data_filter$Anti.PR3.level[i]){
            RKD_data_filter$AntiMPO_PR3[i] <- "MPO"
          }else{
            RKD_data_filter$AntiMPO_PR3[i] <- "PR3"
          }
        }
      }
    }
  }
  
  Clean_RKD_data <- RKD_data_filter
  
  files_test <-  list.dirs(output_path)
  if(identical(files_test, character(0)) == TRUE){
    stop("Your output folder don't exist")
  }
  
  write.csv(Clean_RKD_data, paste(output_path, "/Redcap_clinical_data_clean", Sys.Date() , ".csv", sep=""), row.names = F)
  return(Clean_RKD_data)
}