#' @title Clean rare kidney disease data
#' @author Matthieu COQ
#'
#' The objective is to clean the RKD data and send the problematic data to the RKD person
#' Version: 1.0
#' Date: 24-Jan-23
#'
#' @param rkd_data RKD data from \code{\link{load_rkd}} function
#' @param ouput_path folder where the Redcap data will be saved
#' @return The Redcap data cleaned in your folder and in an object
#' The function change the Date variable with the format "%Y-%m-%d". 
#' The function reduce the ethnicity to 6 group where different subgroup are regrouped.
#' The function clean some RKD.ID trouble to be sure that we have no problem when we do any merge with other dataset 
#' The function select the variable present only for the Encounters data and the Initial data (demographics, diagnostics and all other exams performed at the moment of the diagnostics)and merge the Encounter dtaa and Initail data that the initial data are replicate for each Encounter
#' The function create the following variable Age of the Encounter and ANCA titration (Anti MPO or Anti PR3 or NA) 
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @export


clean_rkd <- function(rkd_data, output_path) {
  # Check arguments
  stopifnot(is.data.frame(rkd_data))
  stopifnot(is.character(output_path))

  # Check that you load a real file
  if (!ncol(rkd_data) || !nrow(rkd_data)) {
    stop("You supplied an empty file")
  }

  # Parse all date columns
  date_columns <- stringr::str_subset(colnames(rkd_data), 'Date')
  date_columns <- stringr::str_subset(date_columns, 'known.unknown', negate = TRUE)
  rkd_data <- dplyr::mutate(rkd_data,
    dplyr::across(dplyr::all_of(date_columns), lubridate::as_date))

  # Collapse ethnicities
  rkd_data <- dplyr::mutate(rkd_data,
    dplyr::across(
      dplyr::starts_with('Ethnicity'),
      ~ forcats::fct_collapse(
        stringr::str_extract(.x, '^[A-Z]{0,2}'),
        White = 'W',
        Asian = 'A',
        Black = 'B',
        `Mixed ethnicity` = 'M',
        Other = 'O',
        `Not Stated` = 'NS'
      ))
  
  # Solve the problem of incorrect RKD IDs
  # If the RKD ID contains a '-', replace it with Patient Id
  rkd_data$RKD.ID[stringr::str_detect(rkd_data$RKD.ID, '-')] <- NA
  if (any(is.na(rkd_data$RKD.ID))) {
    warning('Replacing', paste(sum(is.na(rkd_data$RKD.ID)), 'incorrect RKD.IDs with Patient.Id'))
  }
  rkd_data <- dplyr::mutate(rkd_data, RKD.ID = dplyr::coalesce(RKD.ID, Patient.Id))
 
  RKD_Encounter <- rkd_data[rkd_data$Repeat.Instrument == "Encounters", ]
  RKD_Initial <- rkd_data[rkd_data$Repeat.Instrument == "" & rkd_data$Type.of.Patient != "",]
  
  
  ####select the variable of the Encounters
  
  a_Encounters <- NULL
  b_Encounters <- NULL
  for (i in 5:ncol(RKD_Encounter)) {
    # for-loop over columns
    na_values <- length(which(is.na(RKD_Encounter[,i]) == TRUE)) # sum(is.na(.))
    
    if (na_values == nrow(RKD_Encounter )) { # if (all(is.na(.)))
      a_Encounters <- c(a_Encounters, colnames(RKD_Encounter)[i]) # then this is an encounter column
    }
    else {
      if(length(levels(as.factor(RKD_Encounter[,i])))== 1 & levels(as.factor(RKD_Encounter[,i]))[1] == ""){ # all(!nchar(.))
        a_Encounters <- c(a_Encounters, colnames(RKD_Encounter)[i]) # then this is an encounter column
      }else{
        b_Encounters <- c(b_Encounters, colnames(RKD_Encounter)[i]) # otherwise it is not?
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
  setwd(output_path)
  write.csv(Clean_RKD_data, paste("Redcap_clinical_data_clean", Sys.Date() , ".csv", sep=""), row.names = F)
  return(Clean_RKD_data)
}