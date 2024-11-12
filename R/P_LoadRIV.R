#' @title Load the RIV dataset
#' @description
#'  
#' The objective is to load the RIV data from disk into an R dataframe object.
#' 
#' Version: 1.0
#' 
#' Date: 24-Jan-23
#' @details
#'  
#' The output of this function is re-used in the other functions of the package.
#' An empty or non-existent file or folder will result in an error.
#' The variable consider as date format  are transform in Date format and check if there is no problem.
#' 
#'
#' @author Matthieu COQ
#'
#' @param file_name String. Directory where the RIV files are kept.
#'
#' @return A data frame containing the RKD dataset.
#' @import textclean
#' @import dplyr
#' @export
load_riv <- function (file_name) {
  stopifnot("Your argument need to be a character"=is.character(file_name))
  stopifnot(length(file_name) == 1)
  containing_dir <- dirname(file_name)
  if (!dir.exists(containing_dir)) {
    stop('Your input folder doesn\'t exist')
  }

  dataset <- read.csv(file_name, check.names = TRUE)
  if (!ncol(dataset) | !nrow(dataset)) {
    stop('File is empty')
  }
  colnames(dataset) <- textclean::replace_non_ascii(colnames(dataset))
  dataset$Immunosuppressive.status <- textclean::replace_non_ascii(dataset$Immunosuppressive.status)

  rkd_data <- rkd_parse_dates(dataset)
  
  Encounter=rkd_data[which(rkd_data$Repeat.Instrument=="Encounters"),]
  if(length(which(is.na(Encounter$Date.Of.Visit)==TRUE))>0){
   print(paste(Encounter[which(is.na(Encounter$Date.Of.Visit)==TRUE),c("RKD.ID","Repeat.Instance")]))
    stop("Date of Visit are missing!!")
  }
  
  duplicate_rows_unique_values <- Encounter %>%
    group_by(RKD.ID, Date.Of.Visit) %>%
    filter(n() > 1) %>%
    select(RKD.ID, Date.Of.Visit) %>%  # Select only the columns you are interested in
    distinct() %>%  # Select unique rows among the duplicates
    ungroup()
  duplicate_rows_unique_values$Date.Of.Visit=as.character(duplicate_rows_unique_values$Date.Of.Visit)
  if(nrow(duplicate_rows_unique_values)>0){
    print(paste(duplicate_rows_unique_values))
    stop("Duplicate Encounter")
  }
  
  if(length(which(rkd_data$Date.of.diagnosis<rkd_data$Date.of.Birth))>0){
    warning("There is a problem with format of date with Date of Diagnosis below Date of birth")
  }
 
  
  a <- grep("Date",colnames(rkd_data))
  a <- a[-grep("known.unknown",colnames(rkd_data)[a])]
  for(i in a){
    if(length(which(is.na(rkd_data[,i]) == TRUE))>0){
      warning("You have a problem of format of date in your files. Be sure that the format is YYYY-MM-DD")
    }
    
    if(min(year(rkd_data[,i]),na.rm=T)<1900){
      warning("You have weird dates in your dataset can you check the format of the date of sample")
    }
    
    if(max(year(rkd_data[,i]),na.rm=T)>year(Sys.time())){
      warning("The time machine is still not created. You have a date in the future")
    }
  }
  for(i in 1: nrow(rkd_data)){
    if(is.na(rkd_data$Date.of.diagnosis[i])== F & is.na(rkd_data$Date.of.onset.of.symptons.[i])== F  & rkd_data$Date.of.diagnosis[i]<rkd_data$Date.of.onset.of.symptons.[i]){
      warning("We have a problem with the Date of diagnosis as the date of onset is after the date of diagnosis")
      print(rkd_data$RKD.ID[i])
    }
  }
  
  
  rkd_data
  
}


#' @import stringr
#' @import dplyr
#' @import lubridate
rkd_parse_dates <- function(data) {
  date_columns <- stringr::str_subset(colnames(data), "Date")
  date_columns <- stringr::str_subset(date_columns, "known.unknown", negate = TRUE)
  dplyr::mutate(
    data,
    dplyr::across(dplyr::all_of(date_columns), lubridate::as_date)
  )
}