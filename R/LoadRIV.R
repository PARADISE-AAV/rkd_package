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
  
  if(length(which(rkd_data$Date.of.diagnosis<rkd_data$Date.of.Birth))>0){
    warning("There is a problem with format of date")
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
      warning("We have a problem with the Date of diagnosis")
      
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