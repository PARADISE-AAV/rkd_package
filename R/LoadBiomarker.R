#' @title Load Biomarkerdata
#' @author Matthieu COQ/Yagmur DOGAY
#' @description
#' 
#' The objective is to load the Biomarker data in a dataframe object
#' 
#' Version: 1.0
#' 
#' Date: 24-Jan-23
#' 
#'
#' @param files_name Biomarkerdata used
#' @return The Biomarker Data into DataFrame
#' @details The load of Biomarker data in a dataframe object is use to add the Biomarker data to the rkd data. 
#' 
#' Please mind that some verification of your files ad the date format can interrupt the function.
#' 
#' 
#' @import stringr
#' @export


LoadBiomarker=function(files_name){
  ####Test on the argument
  if (is.character(files_name) == FALSE) {
    stop("The argument files_name need to be a character argument")
  }
  #extract the folder and the files 
  b <- max(gregexpr("\\/", files_name)[[1]])
  if(b > 0){
    input_path <- stringr::str_sub(files_name, 1, b)
    files_name1 <- stringr::str_sub(files_name, b+1, nchar(files_name))
  }else{
    input_path <- getwd()
    files_name1 <- files_name
  }
  dir_test <- list.dirs(input_path)
  files_test <-  list.files(input_path, pattern = ".", all.files = FALSE, recursive = TRUE)
  if(identical(dir_test, character(0)) == TRUE){
    stop("Your input folder don't exist")
  }
  if(length(which(files_test == files_name1)) != 1){
    stop("There is no files requested or multiple times this files")
  }
  
  
  Biomarker_data <- read.table(files_name, header = TRUE, sep = ",")
  ###check that you load a real file
  if(ncol(Biomarker_data)==0 | nrow(Biomarker_data)==0){
    stop("You give an empty files")
  }
  
  Biomarker_data$Date.Of.Visit=as.Date(Biomarker_data$Date.Of.Visit)
  
  if(min(year(Biomarker_data$Date.Of.Visit),na.rm=T)<1950){
    warning("You have weird dates in your dataset can you check the format of the date of sample")
  }
  
  if(length(which(is.na(Biomarker_data$Date.Of.Visit) == TRUE))>0){
    warning("You have a problem of format of date in your files. Be sure that the format is YYYY-MM-DD")
  }
  
  return(Biomarker_data)
}