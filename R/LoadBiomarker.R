#' @title LoadBiomarker
#' @author Matthieu COQ/Yagmur DOGAY
#' Version: 1.0
#' Date: 24-Jan-23
#' Objective: The objective is to load the Biomarker data in a datframe object
#'
#'
#' @param files_name Biomarkerdata used
#' @return The Biomarker Data into DataFrame
#' @import stringr
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
  
  
  Biomarker_data <- read.csv(files_name, header = TRUE)
  ###check that you load a real file
  if(ncol(Biomarker_data)==0 | nrow(Biomarker_data)==0){
    stop("You give an empty files")
  }
  
  return(Biomarker_data)
}