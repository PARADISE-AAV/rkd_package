#' @title LoadBiomarker
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 24-Jan-23
#' Objective: The objective is to load the Biomarker data in a datframe object
#'
#'
#' @param input_path folder where the Biomarker are
#' @param file_name Biomarkerdata used
#' @return The Biomarker Data as datafrme



LoadRKD=function(input_path, file_name){
  ####Test on the argument
  if (is.character(input_path) == FALSE) {
    stop("The argument input_path need to be a character argument")
  }
  if (is.character(files) == FALSE) {
    stop("The argument files need to be a character argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  if (is.character(date) == FALSE) {
    stop("The argument date need to be a character argument")
  }
  
  files_test <-  list.files(input_path, pattern = ".", all.files = FALSE, recursive = TRUE)
  if(identical(files_test, character(0)) == TRUE){
    stop("Your input folder don't exist")
  }
  if(length(which(files_test == files)) != 1){
    stop("There is no files requested or multiple times this files")
  }
  
  setwd(input_path)
  Biomarker_data <- read.csv(files, header = TRUE)
  ###check that you load a real file
  if(ncol(Biomarker_data)==0 | nrow(Biomarker_data)==0){
    stop("You give an empty files")
  }

  return(Biomarker_data)
}