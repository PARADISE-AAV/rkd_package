#' @title LoadRKD
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 24-Jan-23
#' Objective: The objective is to load the RKD data in a datframe object
#'
#'
#' @param files_name RKD data used
#' @return The Redcap data in your folder and in an object
#' @export


LoadRKD=function(files_name){
  ####Test on the argument
  library(stringr)
  if (is.character(files_name) == FALSE) {
    stop("The argument files_name need to be a character argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  #extract the folder and the files 
  b <- max(gregexpr("\\/", files_name)[[1]])
  if(b > 0){
    input_path <- str_sub(files_name, 1, b)
    files_name1 <- str_sub(files_name, b+1, nchar(files_name))
  }else{
    input_path <- getwd()
    files_name1 <- files_name
  }
  
  files_test <-  list.files(input_path, pattern = ".", all.files = FALSE, recursive = TRUE)
  if(identical(files_test, character(0)) == TRUE){
    stop("Your input folder don't exist")
  }
  if(length(which(files_test == files_name1)) != 1){
    stop("There is no files requested or multiple times this files")
  }
  
  
  RKD_data <- read.csv(files_name, header = TRUE)
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  return(RKD_data)
}