#' @title LoadFW
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 24-Jan-23
#' Objective: The objective is to load the Freezerwork data in a dataframe object
#'
#'
#' @param folder_name folder name of the Freezerwork
#' @param matrix_type type of blood matrix needed
#' @details
#' This function is for loading the Freezerwork depending of the following matrix
#' -Serum
#' -Urine
#' -Plasma
#' -DNA
#' -RNA
#' 
#' 
#' @return The Redcap data in your folder and in an object
#' @export

LoadFW=function(folder_name, matrix_type){
  ####Test on the argument
  if (is.character(folder_name) == FALSE) {
    stop("The argument folder_name need to be a character argument")
  }
  if (is.character(matrix_type) == FALSE) {
    stop("The argument matrix_type need to be a character argument")
  }
  
  #extract the folder and the files 
  
  
  dir_test <- list.dirs(folder_name)
  files_test <-  list.files(folder_name, pattern = ".", all.files = FALSE, recursive = TRUE)
  if(identical(dir_test, character(0)) == TRUE){
    stop("Your input folder don't exist")
  }
  files_name1=files_test[grep(matrix_type,files_test)]
  
  if(length(files_name1) != 1){
    stop("There is no files requested or multiple times this files")
  }
  files_name1=paste(folder_name,files_name1,sep="/")
  
  FW_data <- read.table(files_name1, header = TRUE, sep = ';', fill = TRUE)
  ###check that you load a real file
  if(ncol(FW_data)==0 | nrow(FW_data)==0){
    stop("You give an empty files")
  }
  
  return(FW_data)
}