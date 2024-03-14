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

  
  
  dataset
  
}