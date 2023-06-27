#' Load the Rare Kidney Disease dataset
#'
#' The objective is to load the RKD data from disk into an R dataframe object.
#'
#' The output of this function is re-used in the other functions of the package.
#' An empty or non-existent file or folder will result in an error.
#'
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 24-Jan-23
#'
#' @param file_name String. Directory where the RKD files are kept.
#'
#' @return A data frame containing the RKD dataset.
#'
#' @export
load_rkd <- function (file_name) {
  stopifnot(is.character(file_name))
  stopifnot(length(file_name) == 1)
  containing_dir <- dirname(file_name)
  if (!dir.exists(containing_dir)) {
    stop('Your input folder doesn\'t exist')
  }

  dataset <- read.csv(file_name)
  if (!ncol(dataset) | !nrow(dataset)) {
    stop('File is empty')
  }

  dataset
}