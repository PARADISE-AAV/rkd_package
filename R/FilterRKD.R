#' @title This function use the different algorithms to filter the patient of RKD data
#' @author Matthieu COQ
#' @description
#'  to be added
#'
#' @param RKDdata RKD data from \code{\link{clean_rkd}} function
#' @param output_path folder where the Redcap data will be saved
#' @param algorithm function use to filter the RKD patient, the possibility are 
#' @return The Redcap data with the classification variables in your folder and in an R object 
#' @details to be added
#' 
#' @export
FilterRKD = function(RKDdata, output_path, algorithm) {
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  algorithm <- match.arg(algorithm, c('BRelapse'))
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if (ncol(RKD_data) == 0 | nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }
  
  if (algorithm == "BRelapse") {
    Filter_RKD_data <- BRelapseFunction(RKD_data)
  }
  
  
  files_test <-  list.dirs(output_path)
  if (identical(files_test, character(0)) == TRUE) {
    stop("Your output folder don't exist")
  }
  
  output_filename <- file.path(output_path,
                               paste0("Redcap_clinical_data_with-classification", "_version", packageVersion('rkdpipeline'), "_Date", Sys.Date(), ".csv"))
  
  write.csv(Filter_RKD_data, output_filename, row.names = FALSE)
  return(Filter_RKD_data)
  
}