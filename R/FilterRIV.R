#' @title This function use the different algorithms to filter the patient of RIV data
#' @author Matthieu COQ
#' @description
#' The objective is to filter the RIV data based on inclusion criteria defined in several algorithms
#' 
#' Version: 1.0
#' 
#' Date: 03-Jan-24
#'
#' @param RKDdata a list from \code{\link{SplitRIV}} function
#' @param output_path folder where the Redcap data will be saved
#' @param algorithm function use to filter the RKD patient, the possibility are "Definite GPA/MPA" or "Definite GPA/MPA/EGPA"
#' @return The Redcap data with the classification variables in your folder and in an R object 
#' @details The filter of the RKD data are based on the following filter
#' * Definite MPA/GPA without secondary vasculitis with more explanation in \code{\link{DemographicFilterRIV}}
#' * Definite MPA/GPA/EGPA without secondary vasculitis with more explanation in \code{\link{DemographicFilterRIV_EGPA}}
#' 
#' @export
FilterRIV = function(RKDdata, output_path, algorithm) {
  ####Test on the argument
  if (is.list(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  algorithm <- match.arg(algorithm, c("Definite GPA/MPA","Definite GPA/MPA/EGPA"))
  
  RKD_data <- RKDdata$Initial
  ###check that you load a real file
  if (ncol(RKD_data) == 0 | nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }
  
  if (algorithm == "Definite GPA/MPA") {
    Filter_RKD_data <- DemographicFilterRIV(RKD_data)
  }
  if (algorithm == "Definite GPA/MPA/EGPA") {
    Filter_RKD_data <- DemographicFilterRIV_EGPA(RKD_data)
  }
  
  files_test <-  list.dirs(output_path)
  if (identical(files_test, character(0)) == TRUE) {
    stop("Your output folder don't exist")
  }
  
  output_filename <- file.path(output_path,
                               paste0("Redcap_clinical_data_filter", "_version", packageVersion('rivpipeline'), "_Date", Sys.Date(), ".csv"))
  
  write.csv(Filter_RKD_data, output_filename, row.names = FALSE)
  return(Filter_RKD_data)
  
}