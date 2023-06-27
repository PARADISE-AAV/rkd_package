#' @title ImputationRKD
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 15-Mar-23
#' Objective: The objective is to do imputation for some of the vaiable
#'
#'
#' @param RKDdata RKD data from DemographicFilterRKD function
#' @param output_path folder where the Redcap data will be saved
#' @param algorithm function use to classify the RKD patient, the possibility are "BRelapse" or "Paradise_Encounter"
#' @return The Redcap data with the imputation at some variable
#' @export
#' 
ImputationRKD <- function(RKDdata, output_path, algorithm){
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  if (is.character(algorithm) == FALSE) {
    stop("The argument algorithm need to be a character argument")
  }
  if (algorithm != "RemissionImplementation" ){
    stop("The argument algorithm need to be RemissionImplementation")
  }
  
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if (ncol(RKD_data) == 0 | nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }
  
  if (algorithm == "RemissionImplementation") {
    Imputed_RKD_data <- RemissionImplementation(RKD_data)
  }
  
  if (!dir.exists(output_path))
    stop("Your output folder doesn't exist")

  output_filename <- file.path(
    output_path,
    paste0('Redcap_imputated_data', Sys.Date(), '.csv')
  )
  write.csv(Imputed_RKD_data, output_filename, row.names = FALSE)
  
  return(Imputed_RKD_data)
}