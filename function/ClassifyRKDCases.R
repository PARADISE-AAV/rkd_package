#' @title ClassifyRKDCases
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 15-Mar-23
#' Objective: The objective is to clean the RKD data and send the problematic data to the RKD person
#'
#'
#' @param RKDdata RKD data from DemographicFilterRKD function
#' @param ouput_path folder where the Redcap data will be saved
#' @param algorithm function use to classify the RKD patient, the possibility are "bahareh", "jen"
#' @return The Redcap data cleaned in your folder and in an object
#' @export


ClassifyRKDCases=function(RKDdata, output_path,algorithm){
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument files_name need to be a character argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  if (is.character(algorithm) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  if(algorithm == "bahareh"){
    RKD_data$Group <- bahareh(argument)
  }
  if(algorithm == "jen"){
    RKD_data$Group <- jen(argument)
  }
  
}