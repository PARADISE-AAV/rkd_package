#' @title Filter the patient with the rule for Healthy Control
#' @author Matthieu COQ
#' @description
#' The objective is to filter the RIV data based on inclusion criteria as Healthy Control 
#' 
#' Version: 2.0
#' 
#' Date: 24-Jan-23
#'
#' @param RKDdata RIV data from \code{\link{clean_riv}} function
#' @details The filter of the RIV data for Healthy Control are based on the following filter
#' *  Type.of.Patient = Healthy Control
#' 
#' @return The Redcap data filter in your folder and an R object
#' @export

HealthyControl <- function(RKDdata) {
  
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  if (ncol(RKD_data) == 0 | nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }
  
  ###Select on disease Select only GPA and MPA
  RKD_data_DiseaseFilter <- RKD_data[which(RKD_data$Type.of.Patient == "Healthy Control"),]
  
  
  Filter_RKD_data <- RKD_data_DiseaseFilter
  
  
  
  return(Filter_RKD_data)
  
}