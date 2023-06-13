#' @title ClinicalFilterRKD
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 21-Mar-23
#' Objective: The objective is to filter the RKD data based on clinical criteria like the classification of patient
#'
#'
#' @param RKDdata RKD data from ClassifyRKDCases function or AddBiomarker function
#' @param ouput_path folder where the filter RKD data will be saved
#' @param algorithm The algorithm used in the ClassifyRKDCases function
#' @return The Redcap data cleaned in your folder and in an object
#' @export
#' 

ClinicalFilterRKD=function(RKDdata, output_path, algorithm){
  
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
  if (algorithm != "BRelapse" & algorithm != "Paradise_Encounter"){
    stop("The argument algorithm need to be BRelapse or Paradise_Encounter")
  }
  
  RKD_data <- RKDdata
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  if(algorithm == "BRelapse"){
    Filter_RKD_data <- RKD_data[which(RKD_data$Relapse != ""),]
  }
  if (algorithm == "Paradise_Encounter") {
    Filter_RKD_data <- RKD_data[which(RKD_data$Paradise.Encounters == 1),]
  }
  
  
  files_test <-  list.dirs(output_path)
  if(identical(files_test, character(0)) == TRUE){
    stop("Your output folder don't exist")
  }
  write.csv(Filter_RKD_data, paste(output_path,"/Redcap_clinical_data_filter", Sys.Date() , ".csv", sep=""), row.names = F)
  return(Filter_RKD_data)
  
}