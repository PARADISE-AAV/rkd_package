#' @title This function use the different CPD algorithms to classify the Encounter of RKD data
#' @author Matthieu COQ
#' @description
#'  The objective is to classify the patient of RKD data based on criteria described in the function apply
#'
#'
#' @param RKDdata RKD data from \code{\link{DemographicFilterRKD}} function
#' @param output_path folder where the Redcap data will be saved
#' @param algorithm function use to classify the RKD patient, the possibility are "BRelapse" or "Paradise_Encounter" or "CPD Relapse" or "Treatment On/Off
#' @param interval_from_diagnostics the interval from diagnostics for the algorithm Paradise_Encounter by default 6
#' @param rawRKDdata Raw data from \code{\link{load_rkd}} function
#' @return The Redcap data cleaned in your folder and in an object 
#' @details This function use different function to classify the encounter of RKD data. this function are described in their function.
#' * CPD Relapse tells us if an encounter is in relapse or not based on rules and models 
#' * BRelapse tells us if an encounter is in relapse or not based on rules
#' * Paradise_Encounter tells us if we can include the Encounter or not in the Paradise project
#' * Treatment On/Off tells us if an Encounter is under treatment or not
#' 
#' @export
ClassifyRKDEncounter = function(RKDdata, output_path, algorithm, interval_from_diagnostics=6, rawRKDdata) {
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  algorithm <- match.arg(algorithm, c('BRelapse', 'Paradise_Encounter', "CPD Relapse"))

  RKD_data <- RKDdata
  ###check that you load a real file
  if (ncol(RKD_data) == 0 | nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }

  if (algorithm == "BRelapse") {
    Classify_RKD_data <- BRelapseFunction(RKD_data)
  }
  if (algorithm == "Paradise_Encounter") {
    Classify_RKD_data <- Paradise_Encounter(RKD_data, interval_from_diagnostics)
  }
  if(algorithm == "CPD Relapse"){
    Classify_RKD_data <- CPD_Relapse(RKD_data)
     
  }
  if(algorithm == "Treatment On/Off"){
    rkd_treatment <- Treatment_On_Off(RKDdata)
    Classify_RKD_data <- rkdpipeline:::IVTherapy(rkd, rkd_treatment)
  }

  files_test <-  list.dirs(output_path)
  if (identical(files_test, character(0)) == TRUE) {
    stop("Your output folder don't exist")
  }

  output_filename <- file.path(output_path,
    paste0("Redcap_clinical_data_with-classification", Sys.Date(), ".csv"))

  write.csv(Classify_RKD_data, output_filename, row.names = FALSE)
  return(Classify_RKD_data)

}