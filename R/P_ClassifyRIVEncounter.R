#' @title This function use the different CPD algorithms to classify the Encounter of RIV data
#' @author Matthieu COQ
#' @description
#'  The objective is to classify the patient of RIV data based on criteria described in the function apply
#'
#'
#' @param RKDdata RIV data from \code{\link{Merge_Encounter_initial}} function
#' @param output_path folder where the Redcap data will be saved
#' @param algorithm function use to classify the RIV patient, the possibility are "Paradise_Encounter" or "CPD Relapse" or "Treatment On/Off" or "CPD LTROT" or "CPD ANCA" or "CPD Treatment" or "CPD LTROT current" or "All" or "CPD Kidney function" 
#' @param interval_from_diagnostics the interval from diagnostics for the algorithm Paradise_Encounter by default 6
#' @param CM_data Data from \code{\link{CPD_Medication_Treatment}} function for the Treatment On/Off algorithm
#' @param IV_data Data from \code{\link{CPD_IVTherapy_Treatment}} function for the Treatment On/Off algorithm
#' @param nb_month Number of month out of treatment for the CPD LTROT function
#' @param nb_day Number of days out of treatment for the CPD LTROT function
#' @param Renal Data from \code{\link{CPD_Renal}} function for the Kidney function 
#' @return The Redcap data with the classification variables in your folder and in an R object 
#' @details This function allows you to call one of  multiple functions to create a classification variable for each RKD encounter. The possible functions each create one new variable
#' * \code{\link{CPDRelapse}} tells us if an encounter is in relapse or not based on rules and models 
#' * \code{\link{Paradise_Encounter}} tells us if we can include the Encounter or not in the Paradise project. To be use after \code{\link{CPDRelapse}}
#' * \code{\link{CPD_Treatment_OnOff}} tells us if an Encounter is under treatment or not
#' * \code{\link{CPD_LTROT}} tells us if a patient is in Long Term Remission Out of Treatment (LTROT)
#' * \code{\link{CPD_ANCA}} tells us the switch of ANCA from one Encounter to the other.
#' * \code{\link{CPD_Treatment}} tells us which treatment is used at each encounter. To be use after \code{\link{CPD_Treatment_OnOff}}
#' * \code{\link{CPD_LTROT_current}} tells us if an Encounter is a LTROT
#' * \code{\link{CPD_Kidney_function}} tells us the status of the Kideney function
#' 
#' @export
ClassifyRIVEncounter = function(RKDdata, output_path, algorithm="All", interval_from_diagnostics=6, CM_data=NULL, IV_data=NULL, nb_month=24, nb_day=730, Renal=NULL) {
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  algorithm <- match.arg(algorithm, c( 'Paradise_Encounter', "CPD Relapse", "Treatment On/Off", "CPD LTROT", "CPD ANCA","CPD Treatment","CPD LTROT current", "All", "CPD Kidney function"))

  RKD_data <- RKDdata
  ###check that you load a real file
  if (ncol(RKD_data) == 0 | nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }

  
  if (algorithm == "Paradise_Encounter") {
    Classify_RKD_data <- Paradise_Encounter(RKD_data, interval_from_diagnostics)
  }
  
  if(algorithm == "CPD Relapse"){
    Classify_RKD_data <- CPDRelapse(RKD_data, interval_from_diagnostics)
  }
  
  if(algorithm == "Treatment On/Off"){
    Classify_RKD_data <- CPD_Treatment_OnOff(IV_data, CM_data, RKDdata)
  }
  
  if(algorithm == "CPD LTROT"){
    Classify_RKD_data <- CPD_LTROT(RKD_data, nb_month)
  }
  
  if(algorithm == "CPD ANCA"){
    Classify_RKD_data <- CPD_ANCA(RKD_data)
  }
  
  if(algorithm ==  "CPD Treatment"){
    Classify_RKD_data <- CPD_Treatment(RKD_data)
  }
  
  if(algorithm ==  "CPD LTROT current"){
    Classify_RKD_data <- CPD_LTROT_current(RKD_data, nb_day)
  }
  
  if(algorithm == "CPD Kidney function"){
    Classify_RKD_data <- CPD_Kidney_function(RKD_data, Renal)
  }
  
  if(algorithm ==  "All"){
    Classify_RKD_CPD_Relapse <- CPDRelapse(RKD_data, interval_from_diagnostics)
    colnames(Classify_RKD_CPD_Relapse)[which(colnames(Classify_RKD_CPD_Relapse)=="Interval.from.diagnosis..months..x")]="Interval.from.diagnosis..months."
    Classify_RKD_Paradise_encounter <- Paradise_Encounter(Classify_RKD_CPD_Relapse, interval_from_diagnostics)
    Classify_RKD_Treatment_OnOff <- CPD_Treatment_OnOff(IV_data, CM_data, Classify_RKD_Paradise_encounter)
    Classify_RKD_Treatment_list <- CPD_Treatment(Classify_RKD_Treatment_OnOff)
    Classify_RKD_Treatment_list_plus <- CPD_immunosup_med(Classify_RKD_Treatment_list)
    Classify_RKD_Corticosteroid <- CPD_Corticosteroids_on_off(Classify_RKD_Treatment_list_plus)
    Classify_RKD_LTROT_patient <- CPD_LTROT(Classify_RKD_Corticosteroid, nb_month)
    Classify_RKD_ANCA <- CPD_ANCA(Classify_RKD_LTROT_patient)
    colnames(Classify_RKD_ANCA)[which(colnames(Classify_RKD_ANCA)=="Date_Last_Follow_up.x")]="Date_Last_Follow_up"
    Classify_RKD_LTROT_Encounter <- CPD_LTROT_current(Classify_RKD_ANCA, nb_day)
    Classify_RKD_data <- CPD_Kidney_function(Classify_RKD_LTROT_Encounter, Renal)
   
  }

  files_test <-  list.dirs(output_path)
  if (identical(files_test, character(0)) == TRUE) {
    stop("Your output folder don't exist")
  }

  output_filename <- file.path(output_path,
    paste0("Redcap_clinical_data_with-classification", "_version", packageVersion('rivpipeline'), "_Date", Sys.Date(), ".csv"))

  write.csv(Classify_RKD_data, output_filename, row.names = FALSE)
  return(Classify_RKD_data)

}