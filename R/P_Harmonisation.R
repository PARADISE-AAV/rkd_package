#' @title CPD Harmonisation
#' @author Matthieu COQ
#'
#' @description The Goal is to finalize the harmonisation
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param RIVdata  {"name": "rkd_data","desc": "RIV data from \code{\link{ClassifyRIVEncounter}} function","options": (),"type": "file"}
#' @param output_dir  {"name": "output_dir","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#' @details
#' to be added
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @importFrom data.table %like%
#' @export

CPD_Harmonisation <- function(RIVdata, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(RIVdata))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  # Check that you load a real file
  if (!ncol(RIVdata) || !nrow(RIVdata)) {
    stop("You supplied an empty file")
  }
  
  rkd_data <- RIVdata[ ,c("Paradise.ID", "Date.Of.Visit", "interval_from_diagnosis", "CPD_relapse", "rel.method",  "Urinalysis.Protein", "Urinalysis.Blood", "BVAS.score..calculator.", "Number.of.major.BVAS.items", "Number.of.minor.BVAS.items", "CPD_vasc_gran", "Nature.of.confirmed.relapse", "Treatment_Switch", "CPD_treatment", "Immunosuppressive.medication...other..ATC", "ANCA_Status", "CRP", "Creatinine", "eGFR..calculated." , "Platelet.count.x10.9.L", "Total.white.cell.count.x10.9.L", "Neutrophil.count.x10.9.L", "Lymphocyte.count.x10.9.L", "Neutrophil...Lymphocyte.ratio", "Monocyte.count.x10.9.L", "Absolute.CD19.count..cells.uL.", "ANCA.IF", "Anti.PR3.level", "PR3.titre", "Anti.MPO.level", "MPO.titre", "Immunosuppressive.status", "Corticosteroids_On_off", "Current.corticosteroid.dose",  "Corticosteroids.in.response.to.this.clinical.encounter.episode" , "treatment","ANCA_Switch", "anca_kinetics_longterm", "Gender", "Year.of.birth", "Status.x", "Date_Last_Follow_up","Date.of.event", "At.any.point.ANCA.IF.pattern", "At.any.point.ANCA.specificity", "Any.Induction.Treatment", "Any.Maintenance.Treatment", "Required.renal.replacement.therapy.during.first.presentation", "Renal.recovery..independence.from.dialysis..regardless.of.dialysis.duration.", "Date.of.renal.recovery", "renal_recovery_time", "End.stage.kidney.disease", "Date.of.end.stage.kidney.disease..date.of.commencement.on.dialysis.or.transplant..whichever.first.", "ESKD_time", "Date.of.diagnosis", "Age.at.diagnosis", "Small.vessel.vasculitis..ANCA.associated.", "Small.vessel.vasculitis..Immune.complex.", "Diagnosis.confidence")]
  
  LTROT <- RIVdata[,grep("LTROT_current", colnames(RIVdata))]
  IS <- RIVdata[,grep("immunosup_med", colnames(RIVdata))]
  affectedorgan <- RIVdata[,grep("Systems.involved.at.any.point..choice", colnames(RIVdata))]
  inductiontreatment <- RIVdata[,grep("Induction.treatment.received", colnames(RIVdata))]
  maintenancetreatment <- RIVdata[,grep("Maintenance.treatment.received", colnames(RIVdata))]
  
  harmonized_data <- cbind(rkd_data, LTROT, IS, affectedorgan, inductiontreatment, maintenancetreatment)
  
  colnames(harmonized_data)[2] = "dateOfEncounter"
  colnames(harmonized_data)[3] = "encounter_interval_from_diagnosis"
  colnames(harmonized_data)[6] = "urinalysis_protein"
  colnames(harmonized_data)[7] = "urinalysis_blood"
  colnames(harmonized_data)[8] = "bvasScoreCalculator"
  colnames(harmonized_data)[9] = "numberOfMajorBVASItems"
  colnames(harmonized_data)[10] = "numberOfMinorBVASItems"
  colnames(harmonized_data)[11] = "vasculitic_granuloma"
  colnames(harmonized_data)[12] = "natureofrelapse"
  colnames(harmonized_data)[13] = "TreatmentSwitch"
  colnames(harmonized_data)[14] = "imputed_treatment_status"
  colnames(harmonized_data)[16] = "anca_status"
  colnames(harmonized_data)[17] = "CRPtest"
  colnames(harmonized_data)[18] = "creatininetest"
  colnames(harmonized_data)[19] = "egfr_calculated"
  colnames(harmonized_data)[20] = "platelet_count"
  colnames(harmonized_data)[21] = "total_white_cell_count"
  colnames(harmonized_data)[22] = "neutrophil_count"
  colnames(harmonized_data)[23] = "lymphocyte_count"
  colnames(harmonized_data)[24] = "neutrophil_lympho_ratio"
  colnames(harmonized_data)[25] = "monocyte_count"
  colnames(harmonized_data)[26] = "absolute_cd19_count"
  colnames(harmonized_data)[27] = "anca_if_2"
  colnames(harmonized_data)[28] = "anti_pr3_level"
  colnames(harmonized_data)[29] = "pr3Titre"
  colnames(harmonized_data)[30] = "anti_mpo_level"
  colnames(harmonized_data)[31] = "mpoTitre"
  colnames(harmonized_data)[33] = "corticosteroids"
  colnames(harmonized_data)[34] = "corticosteroid_dose"
  colnames(harmonized_data)[35] = "corticosteroid_response"
  colnames(harmonized_data)[36] = "treatment_type"
  colnames(harmonized_data)[37] = "ancaswitch"
  colnames(harmonized_data)[39] = "gender"
  colnames(harmonized_data)[40] = "yearOfBirth"
  colnames(harmonized_data)[42] = "lastRecordedContact"
  colnames(harmonized_data)[43] = "dateOfDeath"
  colnames(harmonized_data)[44] = "ancaIF"
  colnames(harmonized_data)[45] = "ancaSpec"
  colnames(harmonized_data)[46] = "induction"
  colnames(harmonized_data)[47] = "maintenance"
  colnames(harmonized_data)[48] = "required_renal_replacement"
  colnames(harmonized_data)[49] = "renal_recovery_independ"
  colnames(harmonized_data)[50] = "date_of_renal_recovery_2"
  colnames(harmonized_data)[52] = "ESKDStandard"
  colnames(harmonized_data)[53] = "dateOfESKD"
  colnames(harmonized_data)[55] = "dateOfDiagnosis"
  colnames(harmonized_data)[56] = "ageAtDiagnosis"
  colnames(harmonized_data)[57] = "small_vessel_vas_anca"
  colnames(harmonized_data)[58] = "small_vessel_vas_immune"
  colnames(harmonized_data)[59] = "diagnosis_confidence"
  colnames(harmonized_data)[78] = "affectedOrgan_Renal"
  colnames(harmonized_data)[84] = "affectedOrgan_Cutaneous"
  colnames(harmonized_data)[85] = "affectedOrgan_General"
  colnames(harmonized_data)[88] = "affectedOrgan_Abdominal"
  colnames(harmonized_data)[89] = "affectedOrgan_Mucous_membranes_eyes"
  colnames(harmonized_data)[90] = "affectedOrgan_Cardiovascular"
  colnames(harmonized_data)[91] = "affectedOrgan_Other"
  colnames(harmonized_data)[92] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[93] = "inductiontreatmentType_ Pulsed IV corticosteroids"
  colnames(harmonized_data)[94] = "inductiontreatmentType_ Daily Oral Cyclophosphamide"
  colnames(harmonized_data)[95] = "inductiontreatmentType_Pulsed IV Cyclophosphamide"
  colnames(harmonized_data)[96] = "inductiontreatmentType_Plasma exchange"
  colnames(harmonized_data)[97] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[98] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[99] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[100] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[101] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[102] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[103] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[104] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[105] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[106] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[107] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[108] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[109] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[110] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[111] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[112] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[113] = "inductiontreatmentType_Oral corticosteroids"
  colnames(harmonized_data)[114] = "inductiontreatmentType_Oral corticosteroids"
  
  
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_clinical_data_harmonized', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  
  write.csv(harmonized_data, output_filename, row.names = FALSE)
  return(harmonized_data)
  
}