#' @title CPD Harmonisation with full export
#' @author Matthieu COQ
#'
#' @description The Goal is to finalize the harmonization
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param RIVdata  {"name": "rkd_data","desc": "RIV data from \code{\link{ClassifyRIVEncounter}} function","options": (),"type": "file"}
#' @param output_dir  {"name": "output_dir","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#' @details
#' 
#' In this function, we modify the column names to be align with the [Code Overview](https://3.basecamp.com/3790396/buckets/31349172/google_documents/7056193063) document. 
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @importFrom data.table %like%
#' @export

CPD_Harmonisation_full <- function(RIVdata, output_dir){
  
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
  
  colnames(RIVdata)[grep("Induction.treatment.received", colnames(RIVdata))] <- c("inductiontreatmentType_Oral corticosteroids", "inductiontreatmentType_ Pulsed IV corticosteroids", "inductiontreatmentType_ Daily Oral Cyclophosphamide", 
                                                                                  "inductiontreatmentType_Pulsed IV Cyclophosphamide", "inductiontreatmentType_Plasma exchange", "inductiontreatmentType_Rituximab", "inductiontreatmentType_MMF", 
                                                                                  "inductiontreatmentType_Azathioprine", "inductiontreatmentType_Methotrexate", "Induction.treatment.received..choice.Leflunomide.", "inductiontreatmentType_Avacopan (C5aR inhibitor)", 
                                                                                  "inductiontreatmentType_Other (indu_treat_recv_atc)", "indu_treat_recv_atc")
  colnames(RIVdata)[grep("Maintenance.treatment.received", colnames(RIVdata))] <- c("maintenancetreatmentType_Oral corticosteroids", "maintenancetreatmentType_Rituximab", "maintenancetreatmentType_Azathioprine", 
                                                                                    "maintenancetreatmenttype_Cyclophosphamide", "maintenancetreatmenttype_MMF",   
                                                                                   "maintenancetreatmentType_Methotrexate", "Maintenance.treatment.received..choice.Leflunomide." , "maintenancetreatmentType_Avacopan (C5aR inhibitor)", 
                                                                                  "maintenancetreatmentType_Other (indu_treat_recv_atc)", "maint_treat_recv_atc")
  
  
  colnames(RIVdata)[which(colnames(RIVdata)=="interval_from_diagnosis")] = "encounter_interval_from_diagnosis"
  colnames(RIVdata)[which(colnames(RIVdata)=="Urinalysis.Protein")] = "urinalysis_protein"
  colnames(RIVdata)[which(colnames(RIVdata)=="Urinalysis.Blood")] = "urinalysis_blood"
  colnames(RIVdata)[which(colnames(RIVdata)=="BVAS.score..calculator.")] = "bvasScoreCalculator"
  colnames(RIVdata)[which(colnames(RIVdata)=="Number.of.major.BVAS.items")] = "numberOfMajorBVASItems"
  colnames(RIVdata)[which(colnames(RIVdata)=="Number.of.minor.BVAS.items")] = "numberOfMinorBVASItems"
  colnames(RIVdata)[which(colnames(RIVdata)=="CPD_vasc_gran")] = "vasculitic_granuloma"
  colnames(RIVdata)[which(colnames(RIVdata)=="Nature.of.confirmed.relapse")] = "natureofrelapse"
  colnames(RIVdata)[which(colnames(RIVdata)=="Treatment_Switch")] = "TreatmentSwitch"
  colnames(RIVdata)[which(colnames(RIVdata)=="CPD_treatment")] = "imputed_treatment_status"
  colnames(RIVdata)[which(colnames(RIVdata)=="ANCA_Status")] = "anca_status"
  colnames(RIVdata)[which(colnames(RIVdata)=="CRP")] = "CRPtest"
  colnames(RIVdata)[which(colnames(RIVdata)=="Creatinine")] = "creatininetest"
  colnames(RIVdata)[which(colnames(RIVdata)=="eGFR..calculated.")] = "egfr_calculated"
  colnames(RIVdata)[which(colnames(RIVdata)=="Platelet.count.x10.9.L")] = "platelet_count"
  colnames(RIVdata)[which(colnames(RIVdata)=="Total.white.cell.count.x10.9.L")] = "total_white_cell_count"
  colnames(RIVdata)[which(colnames(RIVdata)=="Neutrophil.count.x10.9.L")] = "neutrophil_count"
  colnames(RIVdata)[which(colnames(RIVdata)=="Lymphocyte.count.x10.9.L")] = "lymphocyte_count"
  colnames(RIVdata)[which(colnames(RIVdata)=="Neutrophil...Lymphocyte.ratio")] = "neutrophil_lympho_ratio"
  colnames(RIVdata)[which(colnames(RIVdata)=="Monocyte.count.x10.9.L")] = "monocyte_count"
  colnames(RIVdata)[which(colnames(RIVdata)=="Absolute.CD19.count..cells.uL.")] = "absolute_cd19_count"
  colnames(RIVdata)[which(colnames(RIVdata)=="ANCA.IF")] = "anca_if_2"
  colnames(RIVdata)[which(colnames(RIVdata)=="Anti.PR3.level")] = "anti_pr3_level"
  colnames(RIVdata)[which(colnames(RIVdata)=="PR3.titre")] = "pr3Titre"
  colnames(RIVdata)[which(colnames(RIVdata)=="Anti.MPO.level")] = "anti_mpo_level"
  colnames(RIVdata)[which(colnames(RIVdata)=="MPO.titre")] = "mpoTitre"
  colnames(RIVdata)[which(colnames(RIVdata)=="Corticosteroids_On_off")] = "imputed_corticosteroids"
  colnames(RIVdata)[which(colnames(RIVdata)=="Current.corticosteroid.dose")] = "corticosteroid_dose"
  colnames(RIVdata)[which(colnames(RIVdata)=="Corticosteroids.in.response.to.this.clinical.encounter.episode" )] = "corticosteroid_response"
  colnames(RIVdata)[which(colnames(RIVdata)=="treatment" )] = "treatment_type"
  colnames(RIVdata)[which(colnames(RIVdata)=="ANCA_Switch" )] = "ancaswitch"
  colnames(RIVdata)[which(colnames(RIVdata)=="Gender" )] = "gender"
  colnames(RIVdata)[which(colnames(RIVdata)=="Year.of.birth" )] = "yearOfBirth"
  colnames(RIVdata)[which(colnames(RIVdata)=="Date_Last_Follow_up"  )] = "lastRecordedContact"
  colnames(RIVdata)[which(colnames(RIVdata)=="Date.of.event"  )] = "dateOfDeath"
  colnames(RIVdata)[which(colnames(RIVdata)=="At.any.point.ANCA.IF.pattern"  )] = "ancaIF"
  colnames(RIVdata)[which(colnames(RIVdata)=="At.any.point.ANCA.specificity"  )] = "ancaSpec"
  colnames(RIVdata)[which(colnames(RIVdata)=="Any.Induction.Treatment")] = "induction"
  colnames(RIVdata)[which(colnames(RIVdata)=="Any.Maintenance.Treatment")] = "maintenance"
  colnames(RIVdata)[which(colnames(RIVdata)=="Required.renal.replacement.therapy.during.first.presentation")] = "required_renal_replacement"
  colnames(RIVdata)[which(colnames(RIVdata)=="Renal.recovery..independence.from.dialysis..regardless.of.dialysis.duration.")] = "renal_recovery_independ"
  colnames(RIVdata)[which(colnames(RIVdata)=="Date.of.renal.recovery")] = "date_of_renal_recovery_2"
  colnames(RIVdata)[which(colnames(RIVdata)=="End.stage.kidney.disease")] = "ESKDStandard"
  colnames(RIVdata)[which(colnames(RIVdata)=="Date.of.end.stage.kidney.disease..date.of.commencement.on.dialysis.or.transplant..whichever.first.")] = "dateOfESKD"
  colnames(RIVdata)[which(colnames(RIVdata)=="Date.of.diagnosis")] = "dateOfDiagnosis"
  colnames(RIVdata)[which(colnames(RIVdata)=="Age.at.diagnosis")] = "ageAtDiagnosis"
  colnames(RIVdata)[which(colnames(RIVdata)=="Small.vessel.vasculitis..ANCA.associated.")] = "small_vessel_vas_anca"
  colnames(RIVdata)[which(colnames(RIVdata)=="Small.vessel.vasculitis..Immune.complex.")] = "small_vessel_vas_immune"
  colnames(RIVdata)[which(colnames(RIVdata)=="Diagnosis.confidence" )] = "diagnosis_confidence"
  colnames(RIVdata)[which(colnames(RIVdata)=="Systems.involved.at.any.point..choice.Kidney." )] = "affectedOrgan_Renal"
  colnames(RIVdata)[which(colnames(RIVdata)=="Systems.involved.at.any.point..choice.Mucocutaneous." )] = "affectedOrgan_Cutaneous"
  colnames(RIVdata)[which(colnames(RIVdata)=="Systems.involved.at.any.point..choice.Musculoskeletal." )] = "affectedOrgan_General"
  colnames(RIVdata)[which(colnames(RIVdata)=="Systems.involved.at.any.point..choice.Abdominal." )] = "affectedOrgan_Abdominal"
  colnames(RIVdata)[which(colnames(RIVdata)=="Systems.involved.at.any.point..choice.Eye." )] = "affectedOrgan_Mucous_membranes_eyes"
  colnames(RIVdata)[which(colnames(RIVdata)=="Systems.involved.at.any.point..choice.Cardiovascular." )] = "affectedOrgan_Cardiovascular"
  colnames(RIVdata)[which(colnames(RIVdata)=="Systems.involved.at.any.point..choice.Other." )] = "affectedOrgan_Other"
  colnames(RIVdata)[which(colnames(RIVdata)=="kidney_fx_status" )] = "kidney_function_status"
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_clinical_data_harmonized', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  
  write.csv(RIVdata, output_filename, row.names = FALSE)
  return(RIVdata)
  

}