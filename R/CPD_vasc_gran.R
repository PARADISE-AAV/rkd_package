#' @title CPD vascvs gran
#' @author Matthieu COQ
#'
#' @description to be added
#' 
#' Version: 1.0
#' 
#' Date: 15-Aug-23
#'
#' @param merge_data Data from the merge of encounter and General characteristics in the \code{\link{CPD_Treatment}} function
#' @details
#' to be added
#' 
#' @import lubridate
#' @import dplyr
#' @export

CPD_vasc_vs_gran <- function(merge_data){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  
  merge_data$CPD_vasc_gran="Unknown"
  n=nrow(merge_data)
  for(i in 1:n){
    
    if(merge_data$CPD_relapse[i]=="Definite Relapse"){
      
      if(merge_data$ENT...yes..choice.Bloody.nasal.discharge...crusts...ulcers...granulomata.[i] == "Checked" | 
         merge_data$ENT...yes..choice.Paranasal.sinus.involvement.[i] == "Checked" |
         merge_data$ENT...yes..choice.Subglottic.stenosis.[i] == "Checked" |
         merge_data$ENT...yes..choice.Conductive.hearing.loss.[i] == "Checked" |
         merge_data$ENT...yes..choice.Sensorineural.hearing.loss.[i] == "Checked" |
         merge_data$Chest...yes..choice.Endobronchial.involvement.[i] == "Checked" |
         merge_data$Chest...yes..choice.Nodules.or.cavities.[i] == "Checked" |
         merge_data$Nervous.system...yes..choice.Meningitis.[i] == "Checked" |
         merge_data$Mucous.membranes...eyes...yes..choice.Significant.proptosis.[i] == "Checked"
      ){
        merge_data$CPD_vasc_gran[i] = "Granulomatous"
      }
      
      if(merge_data$General.features...yes..choice.Fever....38.C.[i] == "Checked" |
         merge_data$General.features...yes..choice.Weight.loss.....2.kg.[i] == "Checked" |
         merge_data$Cutaneous...yes..choice.Gangrene.[i] == "Checked" |
         merge_data$Cutaneous...yes..choice.Infarct.[i] == "Checked" |
         merge_data$Cutaneous...yes..choice.Other.skin.vasculitis.[i] == "Checked" |
         merge_data$Cutaneous...yes..choice.Purpura.[i] == "Checked" |
         merge_data$Cutaneous...yes..choice.Ulcer.[i] == "Checked" |
         merge_data$Mucous.membranes...eyes...yes..choice.Mouth.ulcers.[i] == "Checked" |
         merge_data$Mucous.membranes...eyes...yes..choice.Genital.ulcers.[i] == "Checked" |
         merge_data$Mucous.membranes...eyes...yes..choice.Adnexal.inflammation.[i] == "Checked" |
         merge_data$Mucous.membranes...eyes...yes..choice.Conjunctivitis...Blepharitis...Keratitis.[i] == "Checked" |
         merge_data$Mucous.membranes...eyes...yes..choice.Blurred.vision.[i] == "Checked" |
         merge_data$Mucous.membranes...eyes...yes..choice.Sudden.visual.loss.[i] == "Checked" |
         merge_data$Mucous.membranes...eyes...yes..choice.Uveitis.[i] == "Checked" |
         merge_data$Mucous.membranes...eyes...yes..choice.Retinal.changes..vasculitis..thrombosis...exudate...haemorrhage..[i] == "Checked" |
         merge_data$Chest...yes..choice.Wheeze.[i] == "Checked" |
         merge_data$Chest...yes..choice.Pleural.effusion...pleurisy.[i] == "Checked" |
         merge_data$Chest...yes..choice.Infiltrate.[i] == "Checked" |
         merge_data$Chest...yes..choice.Massive.haemoptysis...alveolar.haemorrhage.[i] == "Checked" |
         merge_data$Chest...yes..choice.Respiratory.failure.[i] == "Checked" |
         merge_data$Cardiovascular...yes..choice.Loss.of.pulses.[i] == "Checked" |
         merge_data$Cardiovascular...yes..choice.Valvular.heart.disease.[i] == "Checked" |
         merge_data$Cardiovascular...yes..choice.Pericarditis.[i] == "Checked" |
         merge_data$Cardiovascular...yes..choice.Ischaemic.cardiac.pain.[i] == "Checked" |
         merge_data$Cardiovascular...yes..choice.Cardiomyopathy.[i] == "Checked" |
         merge_data$Cardiovascular...yes..choice.Congestive.cardiac.failure.[i] == "Checked" |
         merge_data$Abdominal...yes..choice.Bloody.diarrhoea.[i] == "Checked" |
         merge_data$Abdominal...yes..choice.Ischaemic.abdominal.pain.[i] == "Checked" |
         merge_data$Abdominal...yes..choice.Peritonitis.[i] == "Checked" |
         merge_data$Renal...yes..choice.Hypertension.[i] == "Checked" |
         merge_data$Renal...yes..choice.Hypertension.[i] == "Checked" |
         merge_data$Renal...yes..choice.Proteinuria..1..[i] == "Checked" |
         merge_data$Renal...yes..choice.Rise.in.serum.creatinine..30..or.fall.in.creatinine.clearance..25..[i] == "Checked" |
         merge_data$Renal...yes..choice.Serum.creatinine....500.uM.L.[i] == "Checked" |
         merge_data$Renal...yes..choice.Serum.creatinine.125.249....uM.L.[i] == "Checked" |
         merge_data$Renal...yes..choice.Serum.creatinine.250.499.....uM.L.[i] == "Checked" |
         merge_data$Nervous.system...yes..choice.Cerebrovascular.accident.[i] == "Checked" |
         merge_data$Nervous.system...yes..choice.Cranial.nerve.palsy.[i] == "Checked" |
         merge_data$Nervous.system...yes..choice.Headache.[i] == "Checked" |
         merge_data$Nervous.system...yes..choice.Mononeuritis.multiplex.[i] == "Checked" |
         merge_data$Nervous.system...yes..choice.Organic.confusion.[i] == "Checked" |
         merge_data$Nervous.system...yes..choice.Seizures..not.hypertensive..[i] == "Checked" |
         merge_data$Nervous.system...yes..choice.Sensory.peripheral.neuropathy.[i] == "Checked" |
         merge_data$Nervous.system...yes..choice.Spinal.cord.lesion.[i] == "Checked"
        
      ){
        merge_data$CPD_vasc_gran[i] = "Vasculitic"
      }
      
    }
    
  }
  
  return(merge_data)
}