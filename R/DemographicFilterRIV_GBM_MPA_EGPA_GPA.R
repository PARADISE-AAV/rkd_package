#' @title Filter the patient with the rule for definite MPA/GPA/GPA and Anti-GBM
#' @author Matthieu COQ
#' @description
#' The objective is to filter the RIV data based on inclusion criteria as Definite MPA/GPA/EGPA
#' 
#' Version: 1.0
#' 
#' Date: 03-Jan-24
#'
#' @param RKDdata RIV data from \code{\link{SplitRIV}} function
#' @details The filter of the RKD data for Definite GPA/MPA/EGPA are based on the following filter
#' *  SVV = GPA / MPA / EGPA
#' *  AND diagnosis confidence = definite
#' *  AND SVV (IC) != Anti-GBM, IgA, cryo
#' *  AND Secondary vasculitis != Yes
#' *  AND 'Other' != Yes
#' *  AND 'Medium vessel' != 1 or 2
#' *  AND 'large vessel' != 1 or 2
#' *  AND 'variable vessel' != 1 or 2
#' *  AND (ANCA subtype = PR3 / MPO / both) OR (biopsy = Definite vasculitis AND histologically confirmed = YES)
#' 
#' 
#' @return The Redcap data filter in your folder and an R object
#' @export
DemographicFilterRIV_GBM_MPA_EGPA_GPA <- function(RKDdata) {
  
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  if (ncol(RKD_data) == 0 | nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }
  
  RKD_data_DiseaseFilter <- RKD_data
  
  
  #####Diagnosis confidence filter
  RKD_data_DiagnosisFilter <- RKD_data_DiseaseFilter[which(RKD_data_DiseaseFilter$Diagnosis.confidence == "Definite"),]
  
  ###### Small vessel immune filter
  
  RKD_data_SmallvesselImmuneFilter <- RKD_data_DiagnosisFilter[which(RKD_data_DiagnosisFilter$Small.vessel.vasculitis..Immune.complex. == "" | RKD_data_DiagnosisFilter$Small.vessel.vasculitis..Immune.complex. == "Anti-GBM disease - ORPHA:375"),]
  
  ########Secondary vascularite filter
  
  RKD_data_SecondaryFilter <- RKD_data_SmallvesselImmuneFilter[which(RKD_data_SmallvesselImmuneFilter$Secondary.vasculitis != "Yes"),]
  
  ########Other filter
  
  RKD_data_OtherFilter <- RKD_data_SecondaryFilter
  
  #######medium vessel filter
  
  RKD_data_MediumVesselFilter <- RKD_data_OtherFilter[which(RKD_data_OtherFilter$Medium.vessel.vasculitis == ""), ]
  
  #######large vessel filter
  
  RKD_data_largeVesselFilter <- RKD_data_MediumVesselFilter[which(RKD_data_MediumVesselFilter$Large.vessel.vasculitis == ""), ]
  
  #####Variable vessel filter
  
  RKD_data_VariableVesselFilter <- RKD_data_largeVesselFilter[which(RKD_data_largeVesselFilter$Variable.vessel.vasculitis == ""), ]
  
  ####Last filter
  
  
  Filter_RKD <- RKD_data_VariableVesselFilter
  
  return(Filter_RKD)
  
}