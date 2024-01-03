#' @title Filter the patient with the rule for definite MPA/GPA
#' @author Matthieu COQ
#' @description
#' The objective is to filter the RKD data based on inclusion criteria as Definite MPA/GPA 
#' 
#' Version: 1.0
#' 
#' Date: 24-Jan-23
#'
#' @param RKDdata RKD data from \code{\link{clean_rkd}} function
#' @details The filter of the RKD data for Definite GPA/MPA are based on the following filter
#' *  SVV = GPA / MPA
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
DemographicFilterRKD <- function(RKDdata) {
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  if (ncol(RKD_data) == 0 | nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }
  
  ###Select on disease Select only GPA and MPA
  RKD_data_DiseaseFilter <- RKD_data[which(RKD_data$Small.vessel.vasculitis..ANCA.associated. == "Granulomatosis with polyangiitis (Wegener) - Orpha:900" | RKD_data$Small.vessel.vasculitis..ANCA.associated. == "Microscopic polyangiitis (including renal limited vasculitis) - ORPHA:727"),]
  
  
  #####Diagnosis confidence filter
  RKD_data_DiagnosisFilter <- RKD_data_DiseaseFilter[which(RKD_data_DiseaseFilter$Diagnosis.confidence == "Definite"),]
  
  ###### Small vessel immune filter
  
  RKD_data_SmallvesselImmuneFilter <- RKD_data_DiagnosisFilter[which(RKD_data_DiagnosisFilter$Small.vessel.vasculitis..Immune.complex. == ""),]
  
  ########Secondary vascularite filter
  
  RKD_data_SecondaryFilter <- RKD_data_SmallvesselImmuneFilter[which(RKD_data_SmallvesselImmuneFilter$Secondary.vasculitis != "Yes"),]
  
  ########Other filter
  
  RKD_data_OtherFilter <- RKD_data_SecondaryFilter[which(RKD_data_SecondaryFilter$Other != "Yes"), ]
  
  #######medium vessel filter
  
  RKD_data_MediumVesselFilter <- RKD_data_OtherFilter[which(RKD_data_OtherFilter$Medium.vessel.vasculitis == ""), ]
  
  #######large vessel filter
  
  RKD_data_largeVesselFilter <- RKD_data_MediumVesselFilter[which(RKD_data_MediumVesselFilter$Large.vessel.vasculitis == ""), ]
  
  #####Variable vessel filter
  
  RKD_data_VariableVesselFilter <- RKD_data_largeVesselFilter[which(RKD_data_largeVesselFilter$Variable.vessel.vasculitis == ""), ]
  
  ####Last filter
  
  RKD_data_LastFilter <- RKD_data_VariableVesselFilter[which((RKD_data_VariableVesselFilter$At.any.point.ANCA.specificity == "PR3" | 
                                                              RKD_data_VariableVesselFilter$At.any.point.ANCA.specificity == "MPO" |
                                                              RKD_data_VariableVesselFilter$At.any.point.ANCA.specificity == "MPO and PR3") | 
                                                             (RKD_data_VariableVesselFilter$Biopsy.performed. == "Yes" & 
                                                              RKD_data_VariableVesselFilter$Histologically.confirmed.diagnosis == "Yes")
                                                             ),]
  
  Filter_RKD_data <- RKD_data_LastFilter
  
  

  return(Filter_RKD_data)
}