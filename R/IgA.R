#' @title Filter the patient with the rule for IgA vasculitis (Henoch-Schonlein)
#' @author Matthieu COQ
#' @description
#' The objective is to filter the RIV data based on inclusion criteria as IgA vasculitis (Henoch-Schonlein) 
#' 
#' Version: 2.0
#' 
#' Date: 24-Jan-23
#'
#' @param RKDdata RIV data from \code{\link{clean_riv}} function
#' @details 
#' 
#' The filter of the RIV data for IgA vasculitis (Henoch-Schonlein) are based on the following filter
#' *  small_vessel_vas_immune = IgA vasculitis (Henoch-Schonlein) - ORPHA:761
#' * AND diagnosis_confidence_init = definite 
#' * AND small_vessel_vas_immune != Anti-GBM disease - ORPHA:375 OR Cryoglobulinemic vasculitis - ORPHA:91138
#' * AND small_vessel_vas_anca != GPA OR MPA OR EGPA OR ANCA vasculitis unclassified
#' * AND secondary_vasculitis != Yes 
#' * AND ’Other’ != Yes
#' * AND medium_vessel_vasculitis’ != 1 OR 2 
#' * AND large_vessel_vasculitis != 1 or 2 
#' * AND variable_vessel_vasculitis != 1 or 2
#' 
#' 
#' @return The Redcap data filter in your folder and an R object
#' @export


IgA <- function(RKDdata) {
  
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  if (ncol(RKD_data) == 0 | nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }
  
  ###Select on disease Select only GPA and MPA
  RKD_data_DiseaseFilter <- RKD_data[which(RKD_data$Small.vessel.vasculitis..ANCA.associated. == ""),]
  
  
  #####Diagnosis confidence filter
  RKD_data_DiagnosisFilter <- RKD_data_DiseaseFilter[which(RKD_data_DiseaseFilter$Diagnosis.confidence == "Definite"),]
  
  ###### Small vessel immune filter
  
  RKD_data_SmallvesselImmuneFilter <- RKD_data_DiagnosisFilter[which(RKD_data_DiagnosisFilter$Small.vessel.vasculitis..Immune.complex. == "IgA vasculitis (Henoch-Schonlein) - ORPHA:761"),]
  
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
  
  
  
  Filter_RKD_data <- RKD_data_VariableVesselFilter
  
  
  
  return(Filter_RKD_data)
  
}