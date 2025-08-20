#' @title Filter the patient with the rule for double postitive
#' @author Matthieu COQ
#' @description
#' The objective is to filter the RIV data based on inclusion criteria as double postive 
#' 
#' Version: 2.0
#' 
#' Date: 24-Jan-23
#'
#' @param RKDdata RIV data from \code{\link{clean_riv}} function
#' @details 
#' The filter of the RIV data for double positive are based on the following filter
#' *   SVV (IC) = Anti-GBM disease – ORPHA:375
#' *  AND diagnosis confidence = definite
#' *  AND SVV = GPA / MPA / EGPA / ANCA vasculitis unclassified
#' *  AND SVV (IC) != IgA, cryo
#' *  AND Secondary vasculitis != Yes
#' *  AND 'Medium vessel' != 1 or 2
#' *  AND 'large vessel' != 1 or 2
#' *  AND 'variable vessel' != 1 or 2
#' 
#' 
#' @return The Redcap data filter in your folder and an R object
#' @export

DoublePositive <- function(RKDdata) {
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  if (ncol(RKD_data) == 0 | nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }
  
  ###Select on disease Select only GPA and MPA
  RKD_data_DiseaseFilter <- RKD_data[which(RKD_data$Small.vessel.vasculitis..Immune.complex. == "Anti-GBM disease - ORPHA:375"),]
  
  
  #####Diagnosis confidence filter
  RKD_data_DiagnosisFilter <- RKD_data_DiseaseFilter[which(RKD_data_DiseaseFilter$Diagnosis.confidence == "Definite"),]
  
  ###### Small vessel immune filter
  
  RKD_data_SmallvesselImmuneFilter <- RKD_data_DiagnosisFilter[which(RKD_data_DiagnosisFilter$Small.vessel.vasculitis..ANCA.associated. != ""),]
  
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
  
  
  Filter_RKD_data <- RKD_data_VariableVesselFilter
  
  
  
  return(Filter_RKD_data)
}