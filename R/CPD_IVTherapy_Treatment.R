#' @title CPD IV Therapy treatment
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD IS Imputation for Treatment On/Off
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param IVTherapy Data from Encounter from \code{\link{CPD_IVTherapy}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' 
#' @import lubridate
#' @export

CPD_IVTherapy_Treatment= function(IV_Therapy, output_dir){
  
  
  IV_Therapy_filter <- IV_Therapy[which(IV_Therapy$IV.therapy == "Methylprednisolone - UATC/D07AA01" 
                                        | IV_Therapy$IV.therapy == "Rituximab - UATC/L01XC02 -- Mabthera" 
                                        | IV_Therapy$IV.therapy == "Cyclophosphamide Injectable Solution - UATC/ L01AA01"
                                        | IV_Therapy$IV.therapy == "Rituximab - UATC/L01XC02 -- Ruxience"
                                        | IV_Therapy$IV.therapy == "Rituximab - UATC/L01XC02 -- Truxima")
                                  , c("RKD.ID", "Date.of.IV.therapy", "IV.therapy")]
  
  Methyl <- IV_Therapy_filter [which(IV_Therapy_filter$IV.therapy == "Methylprednisolone - UATC/D07AA01"), ]
  Cyclo <- IV_Therapy_filter [which(IV_Therapy_filter$IV.therapy == "Cyclophosphamide Injectable Solution - UATC/ L01AA01"), ]
  Ritu <- IV_Therapy_filter [which(IV_Therapy_filter$IV.therapy == "Rituximab - UATC/L01XC02 -- Mabthera" | IV_Therapy$IV.therapy == "Rituximab - UATC/L01XC02 -- Ruxience" | IV_Therapy$IV.therapy == "Rituximab - UATC/L01XC02 -- Truxima"), ]
  
  Methyl$Start.date.of.date.range <-
    as.Date(Methyl$Date.of.IV.therapy)
  
  Methyl$End.date.of.date.range <-
    as.Date(Methyl$Date.of.IV.therapy) + 30
  
  
  merged_frame_methyl <-fuzzy_inner_join(
    Inclusion_Criteria_rkd[, c("RKD.ID", "Date.Of.Visit")], Methyl,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Start.date.of.date.range",
      "Date.Of.Visit" = "End.date.of.date.range"
    ),
    match_fun = list(`==`, `>=`, `<`)
  ) %>%
    select(everything())
  
  
  rownames(merged_frame_methyl) <- NULL
  
  
  Cyclo$Start.date.of.date.range <-
    as.Date(Cyclo$Date.of.IV.therapy)
  
  Cyclo$End.date.of.date.range <-
    as.Date(Cyclo$Date.of.IV.therapy) + 90
  
  merged_frame_Cyclo <-fuzzy_inner_join(
    Inclusion_Criteria_rkd[, c("RKD.ID", "Date.Of.Visit")], Cyclo,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Start.date.of.date.range",
      "Date.Of.Visit" = "End.date.of.date.range"
    ),
    match_fun = list(`==`, `>=`, `<`)
  ) %>%
    select(everything())
  
  
  rownames(merged_frame_Cyclo) <- NULL
  
  
  Ritu$Start.date.of.date.range <-
    as.Date(Ritu$Date.of.IV.therapy)
  
  Ritu$End.date.of.date.range <-
    as.Date(Ritu$Date.of.IV.therapy) + 180
  
  merged_frame_Ritu <-fuzzy_inner_join(
    Inclusion_Criteria_rkd[, c("RKD.ID", "Date.Of.Visit")], Ritu,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Start.date.of.date.range",
      "Date.Of.Visit" = "End.date.of.date.range"
    ),
    match_fun = list(`==`, `>=`, `<`)
  ) %>%
    select(everything())
  
  
  rownames(merged_frame_Ritu) <- NULL
  
}