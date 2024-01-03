#' @title IVTherapy
#' @author Matthieu COQ
#' @description
#' Objective: The objective is to add the IV Therapy at each encounter
#'  
#' Date: 22-Nov-23
#' 
#' Version: 1.0
#'
#' @param RKD RKD data from \code{\link{load_rkd}} function
#' @param RKD_Treatment The RKD data after the treatment on/off function
#' @return The RKD data with treatment on/off updated  with IV Therapy knowledge
#' @details The IV Therapy function give what is the IV therapy for each encounter. 
#' 
#' For this a delay after a injection is given to say if the patient is still under therapy or not.
#'  * Methylprednisolone is 30 days
#'  * Rituximab is 180 days
#'  * Cyclophosphamide is 90 days
#'  
#'If a patient have a visit between the injection data and the injection data + delay, the patient is declare on treatment.
#' 
#' 
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @export 



IVTherapy <- function(RKD, RKD_Treatment){
  IV_Therapy <- RKD[which(RKD$Repeat.Instrument == "Treatment - Intermittent pulse administrations"), ]
  
  IV_Therapy_filter <- IV_Therapy[which(IV_Therapy$IV.therapy == "Methylprednisolone - UATC/D07AA01" 
                                        | IV_Therapy$IV.therapy == "Rituximab - UATC/L01XC02 -- Mabthera" 
                                        | IV_Therapy$IV.therapy == "Cyclophosphamide Injectable Solution - UATC/ L01AA01")
                                  , c("RKD.ID", "Date.of.IV.therapy", "IV.therapy")]
  
  Methyl <- IV_Therapy_filter [which(IV_Therapy_filter$IV.therapy == "Methylprednisolone - UATC/D07AA01"), ]
  Cyclo <- IV_Therapy_filter [which(IV_Therapy_filter$IV.therapy == "Cyclophosphamide Injectable Solution - UATC/ L01AA01"), ]
  Ritu <- IV_Therapy_filter [which(IV_Therapy_filter$IV.therapy == "Rituximab - UATC/L01XC02 -- Mabthera"), ]
  
  Methyl$Start.date.of.date.range <-
    as.Date(Methyl$Date.of.IV.therapy)
  
  Methyl$End.date.of.date.range <-
    as.Date(Methyl$Date.of.IV.therapy) + 30
  

  merged_frame_methyl <-fuzzy_inner_join(
    RKD_Treatment[, c("RKD.ID", "Date.Of.Visit")], Methyl,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Start.date.of.date.range",
      "Date.Of.Visit" = "End.date.of.date.range"
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) %>%
    select(everything())
  
  
  rownames(merged_frame_methyl) <- NULL
  
  
  Cyclo$Start.date.of.date.range <-
    as.Date(Cyclo$Date.of.IV.therapy)
  
  Cyclo$End.date.of.date.range <-
    as.Date(Cyclo$Date.of.IV.therapy) + 90
  
  merged_frame_Cyclo <-fuzzy_inner_join(
    RKD_Treatment[, c("RKD.ID", "Date.Of.Visit")], Cyclo,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Start.date.of.date.range",
      "Date.Of.Visit" = "End.date.of.date.range"
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) %>%
    select(everything())
  
  
  rownames(merged_frame_Cyclo) <- NULL
  
  
  Ritu$Start.date.of.date.range <-
    as.Date(Ritu$Date.of.IV.therapy)
  
  Ritu$End.date.of.date.range <-
    as.Date(Ritu$Date.of.IV.therapy) + 180
  
  merged_frame_Ritu <-fuzzy_inner_join(
    RKD_Treatment[, c("RKD.ID", "Date.Of.Visit")], Ritu,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Start.date.of.date.range",
      "Date.Of.Visit" = "End.date.of.date.range"
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) %>%
    select(everything())
  
  
  rownames(merged_frame_Ritu) <- NULL
  
  merged_frame <- rbind(merged_frame_Cyclo, merged_frame_methyl, merged_frame_Ritu)
  colnames(merged_frame)[1]="RKD.ID"
  merged_frame_unique <- merged_frame[!duplicated(merged_frame[,1:2]), ]
  
  rkd_IV_Therapy <- merge(RKD_Treatment, merged_frame_unique[,c(1,2,5)], by = c("RKD.ID", "Date.Of.Visit"), all.x = TRUE)
  
  n=nrow(rkd_IV_Therapy)
  for(i in 1:n){
    if(rkd_IV_Therapy$treatment.on.off[i] == "" & is.na(rkd_IV_Therapy$IV.therapy[i])==FALSE){
      rkd_IV_Therapy$treatment.on.off[i] = "On Treatment"
    }
  }
  
  return(rkd_IV_Therapy)
}

