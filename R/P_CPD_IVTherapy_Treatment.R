#' @title CPD IV Therapy treatment
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD IS Imputation for Treatment On/Off
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param IV_Therapy Data from IVTherapy from \code{\link{CPD_IVTherapy_interval}} function
#' @param merged_data Data from the merge of encounter and General characteristics in the \code{\link{Merge_Encounter_initial}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @export

CPD_IVTherapy_Treatment= function(IV_Therapy, merged_data, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(merged_data))
  stopifnot("Your argument need to be a data frame"=is.data.frame(IV_Therapy))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  IV_Therapy_filter <- IV_Therapy[which(IV_Therapy$IV.therapy == "Methylprednisolone - UATC/D07AA01" 
                                        | IV_Therapy$IV.therapy == "Rituximab - UATC/L01XC02 -- Mabthera" 
                                        | IV_Therapy$IV.therapy == "Cyclophosphamide Injectable Solution - UATC/ L01AA01"
                                        | IV_Therapy$IV.therapy == "Rituximab - UATC/L01XC02 -- Ruxience"
                                        | IV_Therapy$IV.therapy == "Rituximab - UATC/L01XC02 -- Truxima")
                                  , c("RKD.ID", "Date.of.IV.therapy", "IV.therapy")]
  
  Methyl <- IV_Therapy_filter [which(IV_Therapy_filter$IV.therapy == "Methylprednisolone - UATC/D07AA01"), ]
  Cyclo <- IV_Therapy_filter [which(IV_Therapy_filter$IV.therapy == "Cyclophosphamide Injectable Solution - UATC/ L01AA01"), ]
  Ritu <- IV_Therapy_filter [which(IV_Therapy_filter$IV.therapy == "Rituximab - UATC/L01XC02 -- Mabthera"| IV_Therapy_filter$IV.therapy == "Rituximab - UATC/L01XC02 -- Ruxience" | IV_Therapy_filter$IV.therapy == "Rituximab - UATC/L01XC02 -- Truxima") , ]
  
  Methyl$Start.date.of.date.range <-
    as.Date(Methyl$Date.of.IV.therapy)
  
  Methyl$End.date.of.date.range <-
    as.Date(Methyl$Date.of.IV.therapy) + 30
  
  
  merged_frame_methyl <-fuzzy_inner_join(
    merged_data[, c("RKD.ID", "Date.Of.Visit")], Methyl,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Start.date.of.date.range",
      "Date.Of.Visit" = "End.date.of.date.range"
    ),
    match_fun = list(`==`, `>`, `<=`)
  ) %>%
    select(everything())
  
  
  rownames(merged_frame_methyl) <- NULL
  
  merged_frame_methyl$Encounter=paste(merged_frame_methyl$RKD.ID.x,merged_frame_methyl$Date.Of.Visit,sep="_")
  
  merged_frame_methyl1=merged_frame_methyl[!duplicated(merged_frame_methyl$Encounter),]
  
  Cyclo$Start.date.of.date.range <-
    as.Date(Cyclo$Date.of.IV.therapy)
  
  Cyclo$End.date.of.date.range <-
    as.Date(Cyclo$Date.of.IV.therapy) + 90
  
  merged_frame_Cyclo <-fuzzy_inner_join(
    merged_data[, c("RKD.ID", "Date.Of.Visit")], Cyclo,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Start.date.of.date.range",
      "Date.Of.Visit" = "End.date.of.date.range"
    ),
    match_fun = list(`==`, `>`, `<=`)
  ) %>%
    select(everything())
  
  
  rownames(merged_frame_Cyclo) <- NULL
  
  merged_frame_Cyclo$Encounter=paste(merged_frame_Cyclo$RKD.ID.x,merged_frame_Cyclo$Date.Of.Visit,sep="_")
  
  merged_frame_Cyclo1=merged_frame_Cyclo[!duplicated(merged_frame_Cyclo$Encounter),]
  
  
  Ritu$Start.date.of.date.range <-
    as.Date(Ritu$Date.of.IV.therapy)
  
  Ritu$End.date.of.date.range <-
    as.Date(Ritu$Date.of.IV.therapy) + 180
  
  merged_frame_Ritu <-fuzzy_inner_join(
    merged_data[, c("RKD.ID", "Date.Of.Visit")], Ritu,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Start.date.of.date.range",
      "Date.Of.Visit" = "End.date.of.date.range"
    ),
    match_fun = list(`==`, `>`, `<=`)
  ) %>%
    select(everything())
  rownames(merged_frame_Ritu) <- NULL
  merged_frame_Ritu$Encounter=paste(merged_frame_Ritu$RKD.ID.x,merged_frame_Ritu$Date.Of.Visit,sep="_")
  
  merged_frame_Ritu1=merged_frame_Ritu[!duplicated(merged_frame_Ritu$Encounter),]
  
  
  
  merged_frame_Cyclo_Ritu <- merge(merged_frame_Ritu1[, c("RKD.ID.x", "Date.Of.Visit", "IV.therapy")], merged_frame_Cyclo1[, c("RKD.ID.x", "Date.Of.Visit", "IV.therapy")], by=c("RKD.ID.x", "Date.Of.Visit"), all=T )
  merged_frame_all <- merge(merged_frame_Cyclo_Ritu, merged_frame_methyl1[, c("RKD.ID.x", "Date.Of.Visit", "IV.therapy")], by=c("RKD.ID.x", "Date.Of.Visit"), all=T )
  

  
  merged_frame_all <- merged_frame_all %>% rowwise %>%
    mutate(IVtherapy = list(c(IV.therapy.x, IV.therapy.y, IV.therapy)))%>%
    ungroup
  merged_frame_unique <- merged_frame_all[!duplicated(merged_frame_all[,c(1:2,6)]), ]
  
  colnames(merged_frame_unique)[1] = "RKD.ID"
  
  return(merged_frame_unique)
  
}