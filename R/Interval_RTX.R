#' @title Interval_RTX
#' @author Matthieu COQ
#'
#' @description The goal of this function is to calculate the interval from the last dose of Rituximab
#' 
#' Version: 1.0
#' 
#' Date: 15-Aug-23
#'
#' @param merge_data Data from the merge of encounter and General characteristics
#' @param IV_Therapy IV Therapy frame
#' @details
#' 
#' This function is done to compile the interval from the last dose of Rituximab
#' 
#' 
#' @import lubridate
#' @import dplyr
#' @export


CPD_Interval_RTX = function (merge_data, IV_Therapy){
  
  Ritu <- IV_Therapy [which(IVTherapy$IV.therapy == "Rituximab - UATC/L01XC02 -- Mabthera"| IVTherapy$IV.therapy == "Rituximab - UATC/L01XC02 -- Ruxience" | IVTherapy$IV.therapy == "Rituximab - UATC/L01XC02 -- Truxima") , ]
  Ritu=Ritu[order(Ritu$RKD.ID,Ritu$Date.of.IV.therapy),]
  colnames(Classify_rkd_All)[2]="Date.Of.Visit"
  merged_frame_Ritu <-fuzzy_left_join(
    merge_data[, c("RKD.ID", "Date.Of.Visit")], Ritu,
    by = c(
      "RKD.ID" = "RKD.ID",
      "Date.Of.Visit" = "Date.of.IV.therapy"
      
    ),
    match_fun = list(`==`, `>`)
  ) %>%
    select(everything())
  rownames(merged_frame_Ritu) <- NULL
  
  merged_frame_Ritu$Encounter=paste(merged_frame_Ritu$RKD.ID.x, merged_frame_Ritu$Date.Of.Visit)
  
  merged_frame_Ritu=merged_frame_Ritu[order(merged_frame_Ritu$Encounter),]
  merged_frame_Ritu_1=merged_frame_Ritu[duplicated(merged_frame_Ritu$Encounter, fromLast = TRUE)==FALSE,]
  
  
  merged_frame_Ritu_1$Interval_from_RTX = as.numeric(merged_frame_Ritu_1$Date.Of.Visit - merged_frame_Ritu_1$Date.of.IV.therapy)
  colnames(merged_frame_Ritu_1)[1]="RKD.ID"
  n=length(as.factor(merged_frame_Ritu_1$RKD.ID))
  merged_frame_Ritu_2=NULL
  for(i in 1:n){
    dat=merged_frame_Ritu_1[which(merged_frame_Ritu_1$RKD.ID==levels(as.factor(merged_frame_Ritu_1$RKD.ID))[i]),]
    if(length(table(is.na(dat$RKD.ID.y)))==2){
      dat$Interval_from_RTX[which(is.na(dat$Interval_from_RTX)==TRUE)]="preRTX"
    }
    merged_frame_Ritu_2=rbind(merged_frame_Ritu_2,dat)
  }
  
  merge_data_2=merge(merge_dataClassify_rkd_All, merged_frame_Ritu_2[, c("RKD.ID", "Date.Of.Visit", "Date.of.IV.therapy", "Interval_from_RTX")], by=c("RKD.ID", "Date.Of.Visit"))
  
  return(merge_data_2)
  
}
  