#' @title Paradise_Encounter
#' @author Yagmur Dogay/Matthieu Coq
#' Version: 1.0
#' Date: 03-May-23
#' Objective: Selection of the Encounter that meet the Paradise criteria
#'


#' @param RKD_data RKD data from Demographic Filter RKD Data
#' @return The data with the a variable that tell you if the Encounter match the paradise criteria 
#' @details The criteria to be a Paradise encounter are 1/ need to be in Remission, 2/ need to in remission >6 month 3/ Have more than 1 year follow up
#' @export
#' 

Paradise_Encounter <- function(RKD_data,interval_from_diagnostics){
  library(DT)
  library(lubridate)
  remission_frame<- RKD_data[RKD_data$Disease.activity.since.last.return %in% c('Remission'), ]
  interval_more_then_6_month_frame <- remission_frame[remission_frame$Interval.from.diagnosis..months.>interval_from_diagnostics,]
  interval_more_then_6_month_frame <-interval_more_then_6_month_frame[!is.na(interval_more_then_6_month_frame$RKD.ID),]
  interval_more_then_6_month_frame$Date.Of.Visit=as.character(interval_more_then_6_month_frame$Date.Of.Visit)
  interval_more_then_6_month_frame$Date.of.event=as.character(interval_more_then_6_month_frame$Date.of.event)
  interval_more_then_6_month_frame$Date.of..opt.out..or..Lost.to.follow.up.=as.character(interval_more_then_6_month_frame$Date.of..opt.out..or..Lost.to.follow.up.)
  interval_frame <- interval_more_then_6_month_frame
  interval_frame$last_encounter <- NULL
  for (i in 1:nrow(interval_frame)) {
    #print(df$team[i])
    interval_frame$last_encounter[i] <-
      ifelse(
        interval_frame$Status[i] == "Alive",
        interval_frame$Date.Of.Visit[i],
        ifelse(
          interval_frame$Status[i] == "Dead",
          interval_frame$Date.of.event[i],
          ifelse(
            interval_frame$Status[i] == "Lost to follow-up",
            interval_frame$Date.of..opt.out..or..Lost.to.follow.up.[i]
          )
        )
      )
  }
  frame <- interval_frame
  myData = data.frame(matrix(nrow = 0, ncol = length(colnames(frame))+1))
  for (i in unique(frame$RKD.ID)){
    unique_frame <- frame[frame$RKD.ID==i,]
    max_no <- max(frame[frame$RKD.ID==i,c("last_encounter")], na.rm=TRUE)
    unique_frame$Date_Last_Encounter <- max_no
    # binding dataframes
    myData<-rbind(myData, unique_frame)
  }
  myData$Interval_Last_Encounter_Months <- round(interval(myData$Date.Of.Visit, myData$Date_Last_Encounter ) %/% days(1) / (365/12),2)
  
  more_12_months_frame <- myData[myData$Interval_Last_Encounter_Months>12,]
  more_12_months_frame <- more_12_months_frame[!is.na(more_12_months_frame$RKD.ID),]
  rownames(more_12_months_frame) <- NULL
  more_12_months_frame2=more_12_months_frame[,c("RKD.ID","Interval_Last_Encounter_Months","Date_Last_Encounter","last_encounter","Status","Date.Of.Visit")]
  more_12_months_frame2$Paradise.Encounters=rep(1,nrow(more_12_months_frame2))
  more_12_months_frame2$Date.Of.Visit=as.Date(more_12_months_frame2$Date.Of.Visit)
  
  Paradise_Encounter_data <- merge(RKD_data, more_12_months_frame2, by=c("RKD.ID", "Date.Of.Visit"), all.x = TRUE)
  
  return(Paradise_Encounter_data)
}

