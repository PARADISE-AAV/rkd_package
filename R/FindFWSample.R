#' @title Merge the RKD data and the Freezerwork data
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 22-Mar-23
#' Objective: The objective is to merge the RKD data filter and clean with the clean Freezer work data
#'
#'
#' @param FWdata FW data from \code{\link{CleanFW}} function
#' @param RKDdata RKD data from \code{\link{ClassifyRKDEncounter}} or earlier function
#' @param output_path folder where the merged data will be saved
#' @param interval The number of day in which the merge need to be done
#' @details The merge between the Freezerwork data and the RKD data allows us to find the sample associated to the RKD data.
#' 
#' The interval of day argument is here as we need flexibility in the date merging. The date of sample in the Freezerwork is the one when the sample arrive in the biobank and they can be a delay with the encounter date.
#' 
#' @return The Redcap data cleaned in your folder and in an object
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @export

FindFWSample=function(FWdata, RKDdata, output_path, interval){
  
  #Checking the argument
  if (is.data.frame(FWdata) == FALSE) {
    stop("The argument FWdata need to be a dataframe argument")
  }
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument FWdata need to be a dataframe argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  if (is.numeric(interval) == FALSE) {
    stop("The argument interval need to be a integer argument")
  }
  
  
  RKD_data <- RKDdata
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  FW_data <- FWdata
  if(ncol(FW_data)==0 | nrow(FW_data)==0){
    stop("You give an empty files")
  }
  
  FWdata$Start.date.of.date.range <-
    as.Date(FWdata$Date.of.encounter) - interval
  
  FWdata$End.date.of.date.range <-
    as.Date(FWdata$Date.of.encounter) + interval
  
  merged_frame <-fuzzy_inner_join(
    RKDdata, FWdata,
    by = c(
      "RKD.ID" = "Main.Study.ID",
      "Date.Of.Visit" = "Start.date.of.date.range",
      "Date.Of.Visit" = "End.date.of.date.range"
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) %>%
    select(everything())
  
  
  rownames(merged_frame) <- NULL
  
  files_test <-  list.dirs(output_path)
  if(identical(files_test, character(0)) == TRUE){
    stop("Your output folder don't exist")
  }
  write.csv(merged_frame, paste(output_path, "/Merged_FW_RKD_", "_version", packageVersion('rkdpipeline'), "_Date", Sys.Date() , ".csv", sep=""), row.names = F)
  return(merged_frame)
  
}