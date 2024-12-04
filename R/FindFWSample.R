#' @title Merge the RIV data and the Freezerwork data
#' @author Matthieu COQ
#' @description
#' Objective: The objective is to merge the RIV data filter and clean with the clean Freezer work data
#'
#' Date: 22-Mar-23
#' 
#' Version: 1.0
#' 
#'
#' @param FWdata FW data from \code{\link{CleanFW}} function
#' @param RKDdata RKD data from \code{\link{ClassifyRIVEncounter}} or earlier function
#' @param output_path folder where the merged data will be saved
#' @param interval The number of day in which the merge need to be done
#' @details The merge between the Freezerwork data and the RIV data allows us to find the sample associated to the RKD data.
#' 
#' The interval of day argument is here as we need flexibility in the date merging. The date of sample in the Freezerwork is the one when the sample arrive in the biobank and they can be a delay with the encounter date.
#' 
#' @return to add
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
  
  merged_frame_no_duplicates <- merged_frame %>%
    group_by(Unique.Aliquot.ID) %>%
      # Step 1: Remove duplicates where Date.Of.Visit == Date.of.encounter, keeping only one row
    filter(!(duplicated(Date.Of.Visit) & Date.Of.Visit == Date.of.encounter)) %>%
      # Step 2: For remaining duplicates where Date.Of.Visit != Date.of.encounter, keep the row where Date.of.encounter is closest to Date.Of.Visit
      # If there is a tie (multiple closest rows), keep one randomly
    filter(if_else(n() > 1 & Date.Of.Visit != Date.of.encounter,
                    abs(Date.of.encounter - Date.Of.Visit) == min(abs(Date.of.encounter - Date.Of.Visit)),
                    TRUE)) %>%
    group_by(Unique.Aliquot.ID, Date.Of.Visit) %>%
      # Randomly select one row if there are still multiple rows with the same closest distance
    slice_sample(n = 1) %>%
    ungroup()
  
  
  
  files_test <-  list.dirs(output_path)
  if(identical(files_test, character(0)) == TRUE){
    stop("Your output folder don't exist")
  }
  write.csv(merged_frame_no_duplicates, paste(output_path, "/Merged_FW_RKD_", "_version", packageVersion('rivpipeline'), "_Date", Sys.Date() , ".csv", sep=""), row.names = F)
  return(merged_frame_no_duplicates)
  
}