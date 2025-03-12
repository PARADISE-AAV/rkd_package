#' @title Split RIV data in 7 frame
#' @author Matthieu COQ
#'
#' @description The objective is to split the RIV RedCap export in different frame
#' 
#' Version: 1.0
#' 
#' Date: 13-Mar-23
#' @param RIVdata  {"name": "rkd_data","desc": "RIV data from \code{\link{load_riv}} function","options": (),"type": "file"}
#' @param output_dir  {"name": "output_dir","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#'
#' @details
#' Need to be added
#' @import dplyr
#' @export

SplitRIV <- function(RIVdata, output_dir){
  stopifnot("Your argument need to be a data frame"=is.data.frame(RIVdata))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  # Check that you load a real file
  if (!ncol(RIVdata) || !nrow(RIVdata)) {
    stop("You supplied an empty file")
  }
  
  data <- RIVdata
  rkd_encounters <- dplyr::filter(
    data,
    .data$Repeat.Instrument == "Encounters"
  )
  rkd_initial <- dplyr::filter(
    data,
    .data$Repeat.Instrument == "",
    .data$Type.of.Patient != ""
  )
  # Select the variables of encounters
  id_columns <- c(
    "RKD.ID", "Repeat.Instrument",
    "Repeat.Instance", "Patient.Id"
  )
  # TODO: it is not necessary to store 'empty_encounters' or 'empty_initials'
  # because we can just remove these columns inline using dplyr::where(!.)
  
  # Get rid of empty / all-missing variables
  rkd_encounter_filtered <- rkd_encounters %>%
    dplyr::select(!dplyr::where(~ all(is.na(.x)))) %>%
    dplyr::select(!dplyr::where(~ is.character(.x) && all(.x == "")))
  rkd_initial_filtered <- rkd_initial %>%
    dplyr::select(!dplyr::where(~ all(is.na(.x)))) %>%
    dplyr::select(!dplyr::where(~ is.character(.x) && all(.x == "")))
  
  rkd_IVTherapy <- dplyr::filter(
    data,
    .data$Repeat.Instrument == "Treatment - Intermittent pulse administrations"
  )
  rkd_IVTherapy_filtered <- rkd_IVTherapy %>%
    dplyr::select(!dplyr::where(~ all(is.na(.x)))) %>%
    dplyr::select(!dplyr::where(~ is.character(.x) && all(.x == "")))
  
  rkd_Continuous_medication <- dplyr::filter(
    data,
    .data$Repeat.Instrument == "Treatment - Continuing medications"
  )
  rkd_Continuous_medication_filtered <- rkd_Continuous_medication %>%
    dplyr::select(!dplyr::where(~ all(is.na(.x)))) %>%
    dplyr::select(!dplyr::where(~ is.character(.x) && all(.x == "")))
  
  rkd_Biopsy <- dplyr::filter(
    data,
    .data$Repeat.Instrument == "Biopsies"
  )
  rkd_Biopsy_filtered <- rkd_Biopsy %>%
    dplyr::select(!dplyr::where(~ all(is.na(.x)))) %>%
    dplyr::select(!dplyr::where(~ is.character(.x) && all(.x == "")))
  
  rkd_Transplantation <- dplyr::filter(
    data,
    .data$Repeat.Instrument == "Renal transplantation"
  )
  rkd_Transplantation_filtered <- rkd_Transplantation %>%
    dplyr::select(!dplyr::where(~ all(is.na(.x)))) %>%
    dplyr::select(!dplyr::where(~ is.character(.x) && all(.x == "")))
  
  rkd_Complication <- dplyr::filter(
    data,
    .data$Repeat.Instrument == "Complications"
  )
  rkd_Complication_filtered <- rkd_Complication %>%
    dplyr::select(!dplyr::where(~ all(is.na(.x)))) %>%
    dplyr::select(!dplyr::where(~ is.character(.x) && all(.x == "")))
  
  write.csv(rkd_encounter_filtered, paste(output_dir,"/Redcap_Encounter frame", "_version", packageVersion('rivpipeline'), "_Date"
                                          , Sys.Date(), '.csv',sep=""), row.names = F)
  write.csv(rkd_initial_filtered, paste(output_dir,"/Redcap_General charcteristic frame", "_version", packageVersion('rivpipeline'), "_Date"
                                        , Sys.Date(), '.csv',sep=""), row.names = F)
  write.csv(rkd_IVTherapy_filtered, paste(output_dir,"/Redcap_IVTherapy frame", "_version", packageVersion('rivpipeline'), "_Date"
                                          , Sys.Date(), '.csv',sep=""), row.names = F)
  write.csv(rkd_Continuous_medication_filtered, paste(output_dir,"/Redcap_Continuous medication frame", "_version", packageVersion('rivpipeline'), "_Date"
                                                      , Sys.Date(), '.csv',sep=""), row.names = F)
  write.csv(rkd_Biopsy_filtered, paste(output_dir,"/Redcap_Biopsy frame", "_version", packageVersion('rivpipeline'), "_Date"
                                       , Sys.Date(), '.csv',sep=""), row.names = F)
  write.csv(rkd_Transplantation_filtered, paste(output_dir,"/Redcap_Renal Transplant frame", "_version", packageVersion('rivpipeline'), "_Date"
                                                , Sys.Date(), '.csv',sep=""), row.names = F)
  write.csv(rkd_Complication_filtered, paste(output_dir,"/Redcap_Complication frame", "_version", packageVersion('rivpipeline'), "_Date"
                                             , Sys.Date(), '.csv',sep=""), row.names = F)
  
  rkd_frame <- list(Encounter=rkd_encounter_filtered, Initial=rkd_initial_filtered, IVTherapy=rkd_IVTherapy_filtered, 
                    Medication=rkd_Continuous_medication_filtered, Biopsy=rkd_Biopsy_filtered, 
                    Transplant=rkd_Transplantation_filtered, Complication=rkd_Complication_filtered)
  
  return(rkd_frame)

}