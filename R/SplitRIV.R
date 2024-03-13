#' @title Clean rare kidney disease data
#' @author Matthieu COQ
#'
#' @description The objective is to split the RIV RedCap export in different frame
#' 
#' Version: 1.0
#' 
#' Date: 13-Mar-23
#'
#' @param rkd_data RIV data from \code{\link{load_riv}} function
#' @param output_path folder where the Redcap data will be saved
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
    .data$Repeat.Instrument == "Renal transplantations"
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
  
}