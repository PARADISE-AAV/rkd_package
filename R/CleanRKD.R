#' @title Clean rare kidney disease data
#' @author Matthieu COQ
#'
#' The objective is to clean the RKD data and send the problematic data to the RKD person
#' Version: 1.0
#' Date: 24-Jan-23
#'
#' @param rkd_data RKD data from \code{\link{load_rkd}} function
#' @param ouput_path folder where the Redcap data will be saved
#' @return The Redcap data cleaned in your folder and in an object
#' The function change the Date variable with the format "%Y-%m-%d".
#' The function reduce the ethnicity to 6 group where different subgroup are regrouped.
#' The function clean some RKD.ID trouble to be sure that we have no problem when we do any merge with other dataset
#' The function select the variable present only for the Encounters data and the Initial data (demographics, diagnostics and all other exams performed at the moment of the diagnostics)and merge the Encounter dtaa and Initail data that the initial data are replicate for each Encounter
#' The function create the following variable Age of the Encounter and ANCA titration (Anti MPO or Anti PR3 or NA)
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @export
clean_rkd <- function(rkd_data, output_path) {
  # Check arguments
  stopifnot(is.data.frame(rkd_data))
  stopifnot(is.character(output_path))

  # Check that you load a real file
  if (!ncol(rkd_data) || !nrow(rkd_data)) {
    stop("You supplied an empty file")
  }

  # Parse all date columns
  date_columns <- stringr::str_subset(colnames(rkd_data), "Date")
  date_columns <- stringr::str_subset(date_columns, "known.unknown", negate = TRUE)
  rkd_data <- dplyr::mutate(
    rkd_data,
    dplyr::across(dplyr::all_of(date_columns), lubridate::as_date)
  )

  # Collapse ethnicities
  rkd_data <- dplyr::mutate(
    rkd_data,
    dplyr::across(
      dplyr::starts_with("Ethnicity"),
      ~ forcats::fct_collapse(
        stringr::str_extract(.x, "^[A-Z]{0,2}"),
        White = "W",
        Asian = "A",
        Black = "B",
        `Mixed ethnicity` = "M",
        Other = "O",
        `Not Stated` = "NS"
      )
    )
  )

  # Solve the problem of incorrect RKD IDs
  # If the RKD ID contains a '-', replace it with Patient Id
  rkd_data$RKD.ID[stringr::str_detect(rkd_data$RKD.ID, "-")] <- NA
  if (any(is.na(rkd_data$RKD.ID))) {
    warning(
      "Replacing ",
      sum(is.na(rkd_data$RKD.ID)),
      " incorrect RKD.IDs with Patient.ID"
    )
    rkd_data <- dplyr::mutate(rkd_data,
      RKD.ID = dplyr::coalesce(
        RKD.ID,
        Patient.ID
      )
      # as.integer(Patient.Id))
    )
  }


  rkd_encounters <- dplyr::filter(
    rkd_data,
    Repeat.Instrument == "Encounters"
  )
  rkd_initial <- dplyr::filter(
    rkd_data,
    Repeat.Instrument == "",
    Type.of.Patient != ""
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
    dplyr::select(!dplyr::where(~ all(is.na(.x)) | all(!nchar(.x))))
  rkd_initial_filtered <- rkd_initial %>%
    dplyr::select(!dplyr::where(~ all(is.na(.x)) | all(!nchar(.x))))

  # Merge together so we go from a sparse block-structure table
  # to a wide table, effectively with initial values carried forward
  # TODO: switch this to na.locf?
  rkd_data_filtered <- rkd_encounter_filtered %>%
    dplyr::full_join(rkd_initial_filtered %>%
      dplyr::select(-Repeat.Instrument, -Repeat.Instance, -Patient.Id),
    by = "RKD.ID"
    )

  RKD_data_filter$Age_Encounters <- year(RKD_data_filter$Date.Of.Visit) - year(RKD_data_filter$Date.of.Birth)

  RKD_data_filter$AntiMPO_PR3 <- NA
  m <- nrow(RKD_data_filter)
  for (i in 1:m) {
    if (is.na(RKD_data_filter$Anti.MPO.level[i]) == TRUE & is.na(RKD_data_filter$Anti.PR3.level[i]) == TRUE) {
      RKD_data_filter$AntiMPO_PR3[i] <- NA
    } else {
      if (is.na(RKD_data_filter$Anti.MPO.level[i]) == TRUE & is.na(RKD_data_filter$Anti.PR3.level[i]) == FALSE) {
        RKD_data_filter$AntiMPO_PR3[i] <- "PR3"
      } else {
        if (is.na(RKD_data_filter$Anti.MPO.level[i]) == FALSE & is.na(RKD_data_filter$Anti.PR3.level[i]) == TRUE) {
          RKD_data_filter$AntiMPO_PR3[i] <- "MPO"
        } else {
          if (RKD_data_filter$Anti.MPO.level[i] > RKD_data_filter$Anti.PR3.level[i]) {
            RKD_data_filter$AntiMPO_PR3[i] <- "MPO"
          } else {
            RKD_data_filter$AntiMPO_PR3[i] <- "PR3"
          }
        }
      }
    }
  }

  Clean_RKD_data <- RKD_data_filter

  files_test <- list.dirs(output_path)
  if (identical(files_test, character(0)) == TRUE) {
    stop("Your output folder don't exist")
  }
  setwd(output_path)
  write.csv(Clean_RKD_data, paste("Redcap_clinical_data_clean", Sys.Date(), ".csv", sep = ""), row.names = F)
  return(Clean_RKD_data)
}
