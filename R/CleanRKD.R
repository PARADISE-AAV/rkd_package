#' @title Clean rare kidney disease data
#' @author Matthieu COQ
#'
#' @description The objective is to clean the RKD data and send the problematic data to the RKD person
#' 
#' Version: 1.0
#' 
#' Date: 24-Jan-23
#'
#' @param rkd_data RKD data from \code{\link{load_rkd}} function
#' @param output_path folder where the Redcap data will be saved
#' @return The Redcap data cleaned in your folder and in an object
#' 
#' The function change the Date variable with the format "%Y-%m-%d".
#' 
#' The function reduce the ethnicity to 6 group where different subgroup are regrouped.
#' 
#' The function clean some RKD.ID trouble to be sure that we have no problem when we do any merge with other dataset
#' 
#' The function select the variable present only for the Encounters data and the Initial data (demographics, diagnostics and all other exams performed at the moment of the diagnostics)and merge the Encounter dtaa and Initail data that the initial data are replicate for each Encounter
#' 
#' The function create the following variable Age of the Encounter and ANCA titration (Anti MPO or Anti PR3 or NA)
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @importFrom rlang .data
#' @export
clean_rkd <- function(rkd_data, output_path) {
  # Check arguments
  stopifnot("Your argument need to be a data frame"=is.data.frame(rkd_data))
  stopifnot("Your argument need to be a character"=is.character(output_path))

  # Check output directory
  if (!dir.exists(output_path)) {
    stop('Specified output folder does not exist')
  }

  # Check that you load a real file
  if (!ncol(rkd_data) || !nrow(rkd_data)) {
    stop("You supplied an empty file")
  }

  # Parse all date columns
  rkd_data <- rkd_parse_dates(rkd_data)

  # Collapse ethnicities
  rkd_data <- rkd_collapse_ethnicity(rkd_data)

  # Solve the problem of incorrect RKD IDs:
  # If the RKD ID contains a '-', replace it with Patient ID
  rkd_data <- rkd_fix_ids(rkd_data)

  # Combine the single-row initial records with the (multi-row) encounters
  rkd_data <- rkd_tidy_encounters(rkd_data)
  
  if(length(which(rkd_data$Date.of.diagnosis<rkd_data$Date.of.Birth))>0){
    warning("There is a problem with format of date")
  }
  
  a <- grep("Date",colnames(rkd_data))
  a <- a[-grep("known.unknown",colnames(rkd_data)[a])]
  for(i in a){
    if(length(which(is.na(rkd_data[,i]) == TRUE))>0){
      warning("You have a problem of format of date in your files. Be sure that the format is YYYY-MM-DD")
    }
    
    if(min(year(rkd_data[,i]),na.rm=T)<1900){
      warning("You have weird dates in your dataset can you check the format of the date of sample")
    }
  }

  # Add age variable
  rkd_data <- rkd_data %>%
    dplyr::mutate(Age_Encounters =
      lubridate::year(.data$Date.Of.Visit) - lubridate::year(.data$Date.of.Birth))

  # Anti MPO PR3
  rkd_data <- rkd_anti_mpo_pr3(rkd_data)

  output_filename <- file.path(
    output_path,
    paste0('Redcap_clinical_data_clean',
           Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
}

#' @import stringr
#' @import dplyr
#' @import lubridate
rkd_parse_dates <- function(data) {
  date_columns <- stringr::str_subset(colnames(data), "Date")
  date_columns <- stringr::str_subset(date_columns, "known.unknown", negate = TRUE)
  dplyr::mutate(
    data,
    dplyr::across(dplyr::all_of(date_columns), lubridate::as_date)
  )
}

#' @import forcats
#' @import stringr
#' @import dplyr
rkd_collapse_ethnicity <- function(data) {
  dplyr::mutate(
    data,
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
}

#' @import stringr
#' @import dplyr
rkd_fix_ids <- function(data) {
  data$RKD.ID[stringr::str_detect(data$RKD.ID, "-")] <- NA
  if (any(is.na(data$RKD.ID))) {
    warning(
      "Replacing ",
      sum(is.na(data$RKD.ID)),
      " incorrect RKD.IDs with Patient.ID"
    )
    data <- dplyr::mutate(data,
      RKD.ID = dplyr::coalesce(
        .data$RKD.ID,
        .data$Patient.ID
      )
      # as.integer(Patient.Id))
    )
  }
  return(data)
}

#' @import dplyr
rkd_tidy_encounters <- function(data) {
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

  # Merge together so we go from a sparse block-structure table
  # to a wide table, effectively with initial values carried forward
  # TODO: switch this to na.locf?
  rkd_encounter_filtered %>%
    dplyr::full_join(
      rkd_initial_filtered %>%
        dplyr::select(-.data$Patient.Id),
      by = "RKD.ID"
    )
}

#' @import dplyr
rkd_anti_mpo_pr3 <- function(data) {
  dplyr::mutate(data, AntiMPO_PR3 = dplyr::case_when(
    is.na(.data$Anti.MPO.level) & is.na(.data$Anti.PR3.level) ~ NA,
    is.na(.data$Anti.MPO.level) & !is.na(.data$Anti.PR3.level) ~ "PR3",
    !is.na(.data$Anti.MPO.level) & is.na(.data$Anti.PR3.level) ~ "MPO",
    .data$Anti.MPO.level > .data$Anti.PR3.level ~ "MPO",
    TRUE ~ "PR3"
  ))
}