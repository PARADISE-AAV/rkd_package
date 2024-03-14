#' @title Clean rare kidney disease data
#' @author Matthieu COQ
#'
#' @description The objective is to clean the RIV data and send the problematic data to the RKD person
#' 
#' Version: 1.0
#' 
#' Date: 24-Jan-23
#'
#' @param rkd_data RIV data from \code{\link{load_riv}} function
#' @param output_path folder where the Redcap data will be saved
#' @details
#' The Redcap data cleaned in your folder and in an R object
#' 
#' The function change the Date variable with the format "%Y-%m-%d".
#' 
#' The function reduce the ethnicity to 6 group where different subgroup are regrouped.
#' 
#' The function clean some RKD.ID trouble to be sure that we have no problem when we do any merge with other dataset
#' 
#' The function select the variable present only for the Encounters data and the Initial data (demographics, diagnostics and all other exams performed at the moment of the diagnostics)and merge the Encounter data and Initial data that the initial data are replicate for each Encounter
#' 
#' The function create the following variable Age of the Encounter, ANCA titration (Anti MPO or Anti PR3 or NA), a Paradise.ID that represent the cohort name and the ID and an interval in days between each encounter and the diagnosis. 
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @importFrom rlang .data
#' @export
clean_riv <- function(rkd_data, output_path) {
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



  # Collapse ethnicities
  rkd_data <- rkd_collapse_ethnicity(rkd_data)

  # Solve the problem of incorrect RKD IDs:
  # If the RKD ID contains a '-', replace it with Patient ID
  rkd_data <- rkd_fix_ids(rkd_data)


  
  

  # Add age variable
  rkd_data <- rkd_data %>%
    dplyr::mutate(Age_Encounters =
      lubridate::year(.data$Date.Of.Visit) - lubridate::year(.data$Date.of.Birth))

  # Anti MPO PR3
  rkd_data <- rkd_anti_mpo_pr3(rkd_data)
  
  rkd_data$Paradise.ID <- paste("RIV", rkd_data$RKD.ID, sep = "")
  rkd_data2=rkd_data[,c("RKD.ID", "Paradise.ID",colnames(rkd_data)[-c(grep("RKD.ID",colnames(rkd_data)),grep("Paradise.ID",colnames(rkd_data)))])]
  
  rkd_data2$interval_from_diagnosis=as.numeric(rkd_data2$Date.Of.Visit-rkd_data2$Date.of.diagnosis)
  

  output_filename <- file.path(
    output_path,
    paste0('Redcap_clinical_data_clean', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data2, output_filename, row.names = FALSE)
  return(rkd_data2)
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



