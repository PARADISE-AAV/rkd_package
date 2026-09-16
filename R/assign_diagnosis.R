#' @title Diagnosis Type
#' @author Yagmur Dogay
#' @description 
#' This function processes RKD encounter data to derive a diagnosis variable and
#' create encounter-level criteria for further analysis. The diagnosis is derived
#' from disease indicator columns (DefiniteMPA, DefiniteGPA, DefiniteEGPA,
#' AntiGBM, DoublePositive, IgA, Cryoglobulinemic). If multiple indicators are
#' present, diagnoses are combined into a comma-separated string; if none are
#' present, the diagnosis is set to "Unknown".
#'
#' In addition, the function formats visit dates, creates a unique encounter
#' identifier (ID_date), and generates criteria variables identifying encounters
#' that are either marked as Paradise encounters or Diagnosis encounters: occur within ±14 days of the
#' diagnosis date.
#'
#' @param RKDdata Data frame with the RKD data
#' @import dplyr
#' @import tidyr
#' @import writexl
#' @return The input data frame with additional variables:
#' \itemize{
#'   \item \code{Diagnosis}
#'   \item \code{criteria_pre}
#'   \item \code{ID_date}
#'   \item \code{criteria_1_Paradise_Encounters}
#'   \item \code{criteria_1_Diagnosis_Encounters}
#'   \item \code{criteria_1}
#' @export



assign_diagnosis <- function(RKDdata) {
  
  # Step 1: Create Diagnosis column
  RKDdata <- RKDdata %>%
    rowwise() %>%
    mutate(
      Diagnosis = paste(
        c(
          if (DefiniteMPA == 1) "Definite_MPA",
          if (DefiniteGPA == 1) "Definite_GPA",
          if (DefiniteEGPA == 1) "Definite_EGPA",
          if (AntiGBM == 1) "Anti_GBM",
          if (DoublePositive == 1) "Double_positive",
          if (IgA == 1) "IgA",
          if (Cryoglobulinemic == 1) "Cryoglobulinemic"
        ),
        collapse = ", "
      )
    ) %>%
    mutate(Diagnosis = ifelse(Diagnosis == "", "Unknown", Diagnosis)) %>%
    ungroup()
  
  # Step 2: Add criteria_pre column
  RKDdata$criteria_pre <- 1
  # Step 3: Ensure Date.Of.Visit is Date type
  RKDdata$Date.Of.Visit <- as.Date(RKDdata$Date.Of.Visit)
  # Step 4: Create ID_date
  RKDdata$ID_date <- paste(RKDdata$RKD.ID, RKDdata$Date.Of.Visit, sep = "_")
  # Step 5: Criteria 1 - Paradise Encounters
  RKDdata$criteria_1_Paradise_Encounters <- ifelse(RKDdata$Paradise.Encounters == 1, 1, 0)
  RKDdata$criteria_1_Paradise_Encounters[is.na(RKDdata$criteria_1_Paradise_Encounters)] <- 0
  # Step 6: Criteria 1 - Diagnosis Encounters
  RKDdata$criteria_1_Diagnosis_Encounters <- ifelse(
    RKDdata$encounter_interval_from_diagnosis >= -14 & RKDdata$encounter_interval_from_diagnosis <= 14, 1, 0
  )
  RKDdata$criteria_1_Diagnosis_Encounters[is.na(RKDdata$criteria_1_Diagnosis_Encounters)] <- 0
  # Step 7: Criteria 1 combined (Paradise OR Diagnosis)
  criteria_1_dataset <- subset(RKDdata, Paradise.Encounters == 1 |
                                 (encounter_interval_from_diagnosis >= -14 & encounter_interval_from_diagnosis <= 14))
  
  RKDdata$criteria_1 <- ifelse(
    paste(RKDdata$RKD.ID, RKDdata$Date.Of.Visit) %in% paste(criteria_1_dataset$RKD.ID, criteria_1_dataset$Date.Of.Visit),
    1, 0
  )
  
  return(RKDdata)
}



