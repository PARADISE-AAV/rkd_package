#' @title Paradise_Encounter
#' @author Yagmur Dogay/Matthieu Coq
#' 
#' @description
#' Selection of the Encounter that meet the Paradise criteria
#' 
#' Version: 1.0
#' 
#' Date: 03-May-23
#'  
#' @param RKD_data RIV data from \code{\link{CPDRelapse}}
#' @param months_after_diagnosis Exclude encounters of only recently diagnosed patients (default: 6 months)
#' @return The data with the a variable that tell you if the Encounter match the paradise criteria 
#' @details The criteria to be a Paradise encounter are 
#' * 1/ need to be in Remission 
#' * 2/ need to in remission >6 month 
#' 
#' The code need to launch after \code{\link{CPDRelapse}} as the Remission is define by "No Relapse"
#' 
#' @import lubridate
#' @import dplyr
#' @export
Paradise_Encounter <- function(RKD_data, months_after_diagnosis = 6) {
  max_if_any <- function(x) {
    if (all(is.na(x)))
      return(NA)
    max(x, na.rm = TRUE)
  }
  
  interval_frame <- RKD_data %>%
    dplyr::filter(CPD_relapse == 'No Relapse',
                  Interval.from.diagnosis..months. > months_after_diagnosis,
                  !is.na(RKD.ID)) %>%
  dplyr::mutate(last_encounter = dplyr::case_when(
    Status == 'Alive' ~ Date.Of.Visit,
    Status == 'Dead' ~ Date.of.event,
    Status == 'Lost to follow-up' ~ Date.of..opt.out..or..Lost.to.follow.up.
  )) %>%
    dplyr::group_by(RKD.ID) %>%
    dplyr::mutate(Date_Last_Follow_up = max_if_any(last_encounter),
                  Interval_Last_Encounter_Months = lubridate::interval(Date.Of.Visit, Date_Last_Follow_up) %/%
                    lubridate::days(1) / (365 / 12)) %>%
    dplyr::select(RKD.ID, Interval_Last_Encounter_Months, Date_Last_Follow_up, last_encounter, Status, Date.Of.Visit) %>%
    dplyr::mutate(Paradise.Encounters = 1L) %>%
    dplyr::ungroup()

  dplyr::left_join(RKD_data, interval_frame, by = c('RKD.ID', 'Date.Of.Visit'))
}

