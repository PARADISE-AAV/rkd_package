#' @title Paradise_Encounter
#' @author Yagmur Dogay/Matthieu Coq
#' Version: 1.0
#' Date: 03-May-23
#' Objective: Selection of the Encounter that meet the Paradise criteria
#' @param RKD_data RKD data from Demographic Filter RKD Data
#' @return The data with the a variable that tell you if the Encounter match the paradise criteria 
#' @details The criteria to be a Paradise encounter are 1/ need to be in Remission, 2/ need to in remission >6 month 3/ Have more than 1 year follow up
#' @import lubridate
#' @import dplyr
#' @export
Paradise_Encounter <- function(RKD_data) {
  interval_frame <- RKD_data %>%
    dplyr::filter(Disease.activity.since.last.return == 'Remission',
                  Interval.from.diagnosis..months. > 6,
                  !is.na(RKD.ID)) %>%
  dplyr::mutate(last_encounter = dplyr::case_when(
    Status == 'Alive' ~ Date.Of.Visit,
    Status == 'Dead' ~ Date.of.event,
    Status == 'Lost to follow-up' ~ Date.of..opt.out..or..Lost.to.follow.up.
  )) %>%
    dplyr::group_by(RKD.ID) %>%
    dplyr::mutate(Date_Last_Encounter = pmax(last_encounter, NA, na.rm = TRUE),
                  Interval_Last_Encounter_Months = lubridate::interval(Date.Of.Visit, Date_Last_Encounter) %/%
                    lubridate::days(1) / (365 / 12)) %>%
    dplyr::filter(Interval_Last_Encounter_Months > 12) %>%
    dplyr::select(RKD.ID, Interval_Last_Encounter_Months, Date_Last_Encounter, last_encounter, Status, Date.Of.Visit) %>%
    dplyr::mutate(Paradise.Encounters = 1L) %>%
    dplyr::ungroup()

  dplyr::left_join(RKD_data, interval_frame, by = c('RKD.ID', 'Date.Of.Visit'))
}

