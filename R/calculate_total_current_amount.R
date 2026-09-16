#' @title calculate_total_current_amount
#' @author Yagmur Dogay
#' Calculate total Current.Amount for a dataset
#'
#' @description
#' This function calculates the sum of all "Current.Amount" columns for each FW.
#' It looks for columns that start with the provided prefix (other_data_name) and end with "Current.Amount",
#' and sums them row-wise, creating a new column with the total amount.
#' @import dplyr
#' @import tidyr
#' @import writexl
#' @param processed_data clinical data with FW columns
#' @param other_data_name A string representing the prefix of the dataset (e.g., "DNA_RIV", "RNA_RIV").
#'
#' @return A data.frame or tibble with an additional column: <other_data_name>_total_Current.Amount
#'
#' @export



calculate_total_current_amount <- function(processed_data, other_data_name) {
  processed_data <- processed_data %>%
    rowwise() %>%
    mutate(!!paste0(other_data_name, "_total_Current.Amount") := sum(
      as.numeric(c_across(starts_with(paste0(other_data_name, "_unique_aliquots_")) & ends_with("Current.Amount"))),
      na.rm = TRUE
    )) %>%
    ungroup()
  
  return(processed_data)
}


