
#' @title merge_fw_with_clinicaldata
#' @author Yagmur Dogay
#' Merge FW aliquot data with main dataset
#'
#' Process clinical data with an aliquot dataset
#' @import dplyr
#' @import tidyr
#' @description
#' This function merges an aliquot dataset (other_data) into the main dataset (main_data) by RKD.ID and Date.Of.Visit.
#' It also reshapes the aliquot dataset into wide format with dynamic column names.
#'
#' @param main_data clinical data
#' @param other_data A data.frame or tibble representing the specific aliquot dataset to merge (e.g., DNA, RNA, Serum).
#' @param other_data_name A string representing the prefix name to use for new columns (e.g., "DNA_RIV").
#'
#' @return A data.frame or tibble where main_data is augmented with the reshaped aliquot columns from other_data.





###Function 5 combine fw with riv data for each aliquot create different column


merge_fw_with_clinicaldata <- function(main_data, other_data, other_data_name) {
  library(dplyr)
  library(tidyr)
  
  # Step 1: Calculate the maximum number of aliquots per encounter
  max_aliquots <- other_data %>%
    group_by(RKD.ID, Date.Of.Visit) %>%
    summarise(max_aliquots = n_distinct(Unique.Aliquot.ID), .groups = "drop") %>%
    pull(max_aliquots) %>%
    max()
  
  # Step 2: Add indicator column to main_data with dynamic other_data_name
  main_data <- main_data %>%
    mutate(!!sym(other_data_name) := ifelse(
      paste(RKD.ID, Date.Of.Visit) %in% paste(other_data$RKD.ID, other_data$Date.Of.Visit), 1, 0
    ))
  
  # Step 3: Transform other_data to long format and add dynamic column names
  other_data_long <- other_data %>%
    group_by(RKD.ID, Date.Of.Visit) %>%
    mutate(aliquot_order = row_number()) %>%
    ungroup() %>%
    pivot_longer(
      cols = c(
        Main.Study.ID, Date.of.encounter, Unique.Aliquot.ID, Centrifuge.setting, RNA..ng.ul., 
        X260.280, Blood.Spun.at, Cell.number, Current.Amount, DNA.Concentration, Freezer.Name, 
        Freezer.Section, Initial.Amount, Plasma.Spun.Twice, Position.1, Position.2, Position.3, 
        Position.4, Processor, RNA.Concentration..ng.ul., Thaws, RNA.Integrity.No., 
        Protease.Inhib.added, Blood.Spun.at.1, Time.Frozen, Time.Collected, Sample.Type, Specimen.type,
        Start.date.of.date.range, End.date.of.date.range, Site
      ),
      names_to = "measurement",
      values_to = "value",
      values_transform = list(value = as.character) # Ensure consistent type
    ) %>%
    mutate(column_name = paste0(other_data_name, "_unique_aliquots_", aliquot_order, "_", measurement)) %>%
    select(RKD.ID, Date.Of.Visit, column_name, value)
  
  # Pivot to wide format
  other_data_wide <- other_data_long %>%
    pivot_wider(names_from = column_name, values_from = value)
  
  # Step 4: Fill missing columns based on max_aliquots
  aliquot_columns <- paste0(other_data_name, "_unique_aliquots_", 1:max_aliquots)
  for (col in aliquot_columns) {
    for (suffix in c("Main.Study.ID", "Date.of.encounter", "Unique.Aliquot.ID", "Centrifuge.setting", 
                     "RNA..ng.ul.", "X260.280", "Blood.Spun.at", "Cell.number", "Current.Amount", 
                     "DNA.Concentration", "Freezer.Name", "Freezer.Section", "Initial.Amount", 
                     "Plasma.Spun.Twice", "Position.1", "Position.2", "Position.3", "Position.4", 
                     "Processor", "RNA.Concentration..ng.ul.", "Thaws", "RNA.Integrity.No.", 
                     "Protease.Inhib.added", "Blood.Spun.at.1", "Time.Frozen", "Time.Collected", 
                     "Sample.Type", "Specimen.type","Start.date.of.date.range", "End.date.of.date.range", "Site","")) {
      col_name <- paste0(col, ifelse(suffix == "", "", paste0("_", suffix)))
      if (!col_name %in% names(other_data_wide)) {
        other_data_wide[[col_name]] <- NA
      }
    }
  }
  
  # Step 5: Merge main_data and other_data_wide
  final_data <- main_data %>%
    left_join(other_data_wide, by = c("RKD.ID" = "RKD.ID", "Date.Of.Visit" = "Date.Of.Visit"))
  
  return(final_data)
}


#


