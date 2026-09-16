#' @title process_paradise_fw
#' @author Yagmur Dogay
#' Process Paradise Biobank FW data
#'
#' Creates a new column for each specimen type indicating whether
#' the sample comes from FW, Biobank, or both.
#'
#' @import dplyr
#' @import tidyr
#' @param data Data frame containing RIV datasets with unique aliquots
#'             columns (e.g., PLASMA_RIV_unique_aliquots_1_Specimen.type)
#' @return Data frame with added columns:
#'         PLASMA_RIV_ParadiseBiobank_FW, SERUM_RIV_ParadiseBiobank_FW, URINE_RIV_ParadiseBiobank_FW




process_paradise_fw <- function(data) {
  library(dplyr)
  
  # Drop "_2_" from Serum, Plasma, Urine column names
  names(data) <- gsub("(PLASMA|SERUM|URINE)_2_", "\\1_", names(data))
  
  # Define specimen sets
  specimen_sets <- list(
    PLASMA = c("Plasma", "ParadiseBiobank_Plasma"),
    SERUM  = c("Serum", "ParadiseBiobank_Serum"),
    URINE  = c("Urine-M", "ParadiseBiobank_Urine-M")
  )
  
  for (specimen in names(specimen_sets)) {
    specimen_prefix <- paste0(specimen, "_RIV")
    new_col <- paste0(specimen_prefix, "_ParadiseBiobank_FW")
    
    specimen_cols <- paste0(specimen_prefix, "_unique_aliquots_", 1:4, "_Specimen.type")
    
    # Check for missing columns
    missing_cols <- specimen_cols[!specimen_cols %in% names(data)]
    if (length(missing_cols) > 0) {
      warning(paste0("Missing specimen columns for ", specimen, ": ", paste(missing_cols, collapse = ", ")))
      next
    }
    
    # Create new column
    data[[new_col]] <- NA
    
    data[[new_col]][data[[specimen_prefix]] == 1] <- apply(
      data[data[[specimen_prefix]] == 1, specimen_cols],
      1,
      function(row) {
        values <- specimen_sets[[specimen]]
        if (all(values %in% row)) {
          return("1 and 2")
        } else if (values[2] %in% row) {
          return("2")
        } else if (values[1] %in% row) {
          return("1")
        } else {
          return(NA)
        }
      }
    )
  }
  
  return(data)
}

