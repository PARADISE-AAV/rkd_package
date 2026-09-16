#' @title Process All FW
#' @author Yagmur Dogay
#' @description
#' Process FW data and optionally combine with Biobank data
#'
#' This function processes preloaded FW datasets, converts date columns,
#' ensures unique \code{Unique.Aliquot.ID} values, and optionally combines
#' FW data with matching Biobank datasets.
#'
#' If Biobank data are provided for a dataset:
#' - FW data alone is stored with suffix \code{_1}
#' - Combined FW + Biobank data is stored with suffix \code{_2}
#' @import dplyr
#' @import tidyr
#' @import writexl
#' @param fw_list A named list of FW data frames (already loaded).
#' @param biobank_list Optional named list of Paradise_Biobank data frames with matching names.
#'
#' @return A named list of processed data frames.
#' @export







process_all_fw <- function(fw_list, biobank_list = list()) {
  
  prefixes <- names(fw_list)
  
  process_one <- function(prefix) {
    
    # FW verisini dışarıdan al
    df <- fw_list[[prefix]]
    
    # Date conversion
    if ("Date.of.encounter" %in% colnames(df)) {
      df$Date.of.encounter <- as.Date(df$Date.of.encounter, format = "%d/%m/%Y")
    }
    
    output <- list()
    
    # Biobank varsa
    if (!is.null(biobank_list[[prefix]])) {
      
      biobank_data <- biobank_list[[prefix]]
      
      if ("Date.of.encounter" %in% colnames(biobank_data)) {
        biobank_data$Date.of.encounter <- as.Date(biobank_data$Date.of.encounter)
      }
      
      if ("Freezer.Section" %in% colnames(biobank_data)) {
        biobank_data$Freezer.Section <- as.character(biobank_data$Freezer.Section)
      }
      
      # ---- FW only (_1)
      name_1 <- paste0(gsub("_$", "", prefix), "_1")
      df_1 <- make_unique_ids(df)
      output[[name_1]] <- df_1
      
      # ---- FW + Biobank (_2)
      df_2 <- bind_rows(df, biobank_data)
      df_2 <- make_unique_ids(df_2)
      name_2 <- paste0(gsub("_$", "", prefix), "_2")
      output[[name_2]] <- df_2
      
    } else {
      # Biobank yoksa
      name <- gsub("_$", "", prefix)
      df <- make_unique_ids(df)
      output[[name]] <- df
    }
    
    return(output)
  }
  
  # Duplicate ID düzeltme fonksiyonu (tekrar eden kodu dışarı aldık)
  make_unique_ids <- function(data) {
    if ("Unique.Aliquot.ID" %in% colnames(data)) {
      data <- data %>%
        group_by(Unique.Aliquot.ID, Main.Study.ID) %>%
        mutate(n = n(),
               Unique.Aliquot.ID = if_else(
                 n > 1,
                 paste0(Unique.Aliquot.ID, "_", row_number()),
                 as.character(Unique.Aliquot.ID)
               )) %>%
        ungroup() %>%
        select(-n)
    }
    return(data)
  }
  
  results <- lapply(prefixes, process_one)
  out_list <- do.call(c, results)
  
  return(out_list)
}



