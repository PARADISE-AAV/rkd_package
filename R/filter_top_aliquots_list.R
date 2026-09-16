#' @title filter_top_aliquots_list
#' @author Yagmur Dogay
#' Filter top aliquots for selected datasets
#'
#' This function selects the top N aliquots with the highest amount for each ID
#' within selected datasets from a list. The results are returned as a named list
#' of filtered data frames.
#'
#' @param data_list A named list of data frames to process.
#' @param datasets Character vector of dataset names to filter from `data_list`.
#' @param id_col Character vector specifying the column(s) used for grouping (e.g., patient ID).
#' @param aliquot_col Character string indicating the aliquot number column.
#'   This column will be converted to integer if present.
#' @param amount_col Character string indicating the column used to rank aliquots
#'   (highest values are kept).
#' @param top_n Integer specifying the number of top aliquots to keep per ID.
#'   Default is 4.
#'
#' @return A named list of filtered data frames, one for each processed dataset.
#'
#' @import dplyr







# Function to filter top aliquots for selected datasets from a list
filter_top_aliquots_list <- function(data_list, datasets, id_col, aliquot_col, amount_col, top_n = 4) {

  
  filtered_list <- list()
  
  for (ds in datasets) {
    

    if (!ds %in% names(data_list)) {
      warning(paste0("Dataset not found in data_list: ", ds))
      next
    }
    
    df <- data_list[[ds]]
    

    if (aliquot_col %in% names(df)) {
      df[[aliquot_col]] <- suppressWarnings(as.integer(df[[aliquot_col]]))
    } else {
      warning(paste0("Column not found in ", ds, ": ", aliquot_col))
    }
    

    filtered_df <- df %>%
      group_by(across(all_of(id_col))) %>%
      arrange(desc(.data[[amount_col]]), .by_group = TRUE) %>%
      slice_head(n = top_n) %>%
      ungroup()
    
    filtered_list[[ds]] <- filtered_df
  }
  
  return(filtered_list)
}



