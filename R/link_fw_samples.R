#' @title link_fw_samples
#' @author Yagmur Dogay
#' Link FW datasets to a reference frame using FindFWSample
#'
#' This function links selected FW datasets to clinical data(`RKD_data`)
#' by calling `FindFWSample()`. For Serum, Plasma, and Urine, the `_2` datasets
#' are automatically used if present.
#'
#' @param samples Character vector of sample types to link (e.g., "DNA", "Serum").
#' @param fw_list Named list of processed FW datasets. Names should match the sample types.
#' @param RKD_data Clinical Data
#' @param temp_dir Optional character string for temporary files. Defaults to R's `tempdir()`.
#'
#' @return Named list of linked datasets. Each element corresponds to a sample type
#'   and contains the result of `FindFWSample`.




link_fw_samples <- function(samples, fw_list, RKD_data, temp_dir = tempdir()) {
  
  linked_list <- list()
  
  for (sample in samples) {

    if (sample %in% c("Serum", "Plasma", "Urine")) {
      df_name <- paste0(sample, "_2")
    } else {
      df_name <- sample
    }
    
    # Check existence
    if (!df_name %in% names(fw_list)) {
      warning(paste0("Dataset not found in fw_list: ", df_name))
      next
    }
    
    message(paste0("Processing FW dataset: ", df_name))
    
    fw_data <- fw_list[[df_name]]
    

    linked_result <- FindFWSample(fw_data, RKD_data, temp_dir, 3)
    

    linked_list[[df_name]] <- linked_result
  }
  
  return(linked_list)
}


