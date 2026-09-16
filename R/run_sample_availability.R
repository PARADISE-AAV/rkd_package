#' @title run_sample_availability
#' @author Yagmur Dogay
#' @description
#' Runs the full Sample Availability pipeline end-to-end, mirroring
#' \code{Sample_Availability_Function.Rmd}:
#' \enumerate{
#'   \item Read the RIV/RKD clinical export and derive \code{Diagnosis} /
#'         \code{criteria_1} columns (\code{\link{assign_diagnosis}}).
#'   \item Read the four ParadiseBiobank files.
#'   \item Load the Freezerworks (FW) exports and combine each with its
#'         matching Biobank dataset (\code{\link{prepare_fw_datasets}}).
#'   \item Link every FW/Biobank dataset to the clinical data
#'         (\code{\link{link_fw_samples}}).
#'   \item Keep the top 4 aliquots per encounter
#'         (\code{\link{filter_top_aliquots_list}}).
#'   \item Merge each aliquot dataset into the clinical data and compute the
#'         total current amount per specimen type
#'         (\code{\link{merge_fw_with_clinicaldata}},
#'         \code{\link{calculate_total_current_amount}}).
#'   \item Flag whether each Serum/Plasma/Urine sample came from FW, Biobank,
#'         or both (\code{\link{process_paradise_fw}}).
#' }
#'
#' File paths are hardcoded to match the original analysis notebook. Update
#' the paths inside this function if the source files move.
#'
#' @param output_path Optional. If supplied, the final data frame is written
#'   to this path. The file extension determines the format: \code{".csv"}
#'   uses \code{write.csv()}, \code{".xlsx"} uses \code{writexl::write_xlsx()}.
#'   If \code{NULL} (default), nothing is written and only the data frame is
#'   returned.
#'
#' @return A data.frame: the final merged Sample Availability dataset.
#' @import dplyr
#' @import tidyr
#' @export
run_sample_availability <- function(output_path = NULL) {

  library(dplyr)
  library(tidyr)

  ## ---- Hardcoded paths (mirrors Sample_Availability_Function.Rmd) --------
  pipeline_file <- "C:/Users/DOGAYY/OneDrive - Trinity College Dublin/Yagmur Dogay/Yagmur_2024/Pipeline_Export_Files/November 2025/Redcap_clinical_data_with-classification_version0.0.3.310_Date2025-11-04_potential_additional_LTROT.csv"

  fw_path <- "C:/Users/DOGAYY/OneDrive - Trinity College Dublin/Freezerworks exports/Freezerworks Exports"

  file_path_mini_serum   <- "C:/Users/DOGAYY/OneDrive - Trinity College Dublin/Yagmur Dogay/Yagmur_2024/MiniBiobank/ParadiseBiobank/ParadiseBiobank_Serum_10032025.csv"
  file_path_mini_plasma  <- "C:/Users/DOGAYY/OneDrive - Trinity College Dublin/Yagmur Dogay/Yagmur_2024/MiniBiobank/ParadiseBiobank/ParadiseBiobank_Plasma_10032025.csv"
  file_path_mini_urine   <- "C:/Users/DOGAYY/OneDrive - Trinity College Dublin/Yagmur Dogay/Yagmur_2024/MiniBiobank/ParadiseBiobank/ParadiseBiobank_Urine_10032025.csv"
  file_path_mini_unknown <- "C:/Users/DOGAYY/OneDrive - Trinity College Dublin/Yagmur Dogay/Yagmur_2024/MiniBiobank/ParadiseBiobank/ParadiseBiobank_Unknown_10032025.csv"

  ## ---- Step 1: Clinical data + diagnosis/criteria -------------------------
  message("Step 1/7: Reading clinical data and assigning diagnosis...")
  riv <- read.csv(pipeline_file)
  riv_processed <- assign_diagnosis(riv)

  ## ---- Step 2: ParadiseBiobank files ---------------------------------------
  message("Step 2/7: Reading ParadiseBiobank files...")
  ParadiseBiobank_Serum   <- read.csv(file_path_mini_serum)
  ParadiseBiobank_Plasma  <- read.csv(file_path_mini_plasma)
  ParadiseBiobank_Urine   <- read.csv(file_path_mini_urine)
  ParadiseBiobank_Unknown <- read.csv(file_path_mini_unknown) # loaded for parity with the Rmd; not currently merged in

  biobank_list <- list(
    "Serum"  = ParadiseBiobank_Serum,
    "Plasma" = ParadiseBiobank_Plasma,
    "Urine"  = ParadiseBiobank_Urine
  )

  ## ---- Step 3: Load FW files, combine with Biobank -------------------------
  message("Step 3/7: Loading FW files and merging with Biobank...")
  fw_prefixes <- c("DNA_", "DNAEDTA", "DNANorm", "Serum",
                    "Plasma", "Urine", "RNA_", "RNAPAX", "PBMC_")

  fw_list <- setNames(
    lapply(fw_prefixes, function(p) LoadFW(fw_path, p)),
    fw_prefixes
  )

  all_fw <- prepare_fw_datasets(fw_list, biobank_list)

  ## ---- Step 4: Link FW datasets to clinical data ---------------------------
  message("Step 4/7: Linking FW datasets to clinical data...")
  fw_datasets <- c("DNA", "DNAEDTA", "DNANorm", "Serum_2", "Plasma_2",
                    "Urine_2", "RNA", "RNAPAX", "PBMC")

  linked_multiple <- link_fw_samples(fw_datasets, all_fw, riv_processed)

  ## ---- Step 5: Keep top 4 aliquots per encounter ----------------------------
  message("Step 5/7: Filtering to top 4 aliquots per encounter...")
  filtered_top <- filter_top_aliquots_list(
    data_list   = linked_multiple,
    datasets    = fw_datasets,
    id_col      = "ID_date",
    aliquot_col = "Unique.Aliquot.ID",
    amount_col  = "Current.Amount",
    top_n       = 4
  )

  ## ---- Step 6: Merge aliquot data + total current amount --------------------
  message("Step 6/7: Merging aliquot data into clinical dataset...")
  main_data <- riv_processed
  for (ds in fw_datasets) {
    if (!ds %in% names(filtered_top)) {
      warning(paste0("Dataset not found in filtered_top: ", ds))
      next
    }
    # NOTE: keeps the "_2" suffix on purpose (e.g. "SERUM_2_RIV") -- this is
    # cleaned up later by process_paradise_fw()'s column-name gsub, exactly
    # as in the original Rmd's run_process_pipeline().
    other_data_name <- paste0(toupper(ds), "_RIV")
    message(paste0("  Merging: ", other_data_name))
    main_data <- merge_fw_with_clinicaldata(main_data, filtered_top[[ds]], other_data_name)
    main_data <- calculate_total_current_amount(main_data, other_data_name)
  }

  ## ---- Step 7: Flag Paradise vs FW sample origin -----------------------------
  message("Step 7/7: Flagging Paradise vs FW sample origin...")
  final_data <- process_paradise_fw(main_data)

  ## ---- Optional export --------------------------------------------------------
  if (!is.null(output_path)) {
    ext <- tolower(tools::file_ext(output_path))
    if (ext == "csv") {
      write.csv(final_data, output_path, row.names = FALSE)
    } else if (ext == "xlsx") {
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop("Package 'writexl' is required to write .xlsx output.")
      }
      writexl::write_xlsx(final_data, output_path)
    } else {
      warning(paste0("Unrecognized output_path extension '", ext,
                      "'; nothing written. Use '.csv' or '.xlsx'."))
    }
    if (ext %in% c("csv", "xlsx")) {
      message(paste0("Output written to: ", output_path))
    }
  }

  message("Done.")
  return(final_data)
}
