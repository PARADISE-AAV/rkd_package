#' @title Read Paradise biobank file
#'
#' @description
#' This function reads multiple Paradise_Biobank CSV files from the specified paths
#' and assigns them to variables in the global environment for later use.
#'
#' @param files A named list of file paths. The **names** of the list will be used
#'   as the variable names in the global environment. Example:
#'   \code{list(Urine = "path/to/urine.csv", Blood = "path/to/blood.csv")}
#'
#' @return None. The data frames are stored in the global environment.
#' @import dplyr
#' @import tidyr
#' @import writexl
#' @export
#' 
#' 

read_paradisebiobank_files <- function(files) {
  for (var_name in names(files)) {
    file_path <- files[[var_name]]
    
    # Read CSV file
    df <- read.csv(file_path)
    
    # Assign to global environment with the given name
    assign(var_name, df, envir = .GlobalEnv)

  }
}



