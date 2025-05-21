#' @title CPD Harmonisation
#' @author Matthieu COQ
#'
#' @description The Goal is to finalize the harmonisation
#' 
#' Version: 1.0
#' 
#' Date: 9-May-23
#'
#' @param RIVdata  {"name": "rkd_data","desc": "RIV data from \code{\link{ClassifyRIVEncounter}} function","options": (),"type": "file"}
#' @param output_dir  {"name": "output_dir","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#' @details
#' to be added
#' 
#' @import lubridate
#' @import DT
#' @import dplyr
#' @import fuzzyjoin
#' @importFrom data.table %like%
#' @export

CPD_Harmonisation <- function(RIVdata, output_dir){
  
  stopifnot("Your argument need to be a data frame"=is.data.frame(RIVdata))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  # Check that you load a real file
  if (!ncol(RIVdata) || !nrow(RIVdata)) {
    stop("You supplied an empty file")
  }
  
  rkd_data <- RIVdata[ ,c("RKD.ID", "Date.Of.Visit", "interval_from_diagnosis", "CPD_relapse", "rel.method",  "Urinalysis.Protein", "Urinalysis.Blood", "Number.of.major.BVAS.items", "Number.of.minor.BVAS.items", "CPD_vasc_gran", "Nature.of.confirmed.relapse", "CPD_treatment", "Immunosuppressive.medication...other..ATC", "ANCA_Status", "CRP", "Creatinine", "eGFR..calculated." , "Platelet.count.x10.9.L", "Total.white.cell.count.x10.9.L", "Neutrophil.count.x10.9.L", "Lymphocyte.count.x10.9.L", "Neutrophil...Lymphocyte.ratio", "Monocyte.count.x10.9.L", "Absolute.CD19.count..cells.uL.", "ANCA.IF", "Anti.PR3.level", "PR3.titre", "Anti.MPO.level", "MPO.titre", "Immunosuppressive.status", "Corticosteroids", "Current.corticosteroid.dose",  "ANCA_Switch", "Gender", "Year.of.birth")]
  
  LTROT <- RIVdata[,grep("LTROT_current", colnames(RIVdata))]
  IS <- RIVdata[,grep("immunosup_med", colnames(RIVdata))]
  
  harmonized_data <- cbind(rkd_data, LTROT, IS)
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_clinical_data_harmonized', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}