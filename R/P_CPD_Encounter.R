#' @title CPD Encounter
#' @author Matthieu COQ
#'
#' @description The Goal is to get modification done before the merge with general characteristics from \code{\link{SplitRIV}} function
#' 
#' Version: 1.0
#' 
#' Date: 18-Apr-23
#' @param Encounter_data  {"name": "Encounter_data","desc": "RIV data from \code{\link{SplitRIV}} function","options": (),"type": "file"}
#' @param output_dir  {"name": "output_dir","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#'
#' @details
#' 
#' in this function, we modify the creatinine for the patient in dialysis to 999 and we calculate the MPO and PR3 titre based on geometric mean
#' 
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import forcats
#' @importFrom rlang .data
#' @export
CPD_Encounter <- function (Encounter_data, output_dir){
  stopifnot("Your argument need to be a data frame"=is.list(Encounter_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  
  rkd_data <- Encounter_data
  n=nrow(rkd_data)
  for(i in 1:n){
    if(rkd_data$Dialysis.dependent[i] == "Yes"){
      rkd_data$Creatinine[i]=999
    } 
  }
  
  MPO_geom_mean=exp(mean(log(rkd_data$Anti.MPO.level [rkd_data$Anti.MPO.level>0]), na.rm = TRUE))
  PR3_geom_mean=exp(mean(log(rkd_data$Anti.PR3.level [rkd_data$Anti.PR3.level>0]), na.rm = TRUE))
  
  rkd_data <- rkd_data %>%
    dplyr::mutate(MPO.titre = dplyr::case_when(
      Anti.MPO.level < 2 ~ "Negative",
      is.na(Anti.MPO.level) == TRUE ~ "Not available",
      Anti.MPO.level > 2 & Anti.MPO.level < MPO_geom_mean ~ "Low positive",
      Anti.MPO.level > MPO_geom_mean ~ "High positive"
    ))%>%
    dplyr::mutate(PR3.titre = dplyr::case_when(
      Anti.PR3.level < 2 ~ "Negative",
      is.na(Anti.PR3.level) == TRUE ~ "Not available",
      Anti.PR3.level > 2 & Anti.PR3.level < PR3_geom_mean ~ "Low positive",
      Anti.PR3.level > PR3_geom_mean ~ "High positive"
    ))
  
  output_filename <- file.path(
    output_dir,
    paste0('Redcap_Encounter_data_merged', "_version", packageVersion('rivpipeline'), "_Date"
           , Sys.Date(), '.csv')
  )
  write.csv(rkd_data, output_filename, row.names = FALSE)
  return(rkd_data)
  
}