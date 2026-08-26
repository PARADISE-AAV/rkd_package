#' @title This function use the different algorithms to filter the patient of RIV data
#' @author Matthieu COQ
#' @description
#' The objective is to filter the RIV data based on inclusion criteria defined in several algorithms
#' 
#' Version: 1.0
#' 
#' Date: 03-Jan-24
#'
#' @param RKDdata {"name": "rkd_data","desc": "RIV data from \code{\link{SplitRIV_dataframe}} function","options": (),"type": "file"}
#' @param output_path {"name": "output_path","desc": "folder where the Redcap data will be saved","options": (),"type": "string"}
#' @param algorithm {"name": "algorithm","desc": "function use to filter the RKD patient","options": ("Definite GPA", "Definite MPA", "Definite EGPA", "Anti-GBM", "Double positive", "Healthy Control", "IgA", "Cryoglobulinemic", "Disease Control"),"type": "string"}
#' @return The Redcap data with the classification variables in your folder and in an R object 
#' @details 
#' The filter of the RKD data are based on the following filter. The filter are define in the following document, [Filter_criteria](https://3.basecamp.com/3790396/buckets/31062049/google_documents/8110318588)
#' 
#' @export
FilterRIV = function(RKDdata, output_path, algorithm) {
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  if (is.character(output_path) == FALSE) {
    stop("The argument output_path need to be a character argument")
  }
  
  RKD_data <- RKDdata
  
  ###check that you load a real file
  if (ncol(RKD_data) == 0 | nrow(RKD_data) == 0) {
    stop("You give an empty files")
  }
  
  RKD_data$DefiniteMPA <- 0
  RKD_data$DefiniteGPA <- 0
  RKD_data$DefiniteEGPA <- 0
  RKD_data$AntiGBM <- 0
  RKD_data$DoublePositive <- 0
  RKD_data$IgA <- 0
  RKD_data$Cryoglobulinemic <- 0
  RKD_data$HC <- 0
  
  
  Filter_RKD_data <- NULL
  
  if (is.element("Definite GPA", algorithm)) {
    Filter_RKD_data1 <- DefiniteGPA(RKD_data)
    Filter_RKD_data1$DefiniteGPA <- 1
    Filter_RKD_data <- rbind(Filter_RKD_data, Filter_RKD_data1)
  }
  if (is.element("Definite MPA", algorithm)) {
    Filter_RKD_data1 <- DefiniteMPA(RKD_data)
    Filter_RKD_data1$DefiniteMPA <- 1
    Filter_RKD_data <- rbind(Filter_RKD_data, Filter_RKD_data1)
  }
  if (is.element("Definite EGPA", algorithm)) {
    Filter_RKD_data1 <- DefiniteEGPA(RKD_data)
    Filter_RKD_data1$DefiniteEGPA <- 1
    Filter_RKD_data <- rbind(Filter_RKD_data, Filter_RKD_data1)
  }
  if (is.element("Anti-GBM", algorithm)) {
    Filter_RKD_data1 <- AntiGBMDisease(RKD_data)
    Filter_RKD_data1$AntiGBM <- 1
    Filter_RKD_data <- rbind(Filter_RKD_data, Filter_RKD_data1)
  }
  if (is.element("Double positive", algorithm)) {
    Filter_RKD_data1 <- DoublePositive(RKD_data)
    Filter_RKD_data1$DoublePositive <- 1
    Filter_RKD_data <- rbind(Filter_RKD_data, Filter_RKD_data1)
  }
  if (is.element("Healthy Control", algorithm)) {
    Filter_RKD_data1 <- HealthyControl(RKD_data)
    Filter_RKD_data1$HC <- 1
    Filter_RKD_data <- rbind(Filter_RKD_data, Filter_RKD_data1)
  }
  if (is.element("IgA", algorithm)) {
    Filter_RKD_data1 <- IgA(RKD_data)
    Filter_RKD_data1$IgA <- 1
    Filter_RKD_data <- rbind(Filter_RKD_data, Filter_RKD_data1)
  }
  if (is.element("Cryoglobulinemic", algorithm)) {
    Filter_RKD_data1 <- Cryoglobulinemic(RKD_data)
    Filter_RKD_data1$Cryoglobulinemic <- 1
    Filter_RKD_data <- rbind(Filter_RKD_data, Filter_RKD_data1)
  }
  if (is.element("Disease Control", algorithm)) {
    Filter_RKD_data1 <- RKD_data[which(RKD_data$Type.of.Patient=="Other Disease")]
    Filter_RKD_data <- rbind(Filter_RKD_data, Filter_RKD_data1)
  }
  
  files_test <-  list.dirs(output_path)
  if (identical(files_test, character(0)) == TRUE) {
    stop("Your output folder don't exist")
  }
  
  output_filename <- file.path(output_path,
                               paste0("Redcap_clinical_data_filter", "_version", packageVersion('rivpipeline'), "_Date", Sys.Date(), ".csv"))
  
  write.csv(Filter_RKD_data, output_filename, row.names = FALSE)
  return(Filter_RKD_data)
  
}