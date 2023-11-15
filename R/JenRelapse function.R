#' @title CPD_Relapse
#' @author Matthieu COQ/Jennifer Scott
#' @Version: 1.0
#' @Date: 07-Jul-23
#' @Objective: The objective is to apply jen's approach for relapse on RKD data
#'
#'
#' @param RKDdata Data frame with the RKD data in
#' @details
#' 
#' 
#' 
#' @return The RKD data with the relapse category
#' @export

JenRelapse <- function(RKDdata){
  
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  prep1_data <- prep1(RKD_data)
  prep2_data <- prep2(prep1_data)
  final_data <- cpd.relapse(prep2_data)
  relapse_final_data <- Relapse_final(final_data)
  
  Classify_RKD_data <- merge(RKD_data, relapse_final_data, by.x = c("RKD.ID", "Date.Of.Visit"), by.y = c("ID", "Date.Of.Visit"))
  
  return(relapse_final_data)
  
}