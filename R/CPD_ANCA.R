#' @title CPD ANCA Switch
#' @author Matthieu COQ
#'
#' @description The Goal is to do CPD ANCA Switch
#' 
#' Version: 1.0
#' 
#' Date: 17-Apr-23
#'
#' @param merge_data Encounter and general Characteristic data from \code{\link{Merge_Encounter_initial}} function
#' @param output_dir folder where the Redcap data will be saved
#' @details
#' to be added
#' @export

CPD_ANCA <- function(merge_data, output_dir){
  stopifnot("Your argument need to be a data frame"=is.data.frame(merge_data))
  stopifnot("Your argument need to be a character"=is.character(output_dir))
  
  # Check output directory
  if (!dir.exists(output_dir)) {
    stop('Specified output folder does not exist')
  }
  rkd=merge_data[,c("RKD.ID", "Date.Of.Visit","At.any.point.ANCA.specificity", "Anti.PR3.level", "Anti.MPO.level", "ANCA.IF")]
  
  rkd$ANCA_Levels=NA
  n=nrow(rkd)
  for(i in 1:n){
    if(rkd$At.any.point.ANCA.specificity[i] == "PR3"){
      rkd$ANCA_Levels[i] = rkd$Anti.PR3.level[i]
    }
    if(rkd$At.any.point.ANCA.specificity[i] == "MPO"){
      rkd$ANCA_Levels[i] = rkd$Anti.MPO.level[i]
    }
    if(rkd$At.any.point.ANCA.specificity[i] == "MPO and PR3"){
      rkd$ANCA_Levels[i] = max(rkd$Anti.MPO.level[i], rkd$Anti.PR3.level[i], na.rm=T)
    }
  }
  rkd$ANCA_Statuts="ANCA Status Unknown"
  n=nrow(rkd)
  for(i in 1:n){
    if(rkd$ANCA_Levels[i] == "PR3"){
      rkd$ANCA_Levels[i] = rkd$Anti.PR3.level[i]
    }
    if(rkd$$At.any.point.ANCA.specificity[i] == "MPO"){
      rkd$ANCA_Levels[i] = rkd$Anti.MPO.level[i]
    }
    if(rkd$$At.any.point.ANCA.specificity[i] == "MPO and PR3"){
      rkd$ANCA_Levels[i] = max(rkd$Anti.MPO.level[i], rkd$Anti.PR3.level[i], na.rm=T)
    }
  }
  
}