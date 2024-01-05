#' @title prep1_demo
#' @author Matthieu COQ/Jennifer Scott
#' 
#' @description
#' The objective is to do the first part of the data preparation in the \code{\link{CPDRelapse}} for the demographic data (At.any.point.ANCA.specificity, End.stage.kidney.disease, Systems.involved.at.any.point, Induction.treatment.received)
#' 
#' Version: 1.0
#' 
#' Date: 07-Jul-23
#'
#' @param RKDdata Data frame with the RKD data coming from \code{\link{FilterRKD}}
#' @return The RKD data after the first part of the data preparation on demographic data ready for \code{\link{prep1_enc}}
#' @import dplyr
#' @import forcats
#' @export

prep1_demo <- function(RKDdata){


  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
 
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  RKD_data$At.any.point.ANCA.specificity <- as.factor(RKD_data$At.any.point.ANCA.specificity)
  RKD_data$Small.vessel.vasculitis..ANCA.associated. <- as.factor(RKD_data$Small.vessel.vasculitis..ANCA.associated.)
  RKD_data$End.stage.kidney.disease <- as.factor(RKD_data$End.stage.kidney.disease)
  RKD_data$Gender <- as.factor(RKD_data$Gender)
  
  for (i in c(grep("Systems.involved.at.any.point", colnames(RKD_data)), grep("Induction.treatment.received..choice", colnames(RKD_data)))){
    RKD_data[,i] <- as.factor(RKD_data[,i])
  }
  
  dat_demo <- RKD_data %>%
    mutate(Gender = forcats::fct_drop(Gender)) %>%
    mutate(At.any.point.ANCA.specificity = fct_relevel(At.any.point.ANCA.specificity,
                                                       "PR3","MPO",
                                                       "MPO and PR3",
                                                       "ELISA negative" ,
                                                       "No ELISA performed")) %>%
    mutate(At.any.point.ANCA.specificity = fct_drop(At.any.point.ANCA.specificity)) %>% 
    
    mutate(Small.vessel.vasculitis..ANCA.associated. = fct_relevel(Small.vessel.vasculitis..ANCA.associated.,
                                                                   "Granulomatosis with polyangiitis (Wegener) - Orpha:900",
                                                                   "Microscopic polyangiitis (including renal limited vasculitis) - ORPHA:727",
                                                                   "Eosinophilic granulomatosis with polyangiitis (Churg Strauss) - ORPHA:183")) %>%
    mutate(Small.vessel.vasculitis..ANCA.associated. = recode_factor(Small.vessel.vasculitis..ANCA.associated.,
                                                                     `Granulomatosis with polyangiitis (Wegener) - Orpha:900` = "GPA",
                                                                     `Microscopic polyangiitis (including renal limited vasculitis) - ORPHA:727` = "MPA",
                                                                     `Eosinophilic granulomatosis with polyangiitis (Churg Strauss) - ORPHA:183` = "EGPA")) %>%
    mutate(Small.vessel.vasculitis..ANCA.associated. = fct_drop(Small.vessel.vasculitis..ANCA.associated.)) %>% 
    mutate(End.stage.kidney.disease = fct_collapse(End.stage.kidney.disease,
                                                   `1` = "Yes",
                                                   `0` = c("No", ""))) %>% 
    mutate(Status = fct_drop(Status))
  
  dat_demo <- dat_demo %>% 
    mutate(Systems.involved.at.any.point..choice.Musculoskeletal. = fct_drop(Systems.involved.at.any.point..choice.Musculoskeletal.),
           Systems.involved.at.any.point..choice.Mucocutaneous. = fct_drop(Systems.involved.at.any.point..choice.Mucocutaneous.),
           Systems.involved.at.any.point..choice.Eye. = fct_drop(Systems.involved.at.any.point..choice.Eye.),
           Systems.involved.at.any.point..choice.ENT. = fct_drop(Systems.involved.at.any.point..choice.ENT.),
           Systems.involved.at.any.point..choice.Trachea. = fct_drop(Systems.involved.at.any.point..choice.Trachea.),
           Systems.involved.at.any.point..choice.Lung..granuloma.. = fct_drop(Systems.involved.at.any.point..choice.Lung..granuloma..),
           Systems.involved.at.any.point..choice.Lung..haemorrhage.. = fct_drop(Systems.involved.at.any.point..choice.Lung..haemorrhage..),
           Systems.involved.at.any.point..choice.Cardiovascular. = fct_drop(Systems.involved.at.any.point..choice.Cardiovascular.),
           Systems.involved.at.any.point..choice.Kidney. = fct_drop(Systems.involved.at.any.point..choice.Kidney.),
           Systems.involved.at.any.point..choice.Abdominal. = fct_drop(Systems.involved.at.any.point..choice.Abdominal.),
           Systems.involved.at.any.point..choice.CNS. = fct_drop(Systems.involved.at.any.point..choice.CNS.),
           Systems.involved.at.any.point..choice.PNS. = fct_drop(Systems.involved.at.any.point..choice.PNS.),
           Systems.involved.at.any.point..choice.Interstitial.lung.disease. = fct_drop(Systems.involved.at.any.point..choice.Interstitial.lung.disease.))
  
  dat_demo <- dat_demo %>%
    mutate(Induction.treatment.received..choice.Oral.corticosteroids. = fct_drop(Induction.treatment.received..choice.Oral.corticosteroids.),
           Induction.treatment.received..choice.Pulsed.IV.corticosteroids. = fct_drop(Induction.treatment.received..choice.Pulsed.IV.corticosteroids.),
           Induction.treatment.received..choice.Daily.Oral.Cyclophosphamide. = fct_drop(Induction.treatment.received..choice.Daily.Oral.Cyclophosphamide.),
           Induction.treatment.received..choice.Pulsed.IV.Cyclophosphamide. = fct_drop(Induction.treatment.received..choice.Pulsed.IV.Cyclophosphamide.),
           Induction.treatment.received..choice.Rituximab. = fct_drop(Induction.treatment.received..choice.Rituximab.),
           Induction.treatment.received..choice.MMF. = fct_drop(Induction.treatment.received..choice.MMF.),
           Induction.treatment.received..choice.Azathioprine. = fct_drop(Induction.treatment.received..choice.Azathioprine.),
           Induction.treatment.received..choice.Methotrexate. = fct_drop(Induction.treatment.received..choice.Methotrexate.),
           Induction.treatment.received..choice.Other. = fct_drop(Induction.treatment.received..choice.Other.),
           Induction.treatment.received..choice.Plasma.exchange. = fct_drop(Induction.treatment.received..choice.Plasma.exchange.),
           Any.Induction.Treatment = fct_drop((Any.Induction.Treatment))
    ) 
  
  dat_demo <- dat_demo%>% 
    mutate(Lung.involve = case_when((Systems.involved.at.any.point..choice.Lung..granuloma.. == "Checked" | Systems.involved.at.any.point..choice.Lung..haemorrhage.. == "Checked") ~ 1, 
                                    TRUE ~ 0),
           Neuro.involve = case_when((Systems.involved.at.any.point..choice.CNS. == "Checked" | Systems.involved.at.any.point..choice.PNS. == "Checked") ~ 1,
                                     TRUE ~ 0),
           ENT.involve = case_when((Systems.involved.at.any.point..choice.ENT. == "Checked" | Systems.involved.at.any.point..choice.Trachea. == "Checked") ~ 1,
                                   TRUE ~ 0))
  fact.vars <- c("Lung.involve", "Neuro.involve", "ENT.involve")
  dat_demo <- dat_demo %>% 
    mutate_at(fact.vars, factor)
  
  return(dat_demo)
}

#' @title prep1_enc
#' @author Matthieu COQ/Jennifer Scott
#' 
#' 
#' @description
#' The objective is to do the first part of the data preparation in the \code{\link{CPDRelapse}} with encounter data (Adjudicated.probability.of.relapse, BVAS.score..calculator. and Diagnostic.biopsy)
#' 
#' Version: 1.0
#' 
#' Date: 07-Jul-23
#'
#'
#' @param RKDdata Data frame with the RKD data in
#' @return The RKD data after the first part of the data preparation on encounter data
#' @import dplyr
#' @export

prep1_enc <- function(RKDdata){
  
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  
  dat_enc <- RKD_data %>% 
    mutate(relapse_def_hp = fct_collapse(Adjudicated.probability.of.relapse,
                                         `1` = c("Definite", "High Probability"),
                                         `0` = c("Possible", "No")), # 1= 227/ 0=1952 (original: def 64, hp 163, poss 111, no 1841)
           rel_def_hp_pos = fct_collapse(Adjudicated.probability.of.relapse,
                                         `1` = c("Definite", "High Probability", "Possible"),
                                         `0` = c("No")),  # 1= 338/ 0=1841
           rel_def_hp_excludePOSS = fct_collapse(Adjudicated.probability.of.relapse,
                                                 `1` = c("Definite", "High Probability"),
                                                 `0` = c( "No"))) %>%    # 1= 227/ 0=1841 #numbers correct 
    mutate(Adjudicated.probability.of.relapse = fct_relevel(Adjudicated.probability.of.relapse, "Definite", "High Probability", "Possible", "No", "")) 
  dat_enc <- dat_enc %>% 
    mutate(BVAS.use = ifelse(!is.na(BVAS.score..calculator.), BVAS.score..calculator. , BVAS.score))
  
  dat_enc <- dat_enc %>% 
    relocate(c(Number.of.major.BVAS.items, Number.of.minor.BVAS.items), .after = BVAS.score..calculator.) %>% 
    relocate(Adjudicated.probability.of.relapse.interval.calculation, .after = Adjudicated.probability.of.relapse) %>% 
    relocate(c(Uninalysis.Protein, Uninalysis.Blood), .after = Treatment.Naive..Never.on.Immunosuppression.) %>% 
    relocate(c(relapse_def_hp, rel_def_hp_pos, rel_def_hp_excludePOSS), .after = Adjudicated.probability.of.relapse.interval.calculation) %>% 
    relocate(BVAS.use, .after = BVAS.score..calculator.)
  dat_enc$Diagnostic.biopsy <- as.character(dat_enc$Diagnostic.biopsy)
  dat_enc$Diagnostic.biopsy[dat_enc$Diagnostic.biopsy == ""] <- "Unknown"
  return(dat_enc)
}

#' @title prep1_merged
#' @author Matthieu COQ/Jennifer Scott
#' @description
#' The objective is to summarize the induction treatment receive (Induction.treatment.received) and add this summary to the data
#' 
#' Version: 1.0
#' 
#' Date: 07-Jul-23
#'
#' @param RKDdata Data frame with the RKD data in
#' @return The RKD data with the summary of induction treatment
#' @import dplyr
#' @export

prep1_merged <- function(RKDdata){
  
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  rx_explore <- RKD_data %>%
    group_by(RKD.ID) %>%
    slice(1) %>%
    select(RKD.ID, Induction.treatment.received..choice.Daily.Oral.Cyclophosphamide., Induction.treatment.received..choice.Pulsed.IV.Cyclophosphamide.,
           Induction.treatment.received..choice.Rituximab., Induction.treatment.received..choice.MMF., Induction.treatment.received..choice.Azathioprine.,
           Induction.treatment.received..choice.Methotrexate., Induction.treatment.received..choice.Oral.corticosteroids., Induction.treatment.received..choice.Pulsed.IV.corticosteroids., Any.Induction.Treatment) %>%
    mutate(induc.rx = case_when(Induction.treatment.received..choice.Daily.Oral.Cyclophosphamide. == "Checked" &
                                  Induction.treatment.received..choice.Rituximab. == "Unchecked" & Induction.treatment.received..choice.Pulsed.IV.Cyclophosphamide. == "Unchecked" ~ "PO CYC",
                                Induction.treatment.received..choice.Pulsed.IV.Cyclophosphamide. == "Checked" & Induction.treatment.received..choice.Rituximab. == "Unchecked" &
                                  Induction.treatment.received..choice.Daily.Oral.Cyclophosphamide. == "Unchecked" ~ "IV CYC",
                                Induction.treatment.received..choice.Rituximab. == "Checked" & Induction.treatment.received..choice.Daily.Oral.Cyclophosphamide. == "Unchecked" &
                                  Induction.treatment.received..choice.Pulsed.IV.Cyclophosphamide. == "Unchecked"  ~ "RTX",
                                Induction.treatment.received..choice.Rituximab. == "Checked" & (Induction.treatment.received..choice.Daily.Oral.Cyclophosphamide. == "Checked" |
                                                                                                  Induction.treatment.received..choice.Pulsed.IV.Cyclophosphamide. == "Checked")  ~ "RTX & CYC",
                                (Induction.treatment.received..choice.MMF. == "Checked"| Induction.treatment.received..choice.Azathioprine. == "Checked" |
                                   Induction.treatment.received..choice.Methotrexate. == "Checked" ~ "MMF/MTX/AZA"),
                                ( Induction.treatment.received..choice.Oral.corticosteroids. == "Checked"| Induction.treatment.received..choice.Pulsed.IV.corticosteroids. == "Checked") &
                                  (Induction.treatment.received..choice.Rituximab. == "Unchecked" & Induction.treatment.received..choice.Daily.Oral.Cyclophosphamide. == "Unchecked" &
                                     Induction.treatment.received..choice.Pulsed.IV.Cyclophosphamide. == "Unchecked" &  Induction.treatment.received..choice.MMF. == "Unchecked" &
                                     Induction.treatment.received..choice.Azathioprine. == "Unchecked" &
                                     Induction.treatment.received..choice.Methotrexate.  == "Unchecked") ~ "GCC only",
                                Induction.treatment.received..choice.Daily.Oral.Cyclophosphamide. == "Checked" &  Induction.treatment.received..choice.Pulsed.IV.Cyclophosphamide. == "Checked" &
                                  Induction.treatment.received..choice.Rituximab. == "Unchecked" ~ "PO & IV CYC",
                                Any.Induction.Treatment == "Not Known" ~ "Unknown",
                                Any.Induction.Treatment == "No" ~ "Non"))
  rx_explore$induc.rx <- as.factor(rx_explore$induc.rx)
  rx_explore <- rx_explore %>%
    mutate(cyc.rtx = case_when(induc.rx == "PO CYC" | induc.rx == "IV CYC" | induc.rx == "PO & IV CYC" ~ "CYC",
                               induc.rx == "RTX" ~ "RTX",
                               induc.rx == "RTX & CYC" ~ "CYC & RTX",
                               induc.rx == "MMF/MTX/AZA" | induc.rx == "GCC only" | induc.rx == "Non" | induc.rx == "Unknown" ~ "Other")) 
  rx_explore$cyc.rtx <- as.factor(rx_explore$cyc.rtx)
  rx_explore <- rx_explore %>%   
    mutate(cyc.rtx = fct_relevel(cyc.rtx, "CYC", "RTX", "CYC & RTX", "Other")) %>% 
    mutate(induc.rx = fct_relevel(induc.rx, "PO CYC", "IV CYC", "PO & IV CYC", "RTX", "RTX & CYC", "MMF/MTX/AZA", "GCC only", "Unknown", "Non"))
  
  rx.sum <- rx_explore %>%
    select(RKD.ID, induc.rx, cyc.rtx)
  dat_demo <- full_join(RKD_data, rx.sum, by = "RKD.ID")
  return(dat_demo)
}

#' @title prep1
#' @author Matthieu COQ/Jennifer Scott
#' 
#' 
#' @description
#'  The objective is to do the first part of the data preparation for the \code{\link{CPDRelapse}}
#'  
#' Date: 07-Jul-23
#' 
#' Version: 1.0
#'
#' @param RKDdata Data frame with the RKD data 
#' @details The different step of the first part of the data preparation in the \code{\link{CPDRelapse}} are the following
#' * \code{\link{prep1_demo}} that prepare the demographic data (At.any.point.ANCA.specificity, End.stage.kidney.disease, Systems.involved.at.any.point, Induction.treatment.received)
#' * \code{\link{prep1_enc}} that prepare the encounter data (Adjudicated.probability.of.relapse, BVAS.score..calculator. and Diagnostic.biopsy)
#' * \code{\link{prep1_merged}} that summarize the induction treatment receive (Induction.treatment.received) and add this summary to the data
#' 
#' @return The RKD data after the first part of the data preparation in the \code{\link{CPDRelapse}} and ready for the \code{\link{prep2}}
#' @export

prep1 <- function(RKDdata){
  
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  prep1_demo1 <- prep1_demo(RKD_data)
  prep1_enc1 <- prep1_enc(prep1_demo1)
  prep1_data <- prep1_merged(prep1_enc1)
  
  return(prep1_data)
}