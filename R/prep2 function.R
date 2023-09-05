#' @title prep2
#' @author Matthieu COQ/Jennifer Scott
#' @Version: 1.0
#' @Date: 07-Jul-23
#' @Objective: The objective is to do the second part of the data preparation
#'
#'
#' @param RKDdata Data frame with the RKD data in
#' @return The RKD data after the second part of the data preparation
#' @import dplyr
#' @export

prep2 <- function(RKDdata){
  
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  RKD_data <- RKD_data %>% 
    group_by(RKD.ID) %>% 
    arrange(Date.Of.Visit)
  RKD_inclusion <- inclusion_criteria(RKD_data)
  RKD_ANCA <- ANCA.titre(RKD_inclusion)
  RKD_blood <- sug.blood(RKD_ANCA)
  RKD_status <- is.status(RKD_blood)
  RKD_BVAS <- complete.bvas(RKD_status)
  RKD_ANCA_all <- complete.ANCA(RKD_BVAS)
  
  return(RKD_ANCA_all)
  
}


#' @title inclusion_criteria
#' @author Matthieu COQ/Jennifer Scott
#' @Version: 1.0
#' @Date: 07-Jul-23
#' @Objective: The objective is to do the second part of the data preparation in particularity the inclusion criteria
#'
#'
#' @param RKDdata Data frame with the RKD data in
#' @return The RKD data after the second part of the data preparation
#' @export

inclusion_criteria <- function(RKDdata){
  library(dplyr)
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  RKD_data <- RKD_data %>% 
    filter(Interval.from.diagnosis..months. >= 3)
  return (RKD_data)

}


#' @title ANCA.titre
#' @author Matthieu COQ/Jennifer Scott
#' @Version: 1.0
#' @Date: 07-Jul-23
#' @Objective: The objective is to do the second part of the data preparation in particularity the inference of ANCA titre
#' @details *** explanation of new & interim variables used below ** 
#' ANCA_IF_infer = if ANCA.IF = Not tested or "" (blank), & lag() (the last encounter) and lead() (the next encounter) are both "negative", then infer this encounter is also negative
#' ANCA.IF.Overall = ANCA_IF_infer, or ANCA.IF (original field) if ANCA_IF_infer is NA 
#' MPO_delta = % rise from previous MPO level in prior encounter (lag(MPO) [i.e. last encounter] must be within 12m of current date) // same for PR3 
#' ANCA.ELISA.delta = if anca specificity labelled as MPO -> use MPO_delta, if labelled as PR3 -> use PR3 delta, if labelled MPO and PR3, use the larger delta 
#' ANCA.ELISA.delta.cat = transform ANCA.ELISA.delta into the same categorical levels of the ANCA.titre field 
#' ANCA.if.delta = same categorical levels based on +/- of the IF (pragmatically chose a rise [i.e. negative to p/c] as = to "<4x rise in ANCA preceding")
#' - if current encounter ANCA.IF.Overall = p/c, and last encounter = negative -> = to "<4x rise in ANCA preceding"
#' - if current encounter ANCA.IF.Overall = negative, and last encounter = negative=> "ANCA unchanged",
#' - if current encounter ANCA.IF.Overall = p/c, and last encounter = p/c -> = "ANCA unchanged"
#' - if current encounter ANCA.IF.Overall = negative, and last encounter = p/c -> = "ANCA decrease"
#' ANCA.overall.delta is ELISA (ANCA.ELISA.delta.cat) if available, if not use ANCA.if.delta, if not = "No ANCA data"
#' - if ELISA and IF delta are both negative -> then label "No ANCA data"
#' ANCA.titre.overall = takes original 'anca.titre' registry field when completed, if not available, takes the new composite 'ANCA.overall.delta' [which is a combo of elisa > IF delta]
#' manually checked - makes sense :)
#'
#' @param RKDdata Data frame with the RKD data in
#' @return The RKD data after the second part of the data preparation
#' @export



ANCA.titre <- function(RKDdata){
  library(dplyr)
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  dat_fullT <- RKD_data
  dat_fullT$ANCA.titre[dat_fullT$ANCA.titre == ""] <- NA
  dat_fullT$Adjudicated.probability.of.relapse[dat_fullT$Adjudicated.probability.of.relapse == ""] <- NA
  dat_fullT$Do.you.think.Vasculitis.is.relapsing.in.this.encounter[dat_fullT$Do.you.think.Vasculitis.is.relapsing.in.this.encounter == ""] <- NA
  dat_fullT$Disease.activity.since.last.return[dat_fullT$Disease.activity.since.last.return == ""] <- NA
  dat_fullT$Suggestive.bloods.OR.urine.tests..excluding.ANCA.[dat_fullT$Suggestive.bloods.OR.urine.tests..excluding.ANCA. == ""] <- NA
  dat_fullT$Suggestive.imaging[dat_fullT$Suggestive.imaging == ""] <- NA
  
  dat_fullT <- dat_fullT %>% 
    group_by(RKD.ID) %>%
    mutate(ANCA_IF_infer = case_when((ANCA.IF == "" | ANCA.IF == "Not tested") & (lag(ANCA.IF, 1) == "Negative") & (lead(ANCA.IF, 1) == "Negative") ~ "Negative")) %>% 
    mutate(ANCA.IF = as.character(ANCA.IF),
           ANCA.IF.Overall = ifelse(!is.na(ANCA_IF_infer), ANCA_IF_infer, ANCA.IF),
           ANCA.IF.Overall = as.factor(ANCA.IF.Overall),
           ANCA.IF = as.factor(ANCA.IF),
           ANCA_IF_infer = as.factor(ANCA_IF_infer)) %>% 
    mutate(Interval.btwn.enc = Interval.from.diagnosis..months. - lag(Interval.from.diagnosis..months., 1)) %>% 
    mutate(MPO_delta = ifelse(Interval.btwn.enc <=12,
                              (100*((Anti.MPO.level - lag(Anti.MPO.level, 1)) / lag(Anti.MPO.level, 1))), NA))  %>%
    mutate(PR3_delta = (100*((Anti.PR3.level - lag(Anti.PR3.level, 1)) / lag(Anti.PR3.level, 1))))  %>%
    mutate(ANCA.ELISA.delta = ifelse(At.any.point.ANCA.specificity == "MPO", MPO_delta,
                                     ifelse(At.any.point.ANCA.specificity == "PR3", PR3_delta,
                                            ifelse((At.any.point.ANCA.specificity == "MPO and PR3" & MPO_delta > PR3_delta),  MPO_delta, PR3_delta)))) %>% 
    mutate(ANCA.ELISA.delta.cat = ifelse(ANCA.ELISA.delta <= -5, "ANCA decrease",
                                         ifelse(ANCA.ELISA.delta > -5 & ANCA.ELISA.delta <5, "ANCA unchanged",
                                                ifelse(ANCA.ELISA.delta >= 5 & ANCA.ELISA.delta < 400, "< 4x rise in ANCA preceding",
                                                       ifelse(ANCA.ELISA.delta >= 400, "4x rise in ANCA preceding", "No ANCA data"))))) %>%  
    mutate(ANCA.if_delta = case_when( 
      (lag(ANCA.IF.Overall, 1) == "Negative" & (ANCA.IF.Overall == "P" | ANCA.IF.Overall == "C")) ~ "< 4x rise in ANCA preceding",
      (lag(ANCA.IF.Overall, 1) == "Negative" & (ANCA.IF.Overall == "Negative")) ~ "ANCA unchanged",
      ( (lag(ANCA.IF.Overall, 1) == "P" | lag(ANCA.IF.Overall, 1) == "C") & (ANCA.IF.Overall == "P" | ANCA.IF.Overall == "C")) ~ "ANCA unchanged",
      ( (lag(ANCA.IF.Overall, 1) == "P" | lag(ANCA.IF.Overall, 1) == "C") & (ANCA.IF.Overall == "Negative")) ~ "ANCA decrease")) %>% 
    mutate(ANCA.overall.delta = ifelse(!is.na(ANCA.ELISA.delta.cat), ANCA.ELISA.delta.cat, 
                                       ifelse(!is.na(ANCA.if_delta), ANCA.if_delta, "No ANCA data"))) %>% 
    mutate(ANCA.overall.delta = as.factor(ANCA.overall.delta),
           ANCA.ELISA.delta.cat = as.factor(ANCA.ELISA.delta.cat),
           ANCA.if_delta = as.factor(ANCA.if_delta)) %>% 
    mutate(ANCA.titre.OVERALL = as.factor(if_else((!is.na(ANCA.titre)), ANCA.titre, ANCA.overall.delta))) 
  
  
  return (dat_fullT)
  
}

#' @title sug.blood
#' @author Matthieu COQ/Jennifer Scott
#' @Version: 1.0
#' @Date: 07-Jul-23
#' @Objective: The objective is to do the second part of the data preparation in particularity the inference for suggested blood answer
#'
#'
#' @param RKDdata Data frame with the RKD data in
#' @return The RKD data after the second part of the data preparation
#' @export

sug.blood <- function(RKDdata){
  library(dplyr)
  library(tidyverse)
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  dat_fullT <- RKD_data
  dat_fullT$Uninalysis.Blood[dat_fullT$Uninalysis.Blood == ""] <- NA
  dat_fullT$Uninalysis.Protein[dat_fullT$Uninalysis.Protein == ""] <- NA
  dat_fullT$Suggestive.bloods.OR.urine.tests..excluding.ANCA.[dat_fullT$Suggestive.bloods.OR.urine.tests..excluding.ANCA. == "No blood/urine tests available"] <- NA
  dat_fullT <- dat_fullT %>% 
    group_by(RKD.ID) %>%
    mutate(creat_delta.pct = 100*((Creatinine)-lag(Creatinine,1))/ lag(Creatinine,1), NA,
           creat_20rise = ifelse(creat_delta.pct > 20, 1, 0)) %>% 
    mutate(ucd163_delta.pct = 100*((Urine.sCD163..ng.mmmol...Euroimmun)-lag(Urine.sCD163..ng.mmmol...Euroimmun,1))/ lag(Urine.sCD163..ng.mmmol...Euroimmun,1), NA,
           ucd163_20rise = ifelse((ucd163_delta.pct > 20 & Urine.sCD163..ng.mmmol...Euroimmun >250), 1, 
                                  ifelse(is.na(ucd163_delta.pct), NA, 0))) %>% 
    mutate(crp_G5 = ifelse(CRP > 5, 1, 0)) %>% 
    # urine.blood.OVERALL = if urinalysis NA & interval between enc <6m -> carry forward last observation (urine.pro the same)
    mutate(urine.blo.orig = Uninalysis.Blood,
           u.blo_guide = ifelse(Interval.btwn.enc <6, 1, NA)) %>% 
    fill(Uninalysis.Blood, .direction = "down") %>% 
    mutate(urine.blood.OVERALL = case_when(!is.na(urine.blo.orig) ~ urine.blo.orig,
                                           u.blo_guide == 1 ~ Uninalysis.Blood)) %>% 
    mutate(urine.pro.orig = Uninalysis.Protein) %>% 
    fill(Uninalysis.Protein, .direction = "down") %>% 
    mutate(urine.protein.OVERALL = case_when(!is.na(urine.pro.orig) ~ urine.pro.orig,
                                             u.blo_guide == 1 ~ Uninalysis.Protein)) %>% 
    relocate(Uninalysis.Protein, .before = urine.protein.OVERALL) %>% 
    mutate(new_haematuria = ifelse(lag(urine.blood.OVERALL) == "Negative" & urine.blood.OVERALL == ">=+3", 1,
                                   ifelse(urine.blood.OVERALL == "Negative", 0,
                                          ifelse((is.na(lag(urine.blood.OVERALL))) | is.na(urine.blood.OVERALL), NA, 0)))) %>% 
    mutate(new_proteinuria = ifelse(lag(urine.protein.OVERALL) == "Negative" & urine.protein.OVERALL == ">=+3", 1,
                                    ifelse(urine.protein.OVERALL == "Negative", 0,
                                           ifelse((is.na(lag(urine.protein.OVERALL))) | is.na(urine.protein.OVERALL), NA, 0)))) %>% 
    mutate(sugg_blo_urine = case_when((creat_20rise == 1 |
                                         ucd163_20rise == 1 |
                                         crp_G5 == 1 |
                                         new_haematuria == 1 |
                                         new_proteinuria == 1) ~ "Suggestive blood/urine tests (excluding ANCA)",
                                      (creat_20rise == 0 |
                                         ucd163_20rise == 0 |
                                         crp_G5 == 0 |
                                         new_haematuria == 0 |
                                         new_proteinuria == 0) ~ "Blood/urine tests are not suggestive of relapse",
                                      (is.na(creat_20rise) &
                                         is.na(ucd163_20rise) &
                                         is.na(crp_G5) &
                                         is.na(new_haematuria) &
                                         is.na(new_proteinuria)) ~ NA_character_ ,
                                      TRUE ~ "Explore"),
           # TRUE ~ "Blood/urine tests are not suggestive of relapse"),
           sugg_blo_urine = as.character(sugg_blo_urine),
           Suggestive.bloods.OR.urine.tests..excluding.ANCA. = as.character(Suggestive.bloods.OR.urine.tests..excluding.ANCA.)) %>% 
    mutate(sugg_blo_urine_OVERALL = ifelse(!is.na(Suggestive.bloods.OR.urine.tests..excluding.ANCA.), Suggestive.bloods.OR.urine.tests..excluding.ANCA.,
                                           ifelse(!is.na(sugg_blo_urine), sugg_blo_urine, "No blood/urine tests available"))) %>%
    mutate(Suggestive.bloods.OR.urine.tests..excluding.ANCA. = as.factor(Suggestive.bloods.OR.urine.tests..excluding.ANCA.),
           sugg_blo_urine_OVERALL = as.factor(sugg_blo_urine_OVERALL))
  
  return(dat_fullT)
  
}



#' @title is.status
#' @author Matthieu COQ/Jennifer Scott
#' @Version: 1.0
#' @Date: 07-Jul-23
#' @Objective: The objective is to do the second part of the data preparation in particularity the inference for immunosuppression status
#' @detail  Explanation of inferring 'is.status' from other fields (required as is.status is a relatively new field in the registry):
#'  easy to follow based on logic below, using case-when function. The logically statements are run in order - i.e. 
#'  if the first statement is positive, the code stops there, e.g.
#'  1. When Immunosuppressive.medication !=(not equals) No => "Currently on immunosuppression" is assigned to is.status,
#'  if Immunosuppressive.medication == (equals) No, then the algorithm checks for the next statement and so on...
#'  ~ essentially means 'equals to'
#'  | means 'OR'

#'  There are 2 conditions for corticosteroids (CCS), as a dose of >10mg is considered 'on' treatment =>
#'  the field 'Corticosteroids' must equal Yes AND the dose must be >10mg (in the registry this can be ""(blank if historical entry) OR 11-20mg/day OR >20mg/day
#'  If all 3 fields: Immunosuppressive.medication, Corticosteroids and Treatment.Naïve..Never.on.Immunosuppression. are blank (is.na()) then NA is assigned to is.status
                                                                         
#'
#' @param RKDdata Data frame with the RKD data in
#' @return The RKD data after the second part of the data preparation
#' @export

is.status <- function(RKDdata){
  library(dplyr)
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  dat_fullT <- RKD_data
  
  dat_fullT$Suggestive.imaging[is.na(dat_fullT$Suggestive.imaging)] <- "No imaging"
  dat_fullT$Immunosuppressive.medication.in.response.to.this.encounter[dat_fullT$Immunosuppressive.medication.in.response.to.this.encounter == ""] <- NA
  dat_fullT$Immunosuppressive.medication.in.response.to.this.encounter[is.na(dat_fullT$Immunosuppressive.medication.in.response.to.this.encounter)] <- "Unknown"
  dat_fullT$Response.to.increased.immunosuppression[dat_fullT$Response.to.increased.immunosuppression == ""] <- NA
  dat_fullT$Response.to.increased.immunosuppression[is.na(dat_fullT$Response.to.increased.immunosuppression)] <- "Unknown response"
  dat_fullT$Immunosuppressive.status[dat_fullT$Immunosuppressive.status == ""] <- NA
  dat_fullT$Corticosteroids[dat_fullT$Corticosteroids == ""] <- NA
  dat_fullT$Immunosuppressive.medication[dat_fullT$Immunosuppressive.medication == ""] <- NA
  dat_fullT$Treatment.Naïve..Never.on.Immunosuppression.[dat_fullT$Treatment.Naïve..Never.on.Immunosuppression. == ""] <- NA
  
  dat_fullT <- dat_fullT %>% 
    mutate(is.status = case_when(Immunosuppressive.medication != "No" ~ "Currently on immunosuppression",
                                 Treatment.Naïve..Never.on.Immunosuppression. == "Yes" ~ "Treatment Naïve",
                                 (Corticosteroids == "Yes" & (Current.corticosteroid.dose == "" |
                                                                Current.corticosteroid.dose == "11 - 20 mg/day" |
                                                                Current.corticosteroid.dose == "> 20 mg/day"))  ~ "Currently on immunosuppression",
                                 (Corticosteroids == "Yes" & (Current.corticosteroid.dose == "< 5 mg/day" |
                                                                Current.corticosteroid.dose == "5 - 10 mg/day") &
                                    Immunosuppressive.medication == "No" )  ~ "Discontinuation of immunosuppression > 6 months prior to this encounter",
                                 (Immunosuppressive.medication == "No" & Corticosteroids == "No" & 
                                    (Treatment.Naïve..Never.on.Immunosuppression. == "No" | is.na(Treatment.Naïve..Never.on.Immunosuppression.))) 
                                 ~ "Discontinuation of immunosuppression > 6 months prior to this encounter",
                                 (is.na(Immunosuppressive.medication) & is.na(Corticosteroids) & is.na(Treatment.Naïve..Never.on.Immunosuppression.)) ~ NA_character_   ),
           is.status = as.factor(is.status)) %>% # NB so that both below are factors are => if_else works ***
    mutate(is.status_OVERALL = if_else(!is.na(Immunosuppressive.status), Immunosuppressive.status, is.status)) 
  
  dat_fullT <- dat_fullT %>% 
    mutate(ccs_calc = case_when(
      Immunosuppressive.status == "Discontinuation of immunosuppression > 6 months prior to this encounter" | 
        Immunosuppressive.status ==   "Discontinuation of immunosuppression within 6 months prior to this encounter" |
        Immunosuppressive.status ==    "Treatment Naïve" ~ "No"),
      ccs_calc = as.factor(ccs_calc),
      ccs_OVERALL = if_else(!is.na(Corticosteroids), Corticosteroids, ccs_calc),
      is.med_calc = case_when(
        Immunosuppressive.status == "Discontinuation of immunosuppression > 6 months prior to this encounter" | 
          Immunosuppressive.status ==   "Discontinuation of immunosuppression within 6 months prior to this encounter" |
          Immunosuppressive.status ==    "Treatment Naïve" ~ "No"),
      is.med_calc = as.factor(is.med_calc),
      is.med_OVERALL = if_else(!is.na(Immunosuppressive.medication), Immunosuppressive.medication, is.med_calc)) 
  
  return(dat_fullT)
  
}

#' @title complete.bvas
#' @author Matthieu COQ/Jennifer Scott
#' @Version: 1.0
#' @Date: 07-Jul-23
#' @Objective: The objective is to do the second part of the data preparation in particularity the inference for BVAS
#'
#'
#' @param RKDdata Data frame with the RKD data in
#' @return The RKD data after the second part of the data preparation
#' @export

complete.bvas <- function(RKDdata){
  library(dplyr)
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  dat_fullT <- RKD_data
  
  dat_fullT <- dat_fullT %>% 
    mutate(bvas_calc_ds.act = case_when(Disease.activity.since.last.return == "Remission" ~ 0),
           bvas_calc_ds.act = as.integer(bvas_calc_ds.act),
           BVAS.use_ALL = case_when(!is.na(BVAS.use) ~ BVAS.use, 
                                    !is.na(bvas_calc_ds.act) ~ bvas_calc_ds.act)) 
  
  return(dat_fullT)
}



#' @title complete.ANCA
#' @author Matthieu COQ/Jennifer Scott
#' @Version: 1.0
#' @Date: 07-Jul-23
#' @Objective: The objective is to do the second part of the data preparation in particularity the inference for ANCA
#' @details **** final variables chosen ****
#'  ANCA.overall.2_NA (creation explained in line 164: use ANCA.titre in redcap, if absent uses change in ANCA elisa, if absent change in ANCA IF ):
#'    levels: no anca rise, <4x rise, 4x rise, NA
#'  sugg.blo.ur_overall_na (creation explained line 223: use sugg bloods/urine from redcap, if not present it is calculated as described):
#'    levels: suggestive bloods/urine, bloods/urine not suggestive, NA
#'  Suggestive.imaging (of note, if left blank, presumed to be 'no imaging' as it is rarely performed so if blank reasonable assumption to assume not done)
#'  levels: no imaging, suggestive imagine, imaging is not suggestive 
#'  is.status_OVERALL (explanation line 436: if is.status available in redcap it is used, otherwise it is inferred from is.med and ccs and ccs.dose):
#'    levels: currently on IS, d/c within 6/12, d/c >6m ago, NA
#'  is.response_b (uses the field from redcap and merges levels to give: IS increased, no known increase (includes no change, decrease, stopped), NA)
#'  diag_bx_3 (uses field from redcap with levels merged: Suggestive bx (definitive/biopsy consistent but not deefinitive), negative (biopsy not consistent), no biopsy)
#' 
#'
#' @param RKDdata Data frame with the RKD data in
#' @return The RKD data after the second part of the data preparation
#' @export

complete.ANCA <- function(RKDdata){
  library(dplyr)
  ####Test on the argument
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  dat_fullT <- RKD_data
  
  dat_fullT <- dat_fullT %>% 
    #dat_adj.enc <- dat_adj.enc %>% 
    mutate(ANCA.titre.OVERALL = fct_relevel(ANCA.titre.OVERALL, "ANCA unchanged", "ANCA decrease"),
           Diagnostic.biopsy = fct_relevel(Diagnostic.biopsy, "No biopsy", "Biopsy not consistent with vasculitis",
                                           "Biopsy consistent but not definitive", "Definitive biopsy"),
           Suggestive.imaging = fct_relevel(Suggestive.imaging, "No imaging"),
           Immunosuppressive.medication.in.response.to.this.encounter = 
             fct_relevel(Immunosuppressive.medication.in.response.to.this.encounter, 
                         "No change in Immunosuppression" ),
           Response.to.increased.immunosuppression = fct_relevel(Response.to.increased.immunosuppression,
                                                                 "Unknown response"),
           Diagnostic.biopsy = dplyr::recode(Diagnostic.biopsy, `Unknown` = "No biopsy"),
           diag_biopsy_b = case_when(Diagnostic.biopsy == "Definitive biopsy" ~ "Definitive biopsy",
                                     TRUE ~ "No known definitive biopsy"),
           diag_biopsy_b = as.factor(diag_biopsy_b),
           diag_bx_3 = case_when(Diagnostic.biopsy == "Definitive biopsy" ~ "Suggestive biopsy",
                                 Diagnostic.biopsy == "Biopsy consistent but not definitive" ~ "Suggestive biopsy",
                                 Diagnostic.biopsy == "Biopsy not consistent with vasculitis" ~ "Negative biopsy",
                                 Diagnostic.biopsy == "No biopsy" ~ "No biopsy"),
           diag_bx_3 = as.factor(diag_bx_3),
           anca_3 = dplyr::recode(ANCA.titre.OVERALL, 
                                  `ANCA decrease` = "No known ANCA rise",
                                  `ANCA unchanged` = "No known ANCA rise",
                                  `No ANCA data` = "No known ANCA rise"),
           anca_3 = as.factor(anca_3),
           anca_3 = fct_relevel(anca_3, "No known ANCA rise"),
           blo.urine_b = dplyr::recode(sugg_blo_urine_OVERALL,
                                       `Blood/urine tests are not suggestive of relapse` = "No known suggestive bloods/urine tests",
                                       `No blood/urine tests available` = "No known suggestive bloods/urine tests"),
           blo.urine_b = as.factor(blo.urine_b),
           image_b = dplyr::recode(Suggestive.imaging,
                                   `Imaging is not suggestive of relapse` = "No known suggestive imaging",
                                   `No imaging` = "No known suggestive imaging"),
           image_b = as.factor(image_b),
           is.status_b = dplyr::recode(is.status_OVERALL,
                                       `Discontinuation of immunosuppression > 6 months prior to this encounter` = "Not on IS",
                                       `Discontinuation of immunosuppression within 6 months prior to this encounter` = "Not on IS",
                                       `Treatment Naïve` = "Not on IS"),
           is.status_b = as.factor(is.status_b),
           is.status_b = fct_relevel(is.status_b, "Not on IS"),
           is.response_b = dplyr::recode(Immunosuppressive.medication.in.response.to.this.encounter,
                                         `Immunosuppression reduced` = "No known IS increase",
                                         `Immunosuppression stopped` = "No known IS increase",
                                         `No change in Immunosuppression` = "No known IS increase",
                                         `Unknown` = "Unknown"),
           is.response_b = as.factor(is.response_b),
           is.response_b = fct_relevel(is.response_b, "No known IS increase"),
           is.resp_M.red.stop = dplyr::recode(Immunosuppressive.medication.in.response.to.this.encounter,
                                              `No change in Immunosuppression` = "No change in IS",
                                              `Immunosuppression increased` = "IS increased",
                                              `Immunosuppression reduced` = "IS reduced/stopped",
                                              `Immunosuppression stopped` = "IS reduced/stopped",
                                              `Unknown` = "Unknown"),
           is.resp_M.red.stop = as.factor(is.resp_M.red.stop),
           IS.change.AND.resp = case_when(Immunosuppressive.medication.in.response.to.this.encounter == "No change in Immunosuppression" ~ "No change in IS",
                                          Immunosuppressive.medication.in.response.to.this.encounter == "Immunosuppression reduced" ~ "IS reduced",
                                          Immunosuppressive.medication.in.response.to.this.encounter == "Immunosuppression stopped" ~ "IS stopped",
                                          (Immunosuppressive.medication.in.response.to.this.encounter == "Immunosuppression increased" &
                                             Response.to.increased.immunosuppression == "Clear response to Immunosuppression") ~ "IS increased & clear response to same",
                                          (Immunosuppressive.medication.in.response.to.this.encounter == "Immunosuppression increased" &
                                             Response.to.increased.immunosuppression == "No response to Immunosuppression") ~ "IS increased & no response to same",
                                          (Immunosuppressive.medication.in.response.to.this.encounter == "Immunosuppression increased" &
                                             Response.to.increased.immunosuppression == "Unknown response") ~ "IS increased & unknown response to same"),
           IS.change.AND.resp = as.factor(IS.change.AND.resp),
           IS.change.AND.resp = fct_relevel(IS.change.AND.resp, "No change in IS",
                                            "IS increased & clear response to same", "IS increased & no response to same", "IS increased & unknown response to same",
                                            "IS reduced", "IS stopped"),
           IS.change.AND.resp_M.red.stop = dplyr::recode(IS.change.AND.resp,
                                                         `IS reduced` = "IS reduced/stopped", 
                                                         `IS stopped` = "IS reduced/stopped"),
           IS.change.AND.resp_M.red.stop = as.factor(IS.change.AND.resp_M.red.stop),
           IS.change.AND.resp_M.red.stop.unchange = dplyr::recode(IS.change.AND.resp,
                                                                  `IS reduced` = "IS not increased", 
                                                                  `IS stopped` = "IS not increased",
                                                                  `No change in IS` = "IS not increased"),
           IS.change.AND.resp_M.red.stop.unchange = as.factor(IS.change.AND.resp_M.red.stop.unchange),
           bvas_bin = case_when(BVAS.use_ALL == 0 ~ "0",
                                BVAS.use_ALL >0 ~ "1",
                                TRUE ~ NA_character_),
           bvas_bin = as.factor(bvas_bin),
           is.med.yn = if_else(Immunosuppressive.medication == "No", "No", "Yes"),
           is.med.yn = as.factor(is.med.yn),
           ANCA.overall_NA = recode_factor(ANCA.titre.OVERALL, `No ANCA data` = NA_character_),
           sugg.blo.ur_overall_na = recode_factor(sugg_blo_urine_OVERALL, `No blood/urine tests available` = NA_character_),
           is.response_NA = recode_factor(Immunosuppressive.medication.in.response.to.this.encounter, `Unknown` = NA_character_),
           response_inc.is_NA = recode_factor(Response.to.increased.immunosuppression, `Unknown response` = NA_character_),
           is.response_b = recode_factor(is.response_b, `Unknown` = NA_character_),
           is.resp_M.red.stop = recode_factor(is.resp_M.red.stop, `Unknown` = NA_character_),
           ANCA.overall.2_NA = dplyr::recode(ANCA.overall_NA,
                                             `ANCA decrease` = "No ANCA rise",
                                             `ANCA unchanged` = "No ANCA rise")) 
  
  dat_fullT <- dat_fullT %>%
    mutate(is.response_b = case_when(is.response_b == "Immunosuppression increased" ~ "Immunosuppression increased",
                                     ((is.response_b != "Immunosuppression increased") & 
                                        (Corticosteroids.in.response.to.this.clinical.encounter.episode == "Increased")) ~ "Immunosuppression increased",
                                     ((is.response_b == "No known IS increase") & 
                                        (Corticosteroids.in.response.to.this.clinical.encounter.episode != "Increased")) ~ "No known IS increase",
                                     TRUE ~ NA_character_),
           is.response_b = as.factor(is.response_b),
           is.response_b = fct_relevel(is.response_b, "No known IS increase", "Immunosuppression increased"))
  
  
  return(dat_fullT)
}