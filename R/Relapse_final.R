#' @title The application of the rules defned in the details of \code{\link{CPDRelapse}}
#' @author Matthieu COQ/Jennifer Scott
#' @description
#'  The objective is to apply the clinical rules of \code{\link{CPDRelapse}}
#' Version: 1.0
#' Date: 31-Jul-23
#'
#' @param RKDdata Data frame from \code{\link{cpd_relapse}}
#' @return The RIV data with a relapse variable ready to finalized \code{\link{CPDRelapse}}
#' @import dplyr
#' @export

Relapse_final <- function(RKDdata){
  
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  
  RKD_data <- RKDdata
  ###check that you load a real file
  if(ncol(RKD_data)==0 | nrow(RKD_data)==0){
    stop("You give an empty files")
  }
  
  df_miss_pred <- RKD_data %>% 
    mutate(Adjudicated.probability.of.relapse = as.character(Adjudicated.probability.of.relapse),
           rel.JS = case_when(
             (Adjudicated.probability.of.relapse == "Possible" & Relapse.prediction != "Manual review") ~ Relapse.prediction,
             !is.na(Adjudicated.probability.of.relapse) ~ Adjudicated.probability.of.relapse, 
             !is.na(Relapse.prediction) ~ Relapse.prediction, 
             TRUE ~ NA_character_),
           rel.method = case_when(
             (Adjudicated.probability.of.relapse == "Possible" & Relapse.prediction != "Manual review") ~ "Algorithm derived",
             !is.na(Adjudicated.probability.of.relapse) ~ "Adjudicated", 
             !is.na(Relapse.prediction) ~ "Algorithm derived",
             TRUE ~ NA_character_))
  
  df_miss_pred2 <- df_miss_pred %>% 
    group_by(ID) %>% 
    mutate(Interval.m.to.next.enc = lead(Interval.from.diagnosis..months.) - Interval.from.diagnosis..months.,
           Interval.m.to.prior.enc = Interval.from.diagnosis..months. - lag(Interval.from.diagnosis..months.)) %>% 
    mutate(rel.JS.interim = case_when( 
      (lag(rel.JS) %in% c("1", "Definite", "High Probability") & 
         Interval.m.to.prior.enc <= 3) ~ "Relapse within 90d, so exclude",
      (lag(rel.JS, 2) %in% c("1", "Definite", "High Probability") & 
         ((Interval.m.to.prior.enc + lag(Interval.m.to.prior.enc)) <= 3)) ~ "Relapse within 90d, so exclude",
      (lag(rel.JS, 3) %in% c("1", "Definite", "High Probability") & 
         ((Interval.m.to.prior.enc + lag(Interval.m.to.prior.enc) + lag(Interval.m.to.prior.enc, 2)) <= 3)) ~ "Relapse within 90d, so exclude",
      (lag(rel.JS, 4) %in% c("1", "Definite", "High Probability") & 
         ((Interval.m.to.prior.enc + lag(Interval.m.to.prior.enc) + lag(Interval.m.to.prior.enc, 2) + lag(Interval.m.to.prior.enc, 3)) <= 3)) ~ "Relapse within 90d, so exclude",
      (lag(rel.JS, 5) %in% c("1", "Definite", "High Probability") & 
         ((Interval.m.to.prior.enc + lag(Interval.m.to.prior.enc) + lag(Interval.m.to.prior.enc, 2) + lag(Interval.m.to.prior.enc, 3) + lag(Interval.m.to.prior.enc, 4)) <= 3)) 
      ~ "Relapse within 90d, so exclude",
      (lag(rel.JS, 6) %in% c("1", "Definite", "High Probability") & 
         ((Interval.m.to.prior.enc + lag(Interval.m.to.prior.enc) + lag(Interval.m.to.prior.enc, 2) + lag(Interval.m.to.prior.enc, 3) + lag(Interval.m.to.prior.enc, 4) + 
             lag(Interval.m.to.prior.enc, 5)) <= 3)) ~ "Relapse within 90d, so exclude",
      (lag(rel.JS, 7) %in% c("1", "Definite", "High Probability") & 
         ((Interval.m.to.prior.enc + lag(Interval.m.to.prior.enc) + lag(Interval.m.to.prior.enc, 2) + lag(Interval.m.to.prior.enc, 3) + lag(Interval.m.to.prior.enc, 4) + 
             lag(Interval.m.to.prior.enc, 5) + lag(Interval.m.to.prior.enc, 6)) <= 3)) ~ "Relapse within 90d, so exclude"),
      rel.JS.OVERALL = as.factor(ifelse(!is.na(rel.JS.interim), rel.JS.interim, rel.JS))) %>% 
    relocate(Adjudicated.probability.of.relapse, .after = Relapse.prediction)
  
  
  return(df_miss_pred2)
}
