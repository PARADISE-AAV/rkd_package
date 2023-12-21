#' @title CPD LTROT
#' @author Matthieu COQ/Jennifer Scott
#' @description description to add
#'
#' @param RKDdata Data frame with the RKD data
#' @param duration Duration without treatment
#' @details detail to add
#' 
#' 
#' @return The RKD data with the LTROT category
#' @import dplyr
#' @import tidyverse 
#' @import tidylog
#' @export

CPD_LTROT= function(RKDdata, duration){
  
  if (is.data.frame(RKDdata) == FALSE) {
    stop("The argument RKDdata need to be a dataframe argument")
  }
  if (is.numeric(duration) == FALSE) {
    stop("The argument duration need to be a numeric argument")
  }
  
  dat2 <- RKDdata %>% 
    select(RKD.ID, Date.Of.Visit, Interval.from.diagnosis..months..x, 
           Disease.activity.since.last.return, Do.you.think.Vasculitis.is.relapsing.in.this.encounter,
           Adjudicated.probability.of.relapse.x,
           CPD_relapse, treatment.on.off) %>% 
    dplyr::group_by(RKD.ID) %>% 
    arrange(Date.Of.Visit)
  
  cols <- c("Disease.activity.since.last.return",
            "Do.you.think.Vasculitis.is.relapsing.in.this.encounter"                       ,                     
            "Adjudicated.probability.of.relapse.x"                                          ,   
            "CPD_relapse",
            "treatment.on.off")
  
  dat2 <- dat2 %>%
    mutate_at(cols, factor)
  
  dat_check <- dat2 %>% 
    dplyr::group_by(RKD.ID) %>% 
    mutate(Ever.relapsed_CPD_rel_def = any(CPD_relapse %in% "Definite Relapse"), # TRUE if any encounter == "Definite Relapse"
           Ever.relapsed_CPD_rel_poss = any(CPD_relapse %in% "Possible Relapse"), # TRUE if any encounter == "Possible Relapse"
           Ever.relapsed_CPD_rel_no = any(CPD_relapse %in% "No Relapse"),       # TRUE if any encounter == "No Relapse"
           Ever.relapsed = if_else(Ever.relapsed_CPD_rel_def == TRUE, "yes",
                                   if_else(Ever.relapsed_CPD_rel_poss == TRUE, "NA",
                                           if_else(Ever.relapsed_CPD_rel_no == TRUE, "No", "NA"))),  # see logic above
           Ever.relapsed = as.factor(Ever.relapsed)) %>% 
    ungroup() %>% 
    group_by(RKD.ID, CPD_relapse) %>% 
    mutate(N.event = row_number())
  
  num.rel <- dat_check %>% 
    filter(CPD_relapse == "Definite Relapse") %>% 
    select(RKD.ID, CPD_relapse, N.event, Date.Of.Visit) %>% 
    group_by(RKD.ID) %>% 
    mutate(Num.def.relapse = max(N.event)) %>% 
    mutate(date.1st.def.relapse = min(Date.Of.Visit)) %>% 
    slice(1) %>% 
    select(RKD.ID, Num.def.relapse, date.1st.def.relapse)
  
  dat_check <- left_join(dat_check, num.rel, by = "RKD.ID")
  
  date.last.on.rx <- dat_check %>% 
    group_by(RKD.ID) %>% 
    filter( duration < Interval.from.diagnosis..months..x) %>% 
    filter(treatment.on.off == "On Treatment") %>% 
    mutate(date.last.on.rx = max(Date.Of.Visit)) %>% 
    slice(1) %>% 
    select(RKD.ID, date.last.on.rx)
  
  dat_check <- left_join(dat_check, date.last.on.rx, by = "RKD.ID")
  
  date.rx.stop <- dat_check %>% 
    group_by(RKD.ID) %>% 
    filter( Date.Of.Visit > date.last.on.rx) %>% 
    filter(treatment.on.off == "Not On Treatment") %>% 
    mutate(date.rx.stop_conservative = min(Date.Of.Visit)) %>% 
    mutate(date.rx.stop_liberal = date.last.on.rx) %>% 
    slice(1) %>% 
    select(RKD.ID, date.rx.stop_conservative, date.rx.stop_liberal)
  
  dat_check <- left_join(dat_check, date.rx.stop, by = "RKD.ID")
  
  date.max.fu <- dat_check %>% 
    group_by(RKD.ID) %>% 
    mutate(date.max.fu = max(Date.Of.Visit, na.rm = T)) %>% 
    select(RKD.ID, date.max.fu) %>% 
    slice(1)
  
  dat_check <- left_join(dat_check, date.max.fu, by = "RKD.ID")
  
  dat_check <- dat_check %>% 
    mutate(duration.off.rx.months_conserv =  (date.max.fu - date.rx.stop_conservative )/30.5,
           duration.off.rx.months_conserv = as.numeric(duration.off.rx.months_conserv))
  
  dat_check <- dat_check %>% 
    mutate(duration.off.rx.months_liberal =  (date.max.fu - date.rx.stop_liberal )/30.5,
           duration.off.rx.months_liberal = as.numeric(duration.off.rx.months_liberal))
  
  dat_check <- dat_check %>% 
    group_by(RKD.ID) %>% 
    mutate(LTROT = if_else((Ever.relapsed == "No" & duration.off.rx.months_liberal > duration), "yes", "no"),
           LTROT = as.factor(LTROT))
  
  LTROT_rkd <- merge(RKDdata, dat_check[, c(1,2,9:22)], by= c("RKD.ID", "Date.Of.Visit"))
  
  return(LTROT_rkd)
}