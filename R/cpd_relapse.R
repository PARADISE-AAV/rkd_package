#' @title cpd.relapse
#' @author Matthieu COQ/Jennifer Scott
#' 
#' @description
#' The objective is to assign to an encounter if he is in relapse stage or not. This function is apply after \code{\link{prep2}} function
#' 
#' Version: 1.0
#' 
#' Date: 07-Jul-23
#'
#' @param RKDdata Data frame after the preparation of the data (\code{\link{prep2}}) to apply this function
#' @return The RKD data with the status if the patient is in remission
#' @import dplyr
#' @export

cpd.relapse <- function (RKDdata){
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
  
  dat <- dat_fullT %>%
    filter(Interval.from.diagnosis..months. >= 6) %>% 
    select(RKD.ID, Date.Of.Visit, 
           Repeat.Instance, Interval.from.diagnosis..months., 
           diag_bx_3, ANCA.overall.2_NA, sugg.blo.ur_overall_na, Suggestive.imaging, is.status_OVERALL, is.response_b,
           Adjudicated.probability.of.relapse ) %>%
    dplyr::rename(ID = RKD.ID,
                  Biopsy = diag_bx_3,
                  ANCA.titre = ANCA.overall.2_NA,
                  Suggestive.bloods.urine = sugg.blo.ur_overall_na ,
                  Suggestive.imaging =   Suggestive.imaging ,
                  IS.status = is.status_OVERALL,
                  IS.response = is.response_b ) %>%
    filter(IS.status %in% c("Currently on immunosuppression", "Discontinuation of immunosuppression > 6 months prior to this encounter",
                            "Discontinuation of immunosuppression within 6 months prior to this encounter", NA))
  set.seed(9582)
  
  model_master_list <- readRDS(file = system.file("extdata/model_master_list_17.1.23.rds",package = "rkdpipeline"))
  
  
  cut_point_master <- readRDS(file = system.file("extdata/cut_point_master_17.1.23.rds",package = "rkdpipeline"))
  model_ind <- expand.grid(c(0,1), c(0, 1), c(0, 1), c(0, 1), c(0, 1)) %>% apply(1, as.logical) %>% t
  model_ind <- model_ind[-1, ]
  id <- dat %>% 
    pull(ID) 
  n_patient <- length(id)
  dat_temp <- dat[ , 6:10]
  test2_present <- !is.na(dat_temp)
  
  df_miss_pred <- data.frame(
    dat, #11 cols
    #ID = vector(n_patient, mode = "character"), 
    Model.index = structure(rep(0, n_patient)), #col12 =  model index (1-31) depending on variables present (previously: test2_ind)
    Cut.point = structure(rep(0, n_patient)), #col13
    Predicted.probability.relapse = structure(rep(0, n_patient)), #col14
    Relapse.prediction = structure(rep(0, n_patient)) #col15
  )
  for(ind in 1:n_patient)
  {
    dat_sub <- subset(dat, ID == id[ind] )
   
    
    if(as.character(dat$Biopsy[ind]) == "Suggestive biopsy")
    {
      df_miss_pred[ind, 14] <- 1.00
      df_miss_pred[ind, 15] <- 1
    }
    else
    {
      test2_present2 <- test2_present[ind, ]
      
      Model.index <- apply(model_ind, 1, function(x) all(x == test2_present2)) %>% which
      df_miss_pred[ind, 12] <- Model.index
      
      model_temp <- model_master_list[[Model.index]]
      
      df_miss_pred[ind, 13] <- cut_point_master[Model.index]
      
      if(!Model.index %in% c(2,10,13,5,9,1,12,4,8)) 
        #note: Model Rank:ind (var no.) = 23:2 (2), 24:10 (2,4), 25:13 (1,3,4), 26:5 (1,3), 27:9 (1,4), 28:1 (1), 29:12 (3,4), 30:4 (3), 31:8 (4)
        # 19:14 (2,3,4) and 20:6 (2,3) EXPLORED as 'unacceptable models' for PARADISE - as most positive cases captured in 'manual review' as prob 0.2-0.7, 
        # decided to keep models as 'acceptable' with this extra accuracy check (see word doc for)
      {
        Predicted.probability.relapse = predict(model_temp, newdata = dat[ind,], type = "response",
                                                allow.new.levels = TRUE)
        df_miss_pred[ind, 14] <- Predicted.probability.relapse
        
        Relapse.prediction <- case_when(
          ((Predicted.probability.relapse > cut_point_master[Model.index]) & (Predicted.probability.relapse > 0.7)) ~ "1", 
          ((Predicted.probability.relapse > cut_point_master[Model.index]) & (Predicted.probability.relapse <= 0.7)) ~ "Manual review",
          ((Predicted.probability.relapse <= cut_point_master[Model.index]) & (Predicted.probability.relapse < 0.2)) ~ "0",
          ((Predicted.probability.relapse <= cut_point_master[Model.index]) & (Predicted.probability.relapse >= 0.2)) ~ "Manual review",
          TRUE ~ "check")
        df_miss_pred[ind, 15] <- Relapse.prediction
      }
      else
      {
        df_miss_pred[ind, 14] <- NA
        df_miss_pred[ind, 15] <- "Manual review"
      }
    }
  }
  return(df_miss_pred)
}