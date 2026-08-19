# =============================================================================
# ISI (Immunosuppression / Treatment Intensity Score) pipeline
# R translation of ISI_score_base__RIV_MC__2_.ipynb
#
# Requires: dplyr, tidyr, purrr, rlang, stringr
# =============================================================================

# for na.locf() forward-fill

# -----------------------------------------------------------------------------
# 1. GENERIC HELPERS
# -----------------------------------------------------------------------------

#' @title sigmoid curve
#' @description Logistic decay curve used for every "sigmoid" ITIS component
#' @param x data
#' @param A max of ISI
#' @param n time when the drug have no effect
#' @param d time when the drug is at mid effect
#' 
sigmoid_curve <- function(x, A, n, d) {
  A / (1 + exp(n * (x - d)))
}

#' @title build_sigmoid_params
#' @description Build calculate_A / calculate_d / calculate_n closures from the two
#' anchor points (dose1 -> A1/d1, dose2 -> A2/d2) plus the two "vanish day"
#' anchors, exactly mirroring the notebook's linear-interpolation blocks.
#' @param A1 the max of ISI score with the min dose
#' @param A2  the max of ISI score with the max dose
#' @param dose1 min dose
#' @param dose2 max dose
#' @param d1 the days after which the immunosuppression effect is reduced by 50% for min dose
#' @param d2  the days after which the immunosuppression effect is reduced by 50% for max dose
#' @param vanish1 the time in days to the disappearance of the immunosuppression effects for min dose
#' @param vanish2  the time in days to the disappearance of the immunosuppression effects for max dose
#' 
build_sigmoid_params <- function(A1, A2, dose1, dose2, d1, d2, vanish1, vanish2) {
  m_A <- (A2 - A1) / (dose2 - dose1)
  b_A <- A1 - m_A * dose1
  calculate_A <- function(dose) m_A * dose + b_A

  m_d <- (d2 - d1) / (dose2 - dose1)
  b_d <- d1 - m_d * dose1
  calculate_d <- function(dose) m_d * dose + b_d

  calculate_n_from_vanish <- function(A, d, vanish_day) log(99) / (vanish_day - d)
  n1 <- calculate_n_from_vanish(A1, d1, vanish1)
  n2 <- calculate_n_from_vanish(A2, d2, vanish2)

  m_n <- (n2 - n1) / (dose2 - dose1)
  b_n <- n1 - m_n * dose1
  calculate_n <- function(dose) m_n * dose + b_n

  list(calculate_A = calculate_A, calculate_d = calculate_d, calculate_n = calculate_n)
}

#' @title cumulative_ITIS
#' @description Cumulative "1 - prod(1 - x_i)" combination rule used everywhere in the
#' notebook to combine several ITIS components into one score.
#' @param df  dataframe
#' @param cols column
#' @param out_col output column
#' 
cumulative_ITIS <- function(df, cols, out_col) {
  cols <- intersect(cols, names(df))
  vals <- as.matrix(df[cols])
  out <- apply(vals, 1, function(row) {
    row <- row[!is.na(row)]
    if (length(row) == 0) return(NA_real_)
    1 - prod(1 - row)
  })
  df[[out_col]] <- out
  df
}

# -----------------------------------------------------------------------------
# 2. IV-THERAPY (long -> wide -> visit-merge -> dose-combine) MACHINERY
#    Used for Methylprednisolone (mtp), IV Cyclophosphamide (cyc) and
#    Rituximab (rtx).
# -----------------------------------------------------------------------------

#' @title pivot_iv_wide
#' @description Pivot a long-format IV-therapy extract to one row per patient, with
#' sequential dose columns  <prefix>_<n>_dose_date / <prefix>_<n>_dose_value
#' @param iv_drug IVTherapy frame
#' @param prefix name of the drug
pivot_iv_wide <- function(iv_drug, prefix) {
  iv_drug <- iv_drug %>%
    arrange(RKD.ID, Date.of.IV.therapy) %>%
    group_by(RKD.ID) %>%
    mutate(dose_number = row_number()) %>%
    ungroup()

  wide_date <- iv_drug %>%
    select(RKD.ID, dose_number, Date.of.IV.therapy) %>%
    pivot_wider(names_from = dose_number, values_from = Date.of.IV.therapy,
                names_glue = paste0(prefix, "_{dose_number}_dose_date"))

  wide_value <- iv_drug %>%
    select(RKD.ID, dose_number, Dose.of.IV.therapy) %>%
    pivot_wider(names_from = dose_number, values_from = Dose.of.IV.therapy,
                names_glue = paste0(prefix, "_{dose_number}_dose_value"))

  full_join(wide_date, wide_value, by = "RKD.ID")
}

#' @title merge_visits_with_iv
#' @description Merge the wide dose table onto the visit-level data, blanking any dose
#' that has not yet occurred as of that visit (dose_date must be < visit date).
#' @param visits encounter dataframe 
#' @param wide wide
#' @param prefix prefix
#' 
merge_visits_with_iv <- function(visits, wide, prefix) {
  df <- visits %>% mutate(Date_Of_Visit = as.Date(Date_Of_Visit))
  df <- left_join(df, wide, by = "RKD.ID")

  dose_date_cols <- grep(paste0("^", prefix, "_[0-9]+_dose_date$"), names(df), value = TRUE)

  for (dc in dose_date_cols) {
    n <- as.integer(str_extract(dc, "(?<=_)[0-9]+(?=_)"))
    vc <- paste0(prefix, "_", n, "_dose_value")
    df[[dc]] <- as.Date(df[[dc]])
    keep <- !is.na(df[[dc]]) & (df[[dc]] < df$Date_Of_Visit)
    df[[dc]][!keep] <- as.Date(NA)
    df[[vc]][!keep] <- NA
  }
  df
}

#' @title reorder_iv_columns
#' @description Reorder columns so each dose block (date, value, and optional vanish columns) sits together in
#' ascending dose-number order, matching reorder_*_columns() in the notebook.
#' @param df dataframe
#' @param prefix prefix
#' @param extra_suffixes extra suffixes
#' 
reorder_iv_columns <- function(df, prefix, extra_suffixes = character(0)) {
  cols <- names(df)
  dose_nums <- sort(unique(as.integer(str_match(
    grep(paste0("^", prefix, "_[0-9]+_dose_date$"), cols, value = TRUE),
    paste0("^", prefix, "_([0-9]+)_dose_date$"))[, 2])))

  core_cols <- cols[!startsWith(cols, paste0(prefix, "_"))]

  suffixes <- c("dose_date", "dose_value", extra_suffixes)
  dose_blocks <- unlist(lapply(dose_nums, function(n) {
    candidates <- paste0(prefix, "_", n, "_", suffixes)
    candidates[candidates %in% cols]
  }))

  other_cols <- setdiff(cols[startsWith(cols, paste0(prefix, "_"))], dose_blocks)

  df[, c(core_cols, dose_blocks, other_cols)]
}

#' @title update_iv_doses
#' @description Combine two adjacent doses into one, per patient, when they occurred
#' within `max_days` of each other: the target dose's date/value are folded
#' into the base dose (summed), and the target columns are blanked.
#' Direct translation of update_mtp_doses / update_cyc_doses / update_rtx_doses.
#' @param df dataframe
#' @param prefix prefix
#' @param base_dose dose of base
#' @param target_dose target dose
#' @param max_days max days
#' 
update_iv_doses <- function(df, prefix, base_dose, target_dose, max_days) {
  base_date   <- paste0(prefix, "_", base_dose,   "_dose_date")
  target_date <- paste0(prefix, "_", target_dose, "_dose_date")
  base_value  <- paste0(prefix, "_", base_dose,   "_dose_value")
  target_value<- paste0(prefix, "_", target_dose, "_dose_value")

  if (!all(c(base_date, target_date, base_value, target_value) %in% names(df))) return(df)

  df[[base_date]]    <- as.Date(df[[base_date]])
  df[[target_date]]  <- as.Date(df[[target_date]])
  df[[base_value]]   <- suppressWarnings(as.numeric(df[[base_value]]))
  df[[target_value]] <- suppressWarnings(as.numeric(df[[target_value]]))

  df %>%
    group_by(RKD.ID) %>%
    group_modify(function(g, key) {
      d_base   <- g[[base_date]][!is.na(g[[base_date]])]
      d_target <- g[[target_date]][!is.na(g[[target_date]])]

      if (length(d_base) == 0 || length(d_target) == 0) return(g)

      base_date_val   <- d_base[1]
      target_date_val <- d_target[1]
      interval_days   <- as.numeric(target_date_val - base_date_val)

      if (is.na(interval_days) || interval_days > max_days) return(g)

      v_base   <- g[[base_value]][!is.na(g[[base_date]])]
      v_base   <- v_base[!is.na(v_base)]
      v_target <- g[[target_value]][!is.na(g[[target_date]])]
      v_target <- v_target[!is.na(v_target)]

      base_val   <- if (length(v_base) > 0) v_base[1] else NA_real_
      target_val <- if (length(v_target) > 0) v_target[1] else NA_real_
      cumulative <- base_val + target_val

      mask <- !is.na(g[[base_date]])
      g[[base_date]][mask]  <- target_date_val
      g[[base_value]][mask] <- cumulative
      g[[target_date]]  <- as.Date(NA)
      g[[target_value]] <- NA_real_

      g
    }) %>%
    ungroup()
}

#' Apply a whole sequence of update_iv_doses() calls.
#'
#' @param df Data frame containing IV dose columns.
#' @param prefix Character prefix identifying the therapy.
#' @param plan List of combination steps; each element is c(base_dose, target_dose, max_days).
#' @return Updated data frame.
apply_combine_plan <- function(df, prefix, plan) {
  for (step in plan) {
    df <- update_iv_doses(df, prefix, base_dose = step[1], target_dose = step[2], max_days = step[3])
  }
  df
}

#' @title apply_rename_plan
#' @description Rename dose blocks, e.g. rename_map = list(c(10,6), c(13,7)) means
#' "<prefix>_10_dose_date" -> "<prefix>_6_dose_date", etc. (applied in order,
#' exactly as in the notebook's sequential rename() calls).
#' @param df dataframe
#' @param prefix prefix
#' @param rename_map rename map
#' 
apply_rename_plan <- function(df, prefix, rename_map) {
  for (pair in rename_map) {
    old_n <- pair[1]; new_n <- pair[2]
    old_date <- paste0(prefix, "_", old_n, "_dose_date")
    old_val  <- paste0(prefix, "_", old_n, "_dose_value")
    new_date <- paste0(prefix, "_", new_n, "_dose_date")
    new_val  <- paste0(prefix, "_", new_n, "_dose_value")
    if (old_date %in% names(df)) names(df)[names(df) == old_date] <- new_date
    if (old_val  %in% names(df)) names(df)[names(df) == old_val]  <- new_val
  }
  df
}

#' @title drop_all_na_columns
#' @description Drop columns that are entirely NA (mirrors df.dropna(axis=1, how='all'))
#' @param df dataframe
drop_all_na_columns <- function(df) {
  df[, colSums(!is.na(df)) > 0, drop = FALSE]
}

#' @title compute_days_since_dose
#' @description Days between visit and each remaining dose date, clipped at 0.
#' @param df dataframe
#' @param prefix prefix
#' @param doses doses
#' 
compute_days_since_dose <- function(df, prefix, doses) {
  for (n in doses) {
    dc <- paste0(prefix, "_", n, "_dose_date")
    if (dc %in% names(df)) {
      newcol <- paste0("days_visit_minus_", prefix, n)
      df[[newcol]] <- as.numeric(as.Date(df$Date_Of_Visit) - as.Date(df[[dc]]))
      df[[newcol]] <- pmax(df[[newcol]], 0, na.rm = FALSE)
      df[[newcol]][is.na(df[[newcol]])] <- NA_real_
    }
  }
  df
}

#' @title compute_dose_ITIS_scores
#' @description Clip dose values, then compute the per-dose ITIS (sigmoid) score for a
#' whole run of doses (1..max(doses)), creating <prefix>_dose<n>_ISI_score.
#' @param df dataframe
#' @param prefix prefix
#' @param doses doses
#' @param clip_lower clip lower
#' @param clip_upper clip upper
#' @param sp sp
#'  
compute_dose_ITIS_scores <- function(df, prefix, doses, clip_lower, clip_upper, sp) {
  for (n in doses) {
    value_col <- paste0(prefix, "_", n, "_dose_value")
    if (value_col %in% names(df)) {
      df[[value_col]] <- suppressWarnings(as.numeric(df[[value_col]]))
      df[[value_col]] <- pmin(pmax(df[[value_col]], clip_lower), clip_upper)
    }
  }

  for (n in doses) {
    value_col <- paste0(prefix, "_", n, "_dose_value")
    time_col  <- paste0("days_visit_minus_", prefix, n)
    out_col   <- paste0(prefix, "_dose", n, "_ISI_score")

    if (value_col %in% names(df) && time_col %in% names(df)) {
      dose <- df[[value_col]]
      time <- df[[time_col]]
      A <- sp$calculate_A(dose)
      d <- sp$calculate_d(dose)
      n_ <- sp$calculate_n(dose)
      score <- sigmoid_curve(time, A, n_, d)
      score[is.na(dose) | is.na(time)] <- NA_real_
      df[[out_col]] <- score
    }
  }
  df
}

# -----------------------------------------------------------------------------
# 3. CONTINUOUS-MEDICATION MACHINERY
#    Used for oral Cyclophosphamide, Azathioprine, Mycophenolate Mofetil.
#    conmed is the long "continuous medication" extract with one row per
#    prescription episode: RKD.ID, Drug, Start.Date, Stop.Date, Dose, ...
# -----------------------------------------------------------------------------

#' @title merge_continuous_medication
#' @description Attach a drug's Start.Date / Stop.Date / dose-related columns onto the
#' visit-level data for the (single) episode active at each visit
#' (Start.Date < Date_Of_Visit <= Stop.Date), then forward-fill Stop.Date
#' per patient and compute the day-interval since that stop date.
#' @param visits Encounter frame
#' @param conmed Continuous medication frame
#' @param drug_name Drug Name
#' @param prefix Prefix of column name
#' 
merge_continuous_medication <- function(visits, conmed, drug_name, prefix) {
  drug_df <- conmed %>%
    filter(Drug == drug_name) %>%
    distinct(RKD.ID, Drug, Start.Date, Dose, .keep_all = TRUE) %>%
    rename(!!paste0(prefix, "_Unit.of.Doses") := Unit.of.Doses,
           !!paste0(prefix, "_Frequency")     := Frequency,
           !!paste0(prefix, "_Start.Date")    := Start.Date,
           !!paste0(prefix, "_On.going")      := On.going,
           !!paste0(prefix, "_Stop.Date")     := Stop.Date) %>%
    mutate(across(all_of(c(paste0(prefix, "_Start.Date"), paste0(prefix, "_Stop.Date"))), as.Date))

  visits <- visits %>% mutate(Date_Of_Visit = as.Date(Date_Of_Visit))

  start_col <- paste0(prefix, "_Start.Date")
  stop_col  <- paste0(prefix, "_Stop.Date")
  cols_to_merge <- c(paste0(prefix, "_Unit.of.Doses"), paste0(prefix, "_Frequency"),
                     start_col, paste0(prefix, "_On.going"), stop_col)

  merged <- left_join(visits, drug_df, by = "RKD.ID")
  filtered <- merged %>%
    filter(!!sym(start_col) < Date_Of_Visit, Date_Of_Visit <= !!sym(stop_col))

  visits <- left_join(
    visits,
    filtered %>% select(RKD.ID, Date_Of_Visit, all_of(cols_to_merge)),
    by = c("RKD.ID", "Date_Of_Visit")
  ) %>%
    distinct(RKD.ID, Date_Of_Visit, .keep_all = TRUE)

  # blank rows where the visit IS the start date
  is_start_visit <- !is.na(visits[[start_col]]) & (visits[[start_col]] == visits$Date_Of_Visit)
  visits[is_start_visit, cols_to_merge] <- NA

  # forward-fill Stop.Date within patient, then compute interval (>=0)
  visits <- visits %>%
    arrange(RKD.ID, Date_Of_Visit) %>%
    group_by(RKD.ID) %>%
    mutate(!!paste0(prefix, "_Stop.Date_ffill") := zoo::na.locf(!!sym(stop_col), na.rm = FALSE)) %>%
    ungroup()

  interval_col <- paste0("interval_from_", prefix, "_stop_date")
  visits[[interval_col]] <- as.numeric(visits$Date_Of_Visit - visits[[paste0(prefix, "_Stop.Date_ffill")]])
  visits[[interval_col]] <- pmax(visits[[interval_col]], 0)

  visits
}

#' @description Linear dose->score mapping used for Aza / MMF / Methotrexate:
#' score = min_score for dose<=min_dose, max_score for dose>=max_dose,
#' linear in between.
 #' @title linear_dose_score
#' @param dose dataframe 
#' @param min_dose minimal dose
#' @param max_dose maximal dose
#' @param min_score minimal score
#' @param max_score maximal score
linear_dose_score <- function(dose, min_dose, max_dose, min_score, max_score) {
  score <- ifelse(dose <= min_dose, min_score,
            ifelse(dose >= max_dose, max_score,
                   min_score + (dose - min_dose) / (max_dose - min_dose) * (max_score - min_score)))
  score[is.na(dose)] <- NA_real_
  score
}

# -----------------------------------------------------------------------------
# 4. PER-DRUG PIPELINES
#    Each function takes the running visit-level data frame and returns it
#    with new score column(s) attached. Numeric parameters and dose-combine
#    sequences are transcribed directly from the notebook.
# -----------------------------------------------------------------------------

## ---- 4.1 Methylprednisolone IV (mtp) --------------------------------------
#' @title run_mtp_pipeline
#' @param data1 dataframe with all the previous ISI score calculated
#' @param IV IV Therapy frame
run_mtp_pipeline <- function(data1, IV) {
  mtp <- IV %>% filter(IV.therapy == "Methylprednisolone - UATC/D07AA01")
  mtp <- mtp[, colSums(!is.na(mtp)) > 0]

  mask_g <- mtp$Unit.of.dose == "g"
  mtp$Dose.of.IV.therapy[mask_g] <- mtp$Dose.of.IV.therapy[mask_g] * 1000
  mtp$Unit.of.dose[mask_g] <- "mg"
  mtp$Dose.of.IV.therapy[mtp$Dose.of.IV.therapy < 250] <- 250

  mtp_wide <- pivot_iv_wide(mtp, "mtp")
  df <- merge_visits_with_iv(data1, mtp_wide, "mtp")
  df <- reorder_iv_columns(df, "mtp")
  df <- drop_all_na_columns(df)

  # Sequential dose-combination plan (max_days = 30, as in the notebook)
  combine_plan <- list(
    c(1,2,30), c(1,3,30), c(1,4,30), c(1,5,30), c(1,6,30), c(1,7,30), c(1,8,30), c(1,9,30),
    c(2,3,30), c(2,4,30),
    c(3,4,30), c(3,5,30),
    c(4,5,30), c(4,6,30), c(4,7,30),
    c(10,11,30), c(10,12,30),
    c(13,14,30)
  )
  df <- apply_combine_plan(df, "mtp", combine_plan)
  df <- drop_all_na_columns(df)

  df <- apply_rename_plan(df, "mtp", list(c(10,6), c(13,7)))
  df <- update_iv_doses(df, "mtp", base_dose = 6, target_dose = 7, max_days = 30)
  df <- drop_all_na_columns(df)

  doses <- 1:6
  df <- compute_days_since_dose(df, "mtp", doses)

  sp <- build_sigmoid_params(A1 = 0.40, A2 = 0.80, dose1 = 250, dose2 = 3000,
                              d1 = 15, d2 = 21, vanish1 = 21, vanish2 = 30)
  df <- compute_dose_ITIS_scores(df, "mtp", doses, clip_lower = 250, clip_upper = 3000, sp)

  itis_cols <- paste0("mtp_dose", doses, "_ISI_score")
  df <- cumulative_ITIS(df, itis_cols, "MTP_IV_ISI_score")
  df$MTP_IV_ISI_score[is.na(df$MTP_IV_ISI_score)] <- 0
  df
}

## ---- 4.2 IV Cyclophosphamide (cyc) -----------------------------------------
#' @title run_cyc_iv_pipeline
#' @param mtp_with_intervals dataframe with all the previous ISI score calculated
#' @param IV IV Therapy frame
run_cyc_iv_pipeline <- function(mtp_with_intervals, IV) {
  cyc <- IV %>% filter(IV.therapy == "Cyclophosphamide Injectable Solution - UATC/ L01AA01")
  cyc <- cyc[, colSums(!is.na(cyc)) > 0]

  mask_g <- cyc$Unit.of.dose == "g"
  cyc$Dose.of.IV.therapy[mask_g] <- cyc$Dose.of.IV.therapy[mask_g] * 1000
  cyc$Unit.of.dose[mask_g] <- "mg"
  cyc$Dose.of.IV.therapy[cyc$Dose.of.IV.therapy < 150] <- 150

  cyc_wide <- pivot_iv_wide(cyc, "cyc")
  df <- merge_visits_with_iv(mtp_with_intervals, cyc_wide, "cyc")
  df <- drop_all_na_columns(df)
  df <- reorder_iv_columns(df, "cyc")

  combine_plan <- list(
    c(1,2,180), c(1,3,180), c(1,4,180), c(1,5,180), c(1,6,180), c(1,7,180), c(1,8,180),
    c(1,9,180), c(1,10,180), c(1,11,180), c(1,12,180), c(1,13,180),
    c(2,3,180), c(2,4,180),
    c(3,4,180),
    c(4,5,180), c(4,6,180), c(4,7,180), c(4,8,180),
    c(6,7,180), c(6,8,180), c(6,9,180), c(6,10,180), c(6,11,180), c(6,12,180),
    c(6,13,180), c(6,14,180), c(6,15,180),
    c(7,8,180),
    c(11,12,180),
    c(12,13,180), c(12,14,180), c(12,15,180), c(12,16,180), c(12,17,180)
  )
  df <- apply_combine_plan(df, "cyc", combine_plan)
  df <- drop_all_na_columns(df)

  df <- apply_rename_plan(df, "cyc", list(c(6,5), c(7,6), c(8,7), c(9,8), c(10,9), c(11,10), c(12,11)))

  doses <- 1:11
  df <- compute_days_since_dose(df, "cyc", doses)

  sp <- build_sigmoid_params(A1 = 0.60, A2 = 0.90, dose1 = 150, dose2 = 8000,
                              d1 = 40, d2 = 80, vanish1 = 60, vanish2 = 110)
  df <- compute_dose_ITIS_scores(df, "cyc", doses, clip_lower = 150, clip_upper = 8000, sp)

  # dose 8 is excluded from the cumulative combination in the notebook
  itis_cols <- paste0("cyc_dose", setdiff(doses, 8), "_ISI_score")
  df <- cumulative_ITIS(df, itis_cols, "CYC_IV_ISI_score")
  df$CYC_IV_ISI_score[is.na(df$CYC_IV_ISI_score)] <- 0
  df
}

## ---- 4.3 Rituximab (rtx) ---------------------------------------------------
#' @title run_rtx_pipeline
#' @param rtx_with_intervals dataframe with all the previous ISI score calculated
#' @param IV IV Therapy frame
run_rtx_pipeline <- function(rtx_with_intervals, IV) {
  rtx <- IV %>%
    filter(IV.therapy %in% c("Rituximab - UATC/L01XC02 -- Mabthera",
                              "Rituximab - UATC/L01XC02 -- Ruxience",
                              "Rituximab - UATC/L01XC02 -- Truxima")) %>%
    distinct(RKD.ID, IV.therapy, Date.of.IV.therapy, Dose.of.IV.therapy, .keep_all = TRUE)

  mask_g <- rtx$Unit.of.dose == "g"
  rtx$Dose.of.IV.therapy[mask_g] <- rtx$Dose.of.IV.therapy[mask_g] * 1000
  rtx$Unit.of.dose[mask_g] <- "mg"
  rtx$Dose.of.IV.therapy[rtx$Dose.of.IV.therapy < 500] <- 500

  rtx_wide <- pivot_iv_wide(rtx, "rtx")
  df <- merge_visits_with_iv(rtx_with_intervals, rtx_wide, "rtx")
  df <- drop_all_na_columns(df)

  # max_days = 60, as in the notebook's update_rtx_doses default
  combine_plan <- list(
    c(1,2,60), c(1,3,60), c(1,4,60), c(1,5,60),
    c(2,3,60), c(2,4,60), c(2,5,60),
    c(3,4,60), c(3,5,60), c(3,6,60),
    c(4,5,60),
    c(5,6,60), c(5,7,60), c(5,8,60),
    c(7,8,60), c(7,9,60), c(7,10,60),
    c(8,9,60),
    c(9,10,60), c(9,11,60), c(9,12,60),
    c(10,11,60), c(10,12,60), c(10,13,60),
    c(11,12,60), c(11,13,60), c(11,14,60),
    c(12,13,60),
    c(13,14,60), c(13,15,60), c(13,16,60),
    c(15,16,60),
    c(22,23,60),
    c(24,25,60),
    c(27,28,60),
    c(29,30,60),
    c(31,32,60),
    c(33,34,60)
  )
  df <- apply_combine_plan(df, "rtx", combine_plan)
  df <- drop_all_na_columns(df)

  df <- apply_rename_plan(df, "rtx", list(c(24,23), c(26,24), c(27,25), c(29,26), c(31,27), c(33,28)))
  df <- reorder_iv_columns(df, "rtx")

  doses <- 1:28
  df <- compute_days_since_dose(df, "rtx", doses)

  sp <- build_sigmoid_params(A1 = 0.70, A2 = 0.85, dose1 = 500, dose2 = 2000,
                              d1 = 160, d2 = 200, vanish1 = 240, vanish2 = 300)
  df <- compute_dose_ITIS_scores(df, "rtx", doses, clip_lower = 500, clip_upper = 2000, sp)

  # dose 8 is excluded from the cumulative combination in the notebook
  itis_cols <- paste0("rtx_dose", setdiff(doses, 8), "_ISI_score")
  df <- cumulative_ITIS(df, itis_cols, "RTX_IV_ISI_score")
  df$RTX_IV_ISI_score[is.na(df$RTX_IV_ISI_score)] <- 0
  df
}

## ---- 4.4 Oral Cyclophosphamide --------------------------------------------
#' @title run_cyc_oral_pipeline
#' @param df dataframe with all the previous ISI score calculated
#' @param conmed Continuous medication frame
run_cyc_oral_pipeline <- function(df, conmed) {
  df <- merge_continuous_medication(df, conmed, "Cyclophosphamide - UATC/L01AA01", "cyc_oral")

  df$Days_Difference <- as.numeric(df$Date_Of_Visit - df$cyc_oral_Start.Date)
  # "Dose_Cyclophosphamide - UATC/L01AA01" must already exist upstream (daily dose, mg)
  df$cyc_oral_course <- df[["Dose_Cyclophosphamide - UATC/L01AA01"]] * df$Days_Difference
  df$cyc_oral_course[df$cyc_oral_course > 25000 & !is.na(df$cyc_oral_course)] <- 25000

  df <- df %>%
    arrange(RKD.ID, Date_Of_Visit) %>%
    group_by(RKD.ID) %>%
    mutate(cyc_oral_Stop.Date = zoo::na.locf(cyc_oral_Stop.Date, na.rm = FALSE),
           cyc_oral_course    = zoo::na.locf(cyc_oral_course,    na.rm = FALSE)) %>%
    ungroup()

  df$interval_from_oral_cyc <- as.numeric(df$Date_Of_Visit - df$cyc_oral_Stop.Date)
  df$interval_from_oral_cyc[df$interval_from_oral_cyc < 0] <- 0

  sp <- build_sigmoid_params(A1 = 0.60, A2 = 0.90, dose1 = 75, dose2 = 25000,
                              d1 = 70, d2 = 90, vanish1 = 90, vanish2 = 115)
  A <- sp$calculate_A(df$cyc_oral_course)
  d <- sp$calculate_d(df$cyc_oral_course)
  n <- sp$calculate_n(df$cyc_oral_course)
  df$CYC_Oral_ISI_score <- sigmoid_curve(df$interval_from_oral_cyc, A, n, d)
  df$CYC_Oral_ISI_score[is.na(df$cyc_oral_course) | is.na(df$interval_from_oral_cyc)] <- NA_real_
  df$CYC_Oral_ISI_score[is.na(df$CYC_Oral_ISI_score)] <- 0
  df
}

## ---- 4.5 Prednisolone (categorical) ---------------------------------------
#' @title run_prednisolone_pipeline
#' @param df dataframe with all the previous ISI score calculated

run_prednisolone_pipeline <- function(df) {
  itis_map <- c(
    "< 5 mg/day"    = 0.10,
    "5 - 10 mg/day" = 0.25,
    "11 - 20 mg/day"= 0.45,
    "> 20 mg/day"   = 0.75
  )
  df$Prednisolone_ISI_score <- 0
  mask <- !is.na(df$Current.corticosteroid.dose)
  df$Prednisolone_ISI_score[mask] <- unname(itis_map[df$Current.corticosteroid.dose[mask]])

  # if a high-enough IV methylprednisolone dose was given, oral steroid
  # contribution is zeroed out for that visit
  df$Prednisolone_ISI_score[df$MTP_IV_ISI_score > 0.25] <- 0
  df$Prednisolone_ISI_score[is.na(df$Prednisolone_ISI_score)] <- 0
  df
}

## ---- 4.6 Azathioprine ------------------------------------------------------
#' @title run_aza_pipeline
#' @param df dataframe with all the previous ISI score calculated
#' @param conmed Continuous medication frame
run_aza_pipeline <- function(df, conmed) {
  df <- merge_continuous_medication(df, conmed, "Azathioprine - UATC/L04AX01", "aza")

  dosecol <- "Dose_Azathioprine - UATC/L04AX01"
  df[[dosecol]][df[[dosecol]] < 25]  <- 25
  df[[dosecol]][df[[dosecol]] > 250] <- 250

  df <- df %>% arrange(RKD.ID, Date_Of_Visit) %>%
    mutate(aza_Dose_ffill = zoo::na.locf(.data[[dosecol]], na.rm = FALSE))

  df$Aza_ISI_score <- linear_dose_score(df[[dosecol]], min_dose = 25, max_dose = 250,
                                         min_score = 0.15, max_score = 0.60)

  sp <- build_sigmoid_params(A1 = 0.15, A2 = 0.60, dose1 = 25, dose2 = 250,
                              d1 = 8, d2 = 10, vanish1 = 10, vanish2 = 14)
  A <- sp$calculate_A(df$aza_Dose_ffill); d <- sp$calculate_d(df$aza_Dose_ffill); n <- sp$calculate_n(df$aza_Dose_ffill)
  score2 <- sigmoid_curve(df$interval_from_aza_stop_date, A, n, d)
  score2[is.na(df$aza_Dose_ffill) | is.na(df$interval_from_aza_stop_date)] <- NA_real_

  df$Aza_ISI_score <- ifelse(is.na(df$Aza_ISI_score), score2, df$Aza_ISI_score)
  df$Aza_ISI_score[is.na(df$Aza_ISI_score)] <- 0

  df$aza_Stop.Date_ffill <- NULL
  df$interval_from_aza_stop_date <- NULL
  df$aza_Dose_ffill <- NULL
  df
}

## ---- 4.7 Mycophenolate Mofetil (MMF) --------------------------------------
#' @title run_mmf_pipeline
#' @param df dataframe with all the previous ISI score calculated
#' @param conmed Continuous medication frame
run_mmf_pipeline <- function(df, conmed) {
  df <- merge_continuous_medication(df, conmed, "Mycophenolate mofetil - UATC/L04AA06", "mmf")

  dosecol <- "Dose_Mycophenolate mofetil - UATC/L04AA06"
  df[[dosecol]][df[[dosecol]] < 125] <- 125

  df$MMF_ISI_score <- linear_dose_score(df[[dosecol]], min_dose = 125, max_dose = 4000,
                                         min_score = 0.25, max_score = 0.75)

  df <- df %>% arrange(RKD.ID, Date_Of_Visit) %>%
    mutate(mmf_Dose_ffill = zoo::na.locf(.data[[dosecol]], na.rm = FALSE))

  sp <- build_sigmoid_params(A1 = 0.25, A2 = 0.75, dose1 = 125, dose2 = 4000,
                              d1 = 5, d2 = 7, vanish1 = 7, vanish2 = 9)
  A <- sp$calculate_A(df$mmf_Dose_ffill); d <- sp$calculate_d(df$mmf_Dose_ffill); n <- sp$calculate_n(df$mmf_Dose_ffill)
  score2 <- sigmoid_curve(df$interval_from_mmf_stop_date, A, n, d)
  score2[is.na(df$mmf_Dose_ffill) | is.na(df$interval_from_mmf_stop_date)] <- NA_real_

  df$MMF_ISI_score <- ifelse(is.na(df$MMF_ISI_score), score2, df$MMF_ISI_score)
  df$MMF_ISI_score[is.na(df$MMF_ISI_score)] <- 0
  df
}

## ---- 4.8 Methotrexate ------------------------------------------------------
#' @title run_methotrexate_pipeline
#' @param df dataframe with all the previous ISI score calculated
#' @param conmed Continuous medication frame
run_methotrexate_pipeline <- function(df,conmed) {
  # "Dose_Methotrexate - UATC/L01BA01" and a stop-date column
  # "meth_Stop.Date" are assumed to already exist upstream, as in the notebook.
  df <- merge_continuous_medication(df, conmed, "Methotrexate - UATC/L01BA01", "meth")
  
  dosecol <- "Dose_Methotrexate - UATC/L01BA01"
  df[[dosecol]][df[[dosecol]] < 0.35] <- 0.35
  df[[dosecol]][df[[dosecol]] > 5]    <- 5
  
  

  df <- df %>% arrange(RKD.ID, Date_Of_Visit) %>%
    group_by(RKD.ID) %>%
    mutate(meth_Stop.Date_ffill = zoo::na.locf(meth_Stop.Date, na.rm = FALSE)) %>%
    ungroup()
  df$interval_from_meth_stop_date <- as.numeric(df$Date_Of_Visit - df$meth_Stop.Date_ffill)

  df <- df %>% arrange(RKD.ID, Date_Of_Visit) %>%
    mutate(meth_Dose_ffill = zoo::na.locf(.data[[dosecol]], na.rm = FALSE))

  df$Methotrexate_ISI_score <- linear_dose_score(df[[dosecol]], min_dose = 0.35, max_dose = 5,
                                                  min_score = 0.10, max_score = 0.55)

  sp <- build_sigmoid_params(A1 = 0.10, A2 = 0.55, dose1 = 0.35, dose2 = 5,
                              d1 = 8, d2 = 10, vanish1 = 10, vanish2 = 14)
  A <- sp$calculate_A(df$meth_Dose_ffill); d <- sp$calculate_d(df$meth_Dose_ffill); n <- sp$calculate_n(df$meth_Dose_ffill)
  score2 <- sigmoid_curve(df$interval_from_meth_stop_date, A, n, d)
  score2[is.na(df$meth_Dose_ffill) | is.na(df$interval_from_meth_stop_date)] <- NA_real_

  df$Methotrexate_ISI_score <- ifelse(is.na(df$Methotrexate_ISI_score), score2, df$Methotrexate_ISI_score)
  df$Methotrexate_ISI_score[is.na(df$Methotrexate_ISI_score)] <- 0

  df$meth_Stop.Date_ffill <- NULL
  df$interval_from_meth_stop_date <- NULL
  df$meth_Dose_ffill <- NULL
  df
}

## ---- 4.9 Avacopan -----------------------------------------------------------
#' @title run_avacopan_pipeline
#' @param df dataframe with all the previous ISI score calculated
run_avacopan_pipeline <- function(df) {
  df$Avacopan_ISI_score <- ifelse(df[["Drug_Avacopan (C5aR inhibitor)"]] == "Avacopan (C5aR inhibitor)", 0.50, 0)
  df$Avacopan_ISI_score[is.na(df$Avacopan_ISI_score)] <- 0
  df
}

# -----------------------------------------------------------------------------
# 5. MASTER ORCHESTRATION FUNCTION
# -----------------------------------------------------------------------------

#' Run the full ISI-score pipeline.
#'
#' @param data1  visit-level clinical data (one row per RKD.ID + Date_Of_Visit),
#'   must already contain: Current.corticosteroid.dose, "Dose_Cyclophosphamide - UATC/L01AA01",
#'   "Dose_Azathioprine - UATC/L04AX01", "Dose_Mycophenolate mofetil - UATC/L04AA06",
#'   "Dose_Methotrexate - UATC/L01BA01", meth_Stop.Date, "Drug_Avacopan (C5aR inhibitor)"
#'   (these are produced by an earlier data-cleaning stage not included in this notebook).
#' @param IV     long-format IV-therapy extract (RKD.ID, IV.therapy, Date.of.IV.therapy,
#'   Dose.of.IV.therapy, Unit.of.dose, ...)
#' @param conmed long-format continuous-medication extract (RKD.ID, Drug, Start.Date,
#'   Stop.Date, Dose, Unit.of.Doses, Frequency, On.going, ...)
#' @param output_csv Folder where the file is saved
#'
#' @return data1 with one row per visit and a final "Cumulative_ISI_score_base" column
#'   (plus every intermediate *_ISI_score component, for auditing).
#' 
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import zoo
#' 
compute_cumulative_isi_score <- function(data1, IV, conmed, output_csv) {

  data1 <- data1 %>% rename(Date_Of_Visit = any_of(c("Date.Of.Visit", "Date_Of_Visit")))
  IV <- IV %>%
    rename(RKD.ID = any_of("RKD ID"),
           IV.therapy = any_of("IV therapy"),
           Date.of.IV.therapy = any_of("Date of IV therapy"),
           Dose.of.IV.therapy = any_of("Dose of IV therapy")) %>%
    mutate(Date.of.IV.therapy = as.Date(Date.of.IV.therapy)) %>%
    arrange(RKD.ID, Date.of.IV.therapy) %>%
    distinct(RKD.ID, Date.of.IV.therapy, IV.therapy, .keep_all = TRUE) %>%
    select(RKD.ID, IV.therapy, Date.of.IV.therapy, Dose.of.IV.therapy,
           Unit.of.dose, any_of("Unit.of.dose...other"))

  conmed <- conmed %>%
    distinct(RKD.ID, Drug, Start.Date, Dose, .keep_all = TRUE) %>%
    mutate(Start.Date = as.Date(Start.Date), Stop.Date = as.Date(Stop.Date))

  df <- run_mtp_pipeline(data1, IV)
  df <- run_cyc_iv_pipeline(df, IV)
  df <- run_rtx_pipeline(df, IV)
  df <- run_cyc_oral_pipeline(df, conmed)
  df <- run_prednisolone_pipeline(df)
  df <- run_aza_pipeline(df, conmed)
  df <- run_mmf_pipeline(df, conmed)
  df <- run_methotrexate_pipeline(df,conmed)
  df <- run_avacopan_pipeline(df)

  df <- cumulative_ITIS(df, c("CYC_Oral_ISI_score", "CYC_IV_ISI_score"), "Cumulative__CYC_ISI_score")

  final_cols <- c("MTP_IV_ISI_score", "RTX_IV_ISI_score", "Cumulative__CYC_ISI_score",
                  "Prednisolone_ISI_score", "Aza_ISI_score", "MMF_ISI_score",
                  "Methotrexate_ISI_score", "Avacopan_ISI_score")
  df <- cumulative_ITIS(df, final_cols, "Cumulative_ISI_score_base")
  df$Cumulative_ISI_score_base_round_2 <- round(df$Cumulative_ISI_score_base, 2)

  df
  
  if (!is.null(output_csv)) {
    output_filename <- file.path(
      output_csv,
      paste0('Redcap_clinical_ISI_score', "_version", packageVersion('rivpipeline'), "_Date"
             , Sys.Date(), '.csv')
    )
    write.csv(df, output_filename, row.names = FALSE)
    message("Results saved to: ", output_filename)
  }
}

# -----------------------------------------------------------------------------
# Example usage:
#
# data1  <- read.csv("Redcap_clinical_data_harmonized.csv")
# IV     <- read.csv("Redcap_IVTherapy_data_merged.csv")
# conmed <- read.csv("Redcap_countinuous_medication_data_merged.csv")
#
# result <- compute_cumulative_isi_score(data1, IV, conmed)
# write.csv(result, "Cumulative_ISI_score_base.csv", row.names = FALSE)
# -----------------------------------------------------------------------------
