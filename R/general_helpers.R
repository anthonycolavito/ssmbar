join_all_assumptions <- function(worker, assumptions) {
  # All columns needed by the benefit calculation pipeline
  # - aime: awi, taxmax, qc_rec, qc_required, max_qc_per_year, max_dropout_years, min_comp_period, index_age_offset
  # - pia: bp1, bp2, fact1, fact2, fact3, elig_age_retired, yoc_threshold, special_min_rate, min_yoc_for_special_min
  # - cola: cola (year-by-year COLA percentage)
  # - worker_benefit: nra, rf1, rf2, drc, max_drc_age
  # - spousal_pia: s_pia_share
  # - spouse_benefit: s_rf1, s_rf2
  # - child_pia: child_pia_share
  # - family_maximum: fm_bp1, fm_bp2, fm_bp3
  # - ret: ret1, ret_phaseout_rate
  
  cols_needed <- c("year", "awi", "taxmax", "qc_rec", "qc_required", "max_qc_per_year",
                   "max_dropout_years", "min_comp_period", "index_age_offset",
                   "bp1", "bp2", "fact1", "fact2", "fact3", "eea",
                   "yoc_threshold", "special_min_rate", "min_yoc_for_special_min",
                   "cola", "nra", "rf1", "rf2", "drc", "max_drc_age",
                   "s_pia_share", "s_rf1", "s_rf2",
                   "ret1", "ret_phaseout_rate", "oasi_tr","di_tr")
  
  # Only join columns that aren't already present
  cols_present <- names(worker)
  cols_to_join <- cols_needed[!cols_needed %in% cols_present | cols_needed == "year"]
  
  if (length(cols_to_join) > 1) {  # More than just 'year'
    worker <- worker %>%
      left_join(assumptions %>% select(all_of(cols_to_join)), by = "year")
  }
  
  return(worker)
}

floor_dime <- function(x) {
  floor(x * 10) / 10
}

remove_all_assumptions <- function(worker) {
  # All columns needed by the benefit calculation pipeline
  # - aime: awi, taxmax, qc_rec, qc_required, max_qc_per_year, max_dropout_years, min_comp_period, index_age_offset
  # - pia: bp1, bp2, fact1, fact2, fact3, elig_age_retired, yoc_threshold, special_min_rate, min_yoc_for_special_min
  # - cola: cola (year-by-year COLA percentage)
  # - worker_benefit: nra, rf1, rf2, drc, max_drc_age
  # - spousal_pia: s_pia_share
  # - spouse_benefit: s_rf1, s_rf2
  # - child_pia: child_pia_share
  # - family_maximum: fm_bp1, fm_bp2, fm_bp3
  # - ret: ret1, ret_phaseout_rate
  
  discard_cols <- c("awi", "taxmax", "qc_rec", "qc_required", "max_qc_per_year",
                   "max_dropout_years", "min_comp_period", "index_age_offset",
                   "bp1", "bp2", "fact1", "fact2", "fact3", "eea",
                   "yoc_threshold", "special_min_rate", "min_yoc_for_special_min",
                   "cola", "nra", "rf1", "rf2", "drc", "max_drc_age",
                   "s_pia_share", "s_rf1", "s_rf2",
                   "ret1", "ret_phaseout_rate","oasi_tr","di_tr")
  
  if (length(discard_cols) > 0) {  
    worker <- worker %>% select(-all_of(discard_cols))
  }
  
  return(worker)
}


