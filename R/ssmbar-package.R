#' @keywords internal
"_PACKAGE"

# Global variable declarations to avoid R CMD check NOTEs
# These are column names used in dplyr NSE (non-standard evaluation)
utils::globalVariables(c(

  # Common variables
  "id", "age", "year", "sex", "earnings", "claim_age", "elig_age",
  "spouse_spec", "ben",


  # Assumptions variables
  "awi", "cpi_w", "gdp_pi", "taxmax", "qc_rec",
  "bp1", "bp2", "fact1", "fact2", "fact3",
  "nra", "rf1", "rf2", "drc",
  "s_pia_share", "s_rf1", "s_rf2",

  # AIME calculation variables
  "awi_age60", "index_factor", "capped_earn", "indexed_earn",
  "qc_i", "qc_tot", "comp_period", "elapsed_years", "dropout_years",

  # PIA calculation variables
  "basic_pia", "bp1_age62", "bp2_age62",
  "fact1_age62", "fact2_age62", "fact3_age62",

  # COLA calculation variables
  "cpi_age62", "cpi_index_factor", "cola_basic_pia", "cola_spouse_pia",

  # Spousal PIA variables
  "s_pia", "s_pia_share_ind", "spouse_pia",

  # Benefit calculation variables
  "yr_62", "nra_ind", "rf1_ind", "rf2_ind", "drc_ind",
  "act_factor", "wrk_ben",
  "s_rf1_ind", "s_rf2_ind", "s_act_factor", "spouse_ben",
  "s_age", "s_claim_age",

  # RET (Retirement Earnings Test) variables
  "ret1", "ret2", "spouse_dep_ben", "spouse_dep_pia", "cola_spouse_dep_pia",
  "s_dep_act_factor", "s_own_pia", "s_claim_age_val", "yr_s_claim", "cpi_factor",
  "excess_earnings", "ret_reduction", "ret_reduction_capped",
  "wrk_total_ben", "total_ben_pot", "annual_ben_pot",
  "wrk_share", "wrk_reduction", "wrk_ben_share", "spouse_ben_share",
  "wrk_ben_reduced", "spouse_ben_reduced", "wrk_ben_final", "spouse_ben_final",
  "months_withheld", "cum_months_withheld", "ret_adj_factor",
  "cum_months_withheld_final", "wrk_ben_orig", "spouse_ben_orig",

  # Custom earnings variables
  "pi_curr", "index", "nom_earn", "real_earn", "adj_real_earn",

  # Worker builder variables (if used)
  "dataset", ".data"
))

#' @importFrom dplyr %>% mutate select filter left_join group_by ungroup arrange
#' @importFrom dplyr case_when if_else first row_number group_modify bind_rows rename all_of
#' @importFrom stats na.omit
NULL
