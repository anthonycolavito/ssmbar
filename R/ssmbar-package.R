#' @keywords internal
"_PACKAGE"

# Global variable declarations to avoid R CMD check NOTEs
# These are column names used in dplyr NSE (non-standard evaluation)
utils::globalVariables(c(

  # Common variables
  "id", "age", "year", "sex", "earnings", "claim_age", "elig_age",
  "spouse_spec", "ben", "disabled_age", "is_disabled",


  # Assumptions variables (including parameterized program rules)
  "awi", "cpi_w", "gdp_pi", "taxmax", "qc_rec",
  "bp1", "bp2", "fact1", "fact2", "fact3",
  "nra", "rf1", "rf2", "drc",
  "s_pia_share", "s_rf1", "s_rf2",
  "qc_required", "elig_age_retired", "index_age_offset",
  "max_dropout_years", "min_comp_period", "max_qc_per_year",
  "max_drc_age", "ret_phaseout_rate",

  # Reform parameters
  "pia_multiplier", "ret_enabled", "bp3", "fact4", "cola_cap", "cola_cap_active",
  "bmb_individual", "bmb_couple", "mini_pia_blend", "flat_benefit",
  "taxmax_tax", "taxmax_benefit",
  "bp3_elig", "fact4_elig", "cola_cap_year", "cola_cap_active_vals", "cap_is_active",
  "bmb_rate", "bmb_rate_year", "bmb_rate_elig", "bmb_cola_factor", "bmb_start_year",
  "flat_benefit_elig", "mini_pia_val", "regular_pia", "blended_pia",
  "pia_mult_elig", "regular_pia_mult", "regular_pia_final",
  "benefit_cap", "nra_bmb", "at_or_past_nra", "has_spouse", "bmb_supplement",
  "child_care_credit_active", "years_with_child_under_6",
  "max_child_care_years", "child_care_earnings_rate", "child_care_floor",
  "earnings_with_cc", "mini_pia_blend_elig",
  "widow_75_pct_active", "survivor_ben_current_law", "survivor_ben_alternative",
  "own_wrk_ben", "widow_75_active",

  # AIME calculation variables
  "awi_age60", "awi_index_age", "index_age", "index_factor", "capped_earn", "indexed_earn",
  "qc_i", "qc_tot", "comp_period", "elapsed_years", "dropout_years",

  # PIA calculation variables
  "basic_pia", "bp1_age62", "bp2_age62", "bp1_elig", "bp2_elig",
  "fact1_age62", "fact2_age62", "fact3_age62", "fact1_elig", "fact2_elig", "fact3_elig",
  "elig_age_ret",

  # COLA calculation variables
  "cpi_age62", "cpi_elig", "cpi_index_factor", "cola_basic_pia", "cola_spouse_pia",

  # Spousal PIA variables
  "s_pia", "s_pia_share_ind", "spouse_pia",

  # Benefit calculation variables
  "yr_62", "yr_elig", "nra_ind", "rf1_ind", "rf2_ind", "drc_ind",
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
  "months_withheld", "cum_months_withheld", "ret_adj_factor", "ret_s_adj_factor",
  "cum_months_withheld_final", "wrk_ben_orig", "spouse_ben_orig",
  "ret_rate", "drc_max",
  # Spouse's RET effect on worker's spouse_ben
  "s_excess_earnings", "s_ret_reduction", "s_own_ben", "s_earnings",
  "spouse_ret_to_spouse_ben", "s_months_withheld", "s_cum_months_withheld",
  "s_cum_months_withheld_final", "s_nra_ind", "s_drc_payback_factor",

  # Survivor (widow/widower) benefit variables
  "death_age", "s_death_age", "s_wrk_ben", "worker_age_at_spouse_death",
  "survivor_pia", "prelim_survivor_pia", "survivor_ben", "survivor_ben_adj",
  "effective_widow_claim_age", "actual_widow_claim_age", "w_elig_age_ind", "w_rf", "w_act_factor",
  "spouse_ben_adj", "spouse_frac_alive", "is_spouse_death_year", "bc",
  "surv_s_age", "surv_s_pia", "surv_s_wrk_ben", "surv_s_death_age", "surv_s_claim_age",

  # Disabled widow(er) benefit variables
  "is_disabled_widow", "disabled_widow_claim_age", "benefit_start_age",
  "is_originally_disabled", "is_currently_disabled",

  # Spouse disability status variables (for BD/BR benefit class determination)
  "s_elig_age", "s_is_originally_disabled", "s_is_currently_disabled", "s_yr_62",

  # Tax calculation variables
  "oasi_tr", "di_tr", "ss_taxable_earn", "oasi_tax", "di_tax", "ss_tax",
  "tax_amount", "ss_tax_total",

  # Marginal analysis variables (cumulative stopping-point method)
  "comp_period_val", "working_year",
  "years_worked", "qcs", "eligible", "cumulative_aime", "cumulative_pia",
  "cumulative_pv", "delta_pv_benefits", "aime_at_claim", "bp1_val", "bp2_val",
  "claim_age_val", "death_age_val", "benefit_months", "discount_year",
  "real_df_norm", "real_df_at_claim", "gdp_pi_at_claim", "act_factor_val",
  "net_marginal_tax_rate", "marginal_irr", "delta_pv_benefits_total", "ss_tax_total",

  # Custom earnings variables
  "pi_curr", "index", "nom_earn", "real_earn", "adj_real_earn",

  # Worker builder variables (if used)
  "dataset", ".data",

  # Constants (defined in earnings.R)
  "FIRST_WORKING_AGE", "MAX_AGE",

  # Reform analysis variables
  "annual_nominal", "annual_real", "baseline_nominal", "baseline_real",
  "annual_ind", "rep_rate"
))

#' @importFrom dplyr %>% mutate select filter left_join group_by ungroup arrange
#' @importFrom dplyr case_when if_else first row_number group_modify bind_rows rename all_of summarise any_of n
#' @importFrom tidyr pivot_longer
#' @importFrom stats na.omit uniroot
NULL
