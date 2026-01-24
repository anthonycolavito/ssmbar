
# =============================================================================
# RETIREMENT EARNINGS TEST (RET) CALCULATIONS
# =============================================================================
#
# This file contains the Retirement Earnings Test function and its helpers.
# RET reduces benefits for workers who have earnings above the exempt amount
# between their claiming age and Normal Retirement Age.
#
# Reference: SSA Handbook Chapter 18
# https://www.ssa.gov/OP_Home/handbook/handbook.18/handbook-toc18.html
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Helper Functions
# -----------------------------------------------------------------------------

#' Calculate Excess Earnings Above RET Threshold (Internal)
#'
#' Calculates excess earnings above the RET exempt amount for years between
#' claim age and NRA. Returns 0 outside this window.
#'
#' @param earnings Numeric vector of annual earnings
#' @param ret_threshold Numeric vector of RET exempt amounts (ret1)
#' @param age Numeric vector of ages
#' @param claim_age Numeric scalar of claiming age
#' @param nra Numeric scalar of Normal Retirement Age
#' @return Numeric vector of excess earnings
#' @keywords internal

calculate_excess_earnings <- function(earnings, ret_threshold, age, claim_age, nra) {
  # RET only applies between claim_age and NRA (Section 1803)
  case_when(
    age < claim_age | age >= nra ~ 0,
    TRUE ~ pmax(earnings - ret_threshold, 0)
  )
}


#' Calculate RET Reduction Amount (Internal)
#'
#' Calculates the benefit reduction based on excess earnings and phaseout rate,
#' capped at total annual benefits.
#'
#' @param excess_earnings Numeric vector of excess earnings
#' @param phaseout_rate Numeric scalar (currently 0.5 = $1 per $2 excess)
#' @param total_monthly_benefits Numeric vector of total monthly benefit pot
#' @return Numeric vector of annual reduction amounts (capped)
#' @keywords internal

calculate_ret_reduction <- function(excess_earnings, phaseout_rate, total_monthly_benefits) {
  # Reduction is phaseout_rate per dollar of excess earnings (Section 1803)
  raw_reduction <- excess_earnings * phaseout_rate

  # Cap at total annual benefits.
  annual_benefits <- total_monthly_benefits * 12
  pmin(raw_reduction, annual_benefits)
}


#' Allocate RET Reduction Between Benefits (Internal)
#'
#' Allocates the total reduction proportionally between worker's own benefit,
#' worker's spousal benefit, and spouse's dependent benefit.
#'
#' @param total_reduction Numeric vector of total annual reduction
#' @param wrk_ben Numeric vector of worker's own monthly benefit
#' @param spouse_ben Numeric vector of worker's spousal monthly benefit
#' @param spouse_dep_ben Numeric vector of spouse's dependent monthly benefit
#' @return List with wrk_ben_reduced and spouse_ben_reduced (monthly)
#' @keywords internal

allocate_ret_reduction <- function(total_reduction, wrk_ben, spouse_ben, spouse_dep_ben) {
  # Section 1804: How Excess Earnings are Charged Against Benefits
  # https://www.ssa.gov/OP_Home/handbook/handbook.18/handbook-1804.html
  # Section 1806: Payment of Partial Benefit
  # https://www.ssa.gov/OP_Home/handbook/handbook.18/handbook-1806.html
  # Reductions are allocated proportionally to beneficiaries' "original entitlement rate."
  # That is, the share of the retired worker's benefits without any adjustments for
  # claiming timing or partial benefits received (dual eligibles)
  # For example, a spouse receiving a dependent spousal benefit would be allocated
  # 1/3 of the reduction since their PIA is 50% of the retired worker's (100% + 50% = 150%)


  # Total benefit pot
  wrk_total_ben <- wrk_ben + spouse_ben
  total_ben_pot <- wrk_total_ben + spouse_dep_ben

  # Worker's share of reduction (proportional to their benefits vs total pot)
  wrk_share <- if_else(total_ben_pot > 0, wrk_total_ben / total_ben_pot, 1)
  wrk_reduction <- total_reduction * wrk_share

  # Allocate worker's reduction between wrk_ben and spouse_ben proportionally
  wrk_ben_share <- if_else(wrk_total_ben > 0, wrk_ben / wrk_total_ben, 1)
  spouse_ben_share <- if_else(wrk_total_ben > 0, spouse_ben / wrk_total_ben, 0)

  # Reduce monthly benefits
  wrk_ben_reduced <- pmax(wrk_ben - (wrk_reduction * wrk_ben_share / 12), 0)
  spouse_ben_reduced <- pmax(spouse_ben - (wrk_reduction * spouse_ben_share / 12), 0)

  list(
    wrk_ben_reduced = wrk_ben_reduced,
    spouse_ben_reduced = spouse_ben_reduced,
    wrk_share = wrk_share,
    wrk_reduction = wrk_reduction
  )
}


#' Calculate Months of Benefits Withheld (Internal)
#'
#' Calculates the number of months of benefits withheld due to RET.
#' Used for DRC payback calculation at NRA.
#'
#' @param annual_reduction Numeric vector of annual reduction amounts
#' @param monthly_benefit Numeric vector of total monthly benefits
#' @param age Numeric vector of ages
#' @param claim_age Numeric scalar of claiming age
#' @param nra Numeric scalar of Normal Retirement Age
#' @return Numeric vector of months withheld (max 12 per year)
#' @keywords internal

calculate_months_withheld <- function(annual_reduction, monthly_benefit, age, claim_age, nra) {
  # months_withheld = annual_reduction / monthly_benefit (Section 1806)
  # TODO: Verify the rounding rule (ceiling to nearest tenth) in SSA Handbook
  # Section 1806 or POMS. The current implementation rounds up to nearest 0.1 month.
  # Capped at 12 months
  if_else(
    monthly_benefit > 0 & age >= claim_age & age < nra,
    ceiling(pmin(annual_reduction / monthly_benefit, 12) * 10) / 10,
    0
  )
}


#' Calculate DRC Payback Actuarial Factors (Internal)
#'
#' At NRA, recalculates actuarial factors to account for months of benefits
#' withheld, effectively treating the worker as if they claimed later.
#'
#' @param claim_age Numeric scalar of original claiming age
#' @param cum_months_withheld Numeric scalar of total months withheld before NRA
#' @param nra Numeric scalar of Normal Retirement Age
#' @param rf1 Numeric scalar of first reduction factor
#' @param rf2 Numeric scalar of second reduction factor
#' @param drc Numeric scalar of delayed retirement credit rate
#' @param s_rf1 Numeric scalar of spousal first reduction factor
#' @param s_rf2 Numeric scalar of spousal second reduction factor
#' @param drc_max_months Numeric scalar of maximum DRC months
#' @return List with new_act_factor, new_s_act_factor, orig_act_factor, orig_s_act_factor
#' @keywords internal

calculate_drc_payback <- function(claim_age, cum_months_withheld, nra, rf1, rf2, drc, s_rf1, s_rf2, drc_max_months) {
  # Effective claim age is adjusted by months withheld (Section 1806)
  # TODO: Add SSA Handbook citation explaining that spousal actuarial factor
  # is also recalculated at NRA based on withheld months. Verify whether this
  # is in Section 1806 or identify correct section.
  effective_claim_age <- min(claim_age + (cum_months_withheld / 12), nra)

  # Calculate new actuarial factors with effective claim age

  new_act_factor <- rf_and_drc(effective_claim_age, nra, rf1, rf2, drc, drc_max_months)
  new_s_act_factor <- rf_and_drc(effective_claim_age, nra, s_rf1, s_rf2, 0, drc_max_months)

  # Original factors for comparison
  orig_act_factor <- rf_and_drc(claim_age, nra, rf1, rf2, drc, drc_max_months)
  orig_s_act_factor <- rf_and_drc(claim_age, nra, s_rf1, s_rf2, 0, drc_max_months)

  list(
    new_act_factor = new_act_factor,
    new_s_act_factor = new_s_act_factor,
    orig_act_factor = orig_act_factor,
    orig_s_act_factor = orig_s_act_factor,
    effective_claim_age = effective_claim_age
  )
}


# -----------------------------------------------------------------------------
# 2. Main RET Function
# -----------------------------------------------------------------------------

#' Retirement Earnings Test Calculation
#'
#' Function that reduces an individual's benefits if their earnings exceed the
#' exempt amounts in the Retirement Earnings Test. Also calculates DRC payback
#' at NRA to account for months of benefits withheld.
#'
#' The RET reduces benefits for workers who have earnings above the exempt amount
#' (ret1) in years between their claiming age and Normal Retirement Age.
#' For every $2 of excess earnings, $1 of benefits is withheld.
#'
#' If the worker has a spouse claiming benefits based on their record, the total
#' benefit pot (worker's benefits + spouse's dependent benefit) is reduced, with
#' the reduction allocated proportionally.
#'
#' At NRA, the actuarial factor is recalculated to account for months of benefits
#' withheld, effectively treating the worker as if they claimed later.
#'
#' @param worker Dataframe with a worker's benefits by year and age
#' @param assumptions Dataframe with the Social Security Trustees assumptions
#' @param spouse_data List of spouse data frames (keyed by spouse_spec), where each contains
#'   year, s_age, s_birth_yr, s_claim_age, s_pia. NULL if no spouses or to generate on-the-fly.
#' @param factors Data frame for the Trustees' scaled earnings factors. Required
#'   if spouse_data is NULL and workers have spouse_spec for on-the-fly generation.
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with RET-adjusted benefits
#'
#' @export
ret <- function(worker, assumptions, spouse_data = NULL, factors = NULL, debugg = FALSE) {
  # RET is described in Chapter 18 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.18/handbook-toc18.html

  # Generate spouse_data on-the-fly if needed
  has_spouse_spec <- "spouse_spec" %in% names(worker) && any(!is.na(worker$spouse_spec))
  if (is.null(spouse_data) && has_spouse_spec) {
    if (is.null(factors)) {
      stop("factors parameter is required when spouse_data is NULL and workers have spouse_spec")
    }
    unique_specs <- unique(worker$spouse_spec[!is.na(worker$spouse_spec)])
    spouse_data <- lapply(unique_specs, function(spec) generate_spouse(spec, factors, assumptions))
    names(spouse_data) <- unique_specs
  }

  # Join required assumption columns
  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("ret1", "nra", "rf1", "rf2", "drc", "s_rf1", "s_rf2",
                   "ret_phaseout_rate", "elig_age_retired", "drc_max_months")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>%
      left_join(assumptions %>% select(all_of(cols_to_join)), by = "year")
  } else {
    dataset <- worker
  }

  # Process each worker

  dataset <- dataset %>%
    group_by(id) %>%
    arrange(id, age) %>%
    group_modify(~ {
      wd <- .x  # worker_data
      spec <- wd$spouse_spec[1]
      elig_age_ret <- wd$elig_age_retired[1]
      ret_rate <- wd$ret_phaseout_rate[1]
      drc_max <- wd$drc_max_months[1]
      claim_age_val <- wd$claim_age[1]

      # Get birth-cohort parameters at eligibility age
      yr_elig <- wd$year[1] - wd$age[1] + elig_age_ret
      nra_ind <- wd$nra[wd$year == yr_elig][1]
      rf1_ind <- wd$rf1[wd$year == yr_elig][1]
      rf2_ind <- wd$rf2[wd$year == yr_elig][1]
      drc_ind <- wd$drc[wd$year == yr_elig][1]
      s_rf1_ind <- wd$s_rf1[wd$year == yr_elig][1]
      s_rf2_ind <- wd$s_rf2[wd$year == yr_elig][1]

      # Calculate spouse's dependent benefit
      wd$spouse_dep_ben <- if (!is.na(spec) && !is.null(spouse_data[[spec]])) {
        calculate_spouse_dep_benefit(wd, spouse_data[[spec]], assumptions)
      } else { 0 }

      # Step 1: Calculate excess earnings
      wd$excess_earnings <- calculate_excess_earnings(wd$earnings, wd$ret1, wd$age, claim_age_val, nra_ind)

      # Step 2: Calculate reduction (capped at annual benefits)
      wrk_total_ben <- wd$wrk_ben + wd$spouse_ben
      total_ben_pot <- wrk_total_ben + wd$spouse_dep_ben
      wd$ret_reduction <- calculate_ret_reduction(wd$excess_earnings, ret_rate, total_ben_pot)

      # Step 3: Allocate reduction between benefits
      alloc <- allocate_ret_reduction(wd$ret_reduction, wd$wrk_ben, wd$spouse_ben, wd$spouse_dep_ben)
      wd$wrk_share <- alloc$wrk_share
      wd$wrk_reduction <- alloc$wrk_reduction
      wrk_ben_reduced <- alloc$wrk_ben_reduced
      spouse_ben_reduced <- alloc$spouse_ben_reduced

      # Step 4: Calculate months withheld
      wd$months_withheld <- calculate_months_withheld(wd$wrk_reduction, wrk_total_ben, wd$age, claim_age_val, nra_ind)
      wd$cum_months_withheld <- cumsum(if_else(wd$age < nra_ind, wd$months_withheld, 0))
      cum_months_at_nra <- max(wd$cum_months_withheld[wd$age < nra_ind], 0, na.rm = TRUE)

      # Step 5: Calculate DRC payback factors
      drc_factors <- calculate_drc_payback(claim_age_val, cum_months_at_nra, nra_ind,
                                            rf1_ind, rf2_ind, drc_ind, s_rf1_ind, s_rf2_ind, drc_max)

      # Apply final benefits
      wd <- wd %>% mutate(
        wrk_ben = case_when(
          age < claim_age ~ 0,
          age < nra_ind ~ wrk_ben_reduced,
          TRUE ~ floor(cola_basic_pia * drc_factors$new_act_factor)
        ),
        spouse_ben = case_when(
          age < claim_age ~ 0,
          age < nra_ind ~ spouse_ben_reduced,
          TRUE ~ floor(spouse_pia * drc_factors$new_s_act_factor)
        ),
        ret_adj_factor = if_else(age >= nra_ind, drc_factors$new_act_factor, drc_factors$orig_act_factor),
        ret_s_adj_factor = if_else(age >= nra_ind, drc_factors$new_s_act_factor, drc_factors$orig_s_act_factor),
        cum_months_withheld_final = cum_months_at_nra
      )
      wd
    }) %>%
    ungroup()

  # Select output columns
  if (debugg) {
    worker %>% left_join(
      dataset %>% select(id, age, wrk_ben, spouse_ben, spouse_dep_ben, excess_earnings,
                         ret_reduction, wrk_share, wrk_reduction, months_withheld,
                         cum_months_withheld, ret_adj_factor, ret_s_adj_factor, cum_months_withheld_final),
      by = c("id", "age"), suffix = c("_orig", "")
    ) %>% select(-wrk_ben_orig, -spouse_ben_orig)
  } else {
    worker %>% left_join(
      dataset %>% select(id, age, wrk_ben, spouse_ben),
      by = c("id", "age"), suffix = c("_orig", "")
    ) %>% select(-wrk_ben_orig, -spouse_ben_orig)
  }
}
