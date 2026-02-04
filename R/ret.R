# =============================================================================
# RETIREMENT EARNINGS TEST (RET) CALCULATIONS
# =============================================================================
#
# This file contains the baseline (current law) Retirement Earnings Test function
# (ret) and its helper functions.
#
# Baseline function:
#   - ret(): Current law RET calculation
#
# Helper functions (used by both baseline and reform versions):
#   - calculate_excess_earnings(): Calculates excess earnings above RET threshold
#   - calculate_ret_reduction(): Calculates benefit reduction from excess earnings
#   - allocate_ret_reduction(): Allocates reduction between worker and spouse benefits
#   - calculate_months_withheld(): Calculates months of benefits withheld
#   - calculate_drc_payback(): Recalculates actuarial factors at NRA
#   - calculate_spouse_ret_effect(): Calculates spouse's RET effect on worker's benefit
#
# For REFORM-CAPABLE RET with Reform #23 (RET repeal), see:
#   - ret_reform() in reform_functions.R
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
#' @param s_pia_share Numeric, spousal PIA share (typically 0.5)
#' @return List with wrk_ben_reduced and spouse_ben_reduced (monthly)
#' @keywords internal

allocate_ret_reduction <- function(total_reduction, wrk_ben, spouse_ben, spouse_dep_ben, s_pia_share) {
  # Section 1804: How Excess Earnings are Charged Against Benefits
  # https://www.ssa.gov/OP_Home/handbook/handbook.18/handbook-1804.html
  # Section 1806: Payment of Partial Benefit
  # https://www.ssa.gov/OP_Home/handbook/handbook.18/handbook-1806.html
  # Reductions are allocated proportionally to beneficiaries' "original entitlement rate."
  # That is, the share of the retired worker's benefits without any adjustments for
  # claiming timing or partial benefits received (dual eligibles)
  # For example, a spouse receiving a dependent spousal benefit would be allocated
  # 1/3 of the reduction since their PIA is 50% of the retired worker's (100% + 50% = 150%)

  wrk_share <- if_else(spouse_dep_ben > 0, 1/(1+s_pia_share), 1)

  # Total benefit pot
  wrk_total_ben <- wrk_ben + spouse_ben
  total_ben_pot <- wrk_total_ben + spouse_dep_ben

  # Worker's share of reduction (proportional to their benefits vs total pot)
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


#' Calculate Spouse's RET Effect on Worker's Spousal Benefit (Internal)
#'
#' When the worker receives a spousal benefit based on their spouse's record,
#' and the spouse has excess earnings, part of the spouse's RET reduction
#' is allocated to the worker's spousal benefit.
#'
#' Section 1803: RET applies to benefits based on the earner's record.
#' When the spouse (whose record the worker's spousal benefit is based on)
#' has excess earnings, benefits on their record are reduced, including
#' any dependent spousal benefits the worker receives.
#'
#' The reduction is allocated based on original entitlement rate:
#' - Spouse's own benefit: 100% of PIA -> share = 1/(1+s_pia_share) = 2/3
#' - Worker's spouse_ben: 50% of PIA -> share = s_pia_share/(1+s_pia_share) = 1/3
#'
#' @param worker_data Data frame for a single worker (one row per age)
#' @param spouse_df Data frame from generate_spouse() with s_earnings, s_pia, etc.
#' @param assumptions Data frame of assumptions
#' @param s_pia_share Numeric, spousal PIA share (typically 0.5)
#' @return List with:
#'   \itemize{
#'     \item \code{spouse_ben_reduction}: Monthly reduction to worker's spouse_ben
#'     \item \code{s_excess_earnings}: Spouse's excess earnings
#'     \item \code{s_ret_reduction}: Spouse's total RET reduction
#'     \item \code{s_own_ben}: Spouse's own benefit
#'     \item \code{s_nra}: Spouse's NRA
#'     \item \code{s_months_withheld}: Months of worker's spouse_ben withheld
#'   }
#' @keywords internal

calculate_spouse_ret_effect <- function(worker_data, spouse_df, assumptions, s_pia_share) {
  # Get spouse's parameters
  s_birth_yr <- spouse_df$s_birth_yr[1]
  s_claim_age <- spouse_df$s_claim_age[1]
  s_earnings <- spouse_df$s_earnings
  s_pia <- spouse_df$s_pia  # COLA-adjusted PIA

  # Calculate spouse's age at each year
  s_age <- worker_data$year - s_birth_yr

  # Get spouse's birth-cohort parameters (NRA, reduction factors)
  # Based on the year spouse turns eligibility age
  # NOTE: Edge case - if spouse's eligibility year is outside assumptions data range,

  # the lookup will return NA. This is not currently handled.
  elig_age_ret <- assumptions$elig_age_retired[1]
  s_yr_elig <- s_birth_yr + elig_age_ret

  # Look up spouse's NRA and actuarial factors at their eligibility year
  s_nra <- assumptions$nra[assumptions$year == s_yr_elig][1]
  s_rf1 <- assumptions$rf1[assumptions$year == s_yr_elig][1]
  s_rf2 <- assumptions$rf2[assumptions$year == s_yr_elig][1]
  s_drc <- assumptions$drc[assumptions$year == s_yr_elig][1]
  drc_max <- assumptions$drc_max_months[1]
  ret_rate <- assumptions$ret_phaseout_rate[1]

  # Calculate spouse's actuarial factor
  s_act_factor <- rf_and_drc(s_claim_age, s_nra, s_rf1, s_rf2, s_drc, drc_max)

  # Calculate spouse's own benefit at each age (after actuarial adjustment)
  s_own_ben <- if_else(s_age >= s_claim_age, floor(s_pia * s_act_factor), 0)

  # Get worker's spouse_ben (benefit worker receives from spouse's record)
  worker_spouse_ben <- worker_data$spouse_ben

  # Total benefit pot on spouse's record = spouse's own + worker's spousal
  s_total_pot <- s_own_ben + worker_spouse_ben

  # Calculate spouse's excess earnings (RET window: claim_age to NRA)
  ret1 <- worker_data$ret1
  s_excess <- calculate_excess_earnings(s_earnings, ret1, s_age, s_claim_age, s_nra)

  # Calculate spouse's total RET reduction (capped at total pot)
  s_ret_reduction <- calculate_ret_reduction(s_excess, ret_rate, s_total_pot)

  # Allocate reduction based on original entitlement rate:
  # - Spouse's own benefit: 100% of PIA -> share = 1/(1+s_pia_share) = 2/3
  # - Worker's spouse_ben: 50% of PIA -> share = s_pia_share/(1+s_pia_share) = 1/3
  worker_share <- s_pia_share / (1 + s_pia_share)

  # Annual reduction to worker's spouse_ben
  spouse_ben_reduction_annual <- s_ret_reduction * worker_share

  # Monthly reduction to worker's spouse_ben
  spouse_ben_reduction_monthly <- spouse_ben_reduction_annual / 12

  # Calculate months of spouse_ben withheld (for potential DRC payback)
  s_months_withheld <- if_else(
    worker_spouse_ben > 0 & s_age >= s_claim_age & s_age < s_nra,
    pmin(spouse_ben_reduction_annual / worker_spouse_ben, 12),
    0
  )

  list(
    spouse_ben_reduction = spouse_ben_reduction_monthly,
    s_excess_earnings = s_excess,
    s_ret_reduction = s_ret_reduction,
    s_own_ben = s_own_ben,
    s_nra = s_nra,
    s_act_factor = s_act_factor,
    s_months_withheld = s_months_withheld
  )
}


# -----------------------------------------------------------------------------
# 2. Main RET Function (Baseline)
# -----------------------------------------------------------------------------

#' Retirement Earnings Test Calculation (Baseline - Current Law)
#'
#' Function that reduces an individual's benefits if their earnings exceed the
#' exempt amounts in the Retirement Earnings Test, using current statutory rules.
#' Also calculates DRC payback at NRA to account for months of benefits withheld.
#'
#' This is the baseline version. For reform-capable RET calculation (with
#' options for RET repeal), see \code{\link{ret_reform}}.
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
#' @seealso \code{\link{ret_reform}} for the reform-capable version
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


  # Ensure spousal columns exist (may not exist if spousal_pia/spouse_benefit were skipped)
  if (!"spouse_ben" %in% names(dataset)) {
    dataset$spouse_ben <- 0
  }
  if (!"spouse_pia" %in% names(dataset)) {
    dataset$spouse_pia <- 0
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
      s_pia_share_val <- wd$s_pia_share[1]

      # Get birth-cohort parameters at eligibility age
      yr_elig <- wd$year[1] - wd$age[1] + elig_age_ret
      nra_ind <- wd$nra[wd$year == yr_elig][1]
      rf1_ind <- wd$rf1[wd$year == yr_elig][1]
      rf2_ind <- wd$rf2[wd$year == yr_elig][1]
      drc_ind <- wd$drc[wd$year == yr_elig][1]
      s_rf1_ind <- wd$s_rf1[wd$year == yr_elig][1]
      s_rf2_ind <- wd$s_rf2[wd$year == yr_elig][1]

      # =====================================================
      # Step 0: Apply SPOUSE's RET to worker's spouse_ben
      # =====================================================
      # If worker receives spouse_ben from spouse's record, and spouse has
      # excess earnings, reduce spouse_ben first (before worker's own RET).
      # The spouse is the "primary worker" for benefits based on their record.

      # Initialize spouse RET debug variables
      wd$s_excess_earnings <- 0
      wd$s_ret_reduction <- 0
      wd$s_own_ben <- 0
      wd$spouse_ret_to_spouse_ben <- 0
      wd$s_months_withheld <- 0
      wd$s_cum_months_withheld <- 0
      s_nra_ind <- NA_real_
      s_cum_months_at_s_nra <- 0

      if (!is.na(spec) && !is.null(spouse_data[[spec]]) && any(wd$spouse_ben > 0)) {
        spouse_ret <- calculate_spouse_ret_effect(wd, spouse_data[[spec]], assumptions, s_pia_share_val)

        # Store debug variables
        wd$s_excess_earnings <- spouse_ret$s_excess_earnings
        wd$s_ret_reduction <- spouse_ret$s_ret_reduction
        wd$s_own_ben <- spouse_ret$s_own_ben
        wd$spouse_ret_to_spouse_ben <- spouse_ret$spouse_ben_reduction * 12  # Annual for consistency
        wd$s_months_withheld <- spouse_ret$s_months_withheld
        s_nra_ind <- spouse_ret$s_nra

        # Apply spouse's RET reduction to worker's spouse_ben
        wd$spouse_ben <- pmax(wd$spouse_ben - spouse_ret$spouse_ben_reduction, 0)

        # Calculate cumulative months of spouse_ben withheld (for DRC payback)
        s_birth_yr <- spouse_data[[spec]]$s_birth_yr[1]
        wd$s_cum_months_withheld <- cumsum(if_else(
          (wd$year - s_birth_yr) < s_nra_ind,
          wd$s_months_withheld, 0
        ))
        s_cum_months_at_s_nra <- max(wd$s_cum_months_withheld, 0, na.rm = TRUE)
      }

      # =====================================================
      # Steps 1-5: Apply WORKER's RET (existing logic)
      # =====================================================
      # Now uses the already-reduced spouse_ben from Step 0

      # Calculate spouse's dependent benefit (based on worker's record)
      wd$spouse_dep_ben <- if (!is.na(spec) && !is.null(spouse_data[[spec]])) {
        calculate_spouse_dep_benefit(wd, spouse_data[[spec]], assumptions)
      } else { 0 }

      # Step 1: Calculate worker's excess earnings
      wd$excess_earnings <- calculate_excess_earnings(wd$earnings, wd$ret1, wd$age, claim_age_val, nra_ind)

      # Step 2: Calculate reduction (capped at annual benefits)
      wrk_total_ben <- wd$wrk_ben + wd$spouse_ben
      total_ben_pot <- wrk_total_ben + wd$spouse_dep_ben
      wd$ret_reduction <- calculate_ret_reduction(wd$excess_earnings, ret_rate, total_ben_pot)

      # Step 3: Allocate reduction between benefits
      alloc <- allocate_ret_reduction(wd$ret_reduction, wd$wrk_ben, wd$spouse_ben, wd$spouse_dep_ben, s_pia_share_val)
      wd$wrk_share <- alloc$wrk_share
      wd$wrk_reduction <- alloc$wrk_reduction
      wrk_ben_reduced <- alloc$wrk_ben_reduced
      spouse_ben_reduced <- alloc$spouse_ben_reduced

      # Step 4: Calculate months withheld (from worker's own RET)
      wd$months_withheld <- calculate_months_withheld(wd$wrk_reduction, wrk_total_ben, wd$age, claim_age_val, nra_ind)
      wd$cum_months_withheld <- cumsum(if_else(wd$age < nra_ind, wd$months_withheld, 0))
      cum_months_at_nra <- max(wd$cum_months_withheld[wd$age < nra_ind], 0, na.rm = TRUE)

      # Step 5: Calculate DRC payback factors for worker's own benefits
      # Note: Disabled workers (elig_age < elig_age_ret) don't get actuarial adjustments
      # or DRC payback - their benefit equals 100% of their PIA at all ages
      is_disabled <- wd$elig_age[1] < elig_age_ret

      drc_factors <- calculate_drc_payback(claim_age_val, cum_months_at_nra, nra_ind,
                                            rf1_ind, rf2_ind, drc_ind, s_rf1_ind, s_rf2_ind, drc_max)

      # TODO: Verify DRC payback for spouse_ben withheld due to spouse's RET.
      # Currently assuming the worker gets DRC payback on their spousal actuarial
      # factor based on months of spouse_ben withheld. This needs SSA verification.
      # The payback would occur at the SPOUSE's NRA, not the worker's NRA.
      s_drc_payback_factor <- if (s_cum_months_at_s_nra > 0 && !is.na(s_nra_ind)) {
        # Recalculate spousal actuarial factor based on effective claim age
        effective_s_claim_age <- min(claim_age_val + (s_cum_months_at_s_nra / 12), s_nra_ind)
        rf_and_drc(effective_s_claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0, drc_max)
      } else {
        drc_factors$new_s_act_factor
      }

      # Apply final benefits
      # For disabled workers: act_factor = 1.0 always (no DRC payback)
      # For retired workers: recalculate at NRA with DRC payback
      wd <- wd %>% mutate(
        wrk_ben = case_when(
          age < claim_age ~ 0,
          is_disabled ~ floor(cola_basic_pia * 1.0),  # Disabled: no actuarial adjustment
          age < nra_ind ~ wrk_ben_reduced,
          TRUE ~ floor(cola_basic_pia * drc_factors$new_act_factor)  # Retired: DRC payback at NRA
        ),
        spouse_ben = case_when(
          age < claim_age ~ 0,
          age < nra_ind ~ spouse_ben_reduced,
          # At/after NRA, use the DRC payback factor (accounting for spouse's RET withheld months)
          TRUE ~ floor(spouse_pia * s_drc_payback_factor)
        ),
        ret_adj_factor = if_else(age >= nra_ind, drc_factors$new_act_factor, drc_factors$orig_act_factor),
        ret_s_adj_factor = if_else(age >= nra_ind, s_drc_payback_factor, drc_factors$orig_s_act_factor),
        cum_months_withheld_final = cum_months_at_nra,
        s_cum_months_withheld_final = s_cum_months_at_s_nra
      )
      wd
    }) %>%
    ungroup()

  # Select output columns
  # Determine which columns to remove (only if they existed in original worker)
  cols_to_remove <- c()
  if ("wrk_ben" %in% names(worker)) cols_to_remove <- c(cols_to_remove, "wrk_ben_orig")
  if ("spouse_ben" %in% names(worker)) cols_to_remove <- c(cols_to_remove, "spouse_ben_orig")

  if (debugg) {
    # Debug columns need existence check
    debug_cols <- c("spouse_dep_ben", "s_excess_earnings", "s_ret_reduction", "s_own_ben",
                    "spouse_ret_to_spouse_ben", "s_months_withheld", "s_cum_months_withheld",
                    "excess_earnings", "ret_reduction", "wrk_share", "wrk_reduction",
                    "months_withheld", "cum_months_withheld", "ret_adj_factor", "ret_s_adj_factor",
                    "cum_months_withheld_final", "s_cum_months_withheld_final")
    cols_new <- debug_cols[!debug_cols %in% names(worker)]

    # First join wrk_ben and spouse_ben with suffix handling
    result <- worker %>% left_join(
      dataset %>% select(id, age, wrk_ben, spouse_ben),
      by = c("id", "age"), suffix = c("_orig", "")
    ) %>% select(-any_of(cols_to_remove))

    # Then join additional debug columns that don't already exist
    if (length(cols_new) > 0) {
      result <- result %>% left_join(
        dataset %>% select(id, age, all_of(cols_new)),
        by = c("id", "age")
      )
    }
    result
  } else {
    worker %>% left_join(
      dataset %>% select(id, age, wrk_ben, spouse_ben),
      by = c("id", "age"), suffix = c("_orig", "")
    ) %>% select(-any_of(cols_to_remove))
  }
}
