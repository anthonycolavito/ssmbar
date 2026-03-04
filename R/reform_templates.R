# =============================================================================
# SOCIAL SECURITY REFORM TEMPLATES
# =============================================================================
#
# This file contains factory functions that create Reform objects for common
# Social Security reform proposals. Each function returns a Reform object that
# can be applied to assumptions using apply_reform().
#
# Key Principle: Most reforms phase in by ELIGIBILITY COHORT (year turning 62
# for retired workers, year of disability onset for disabled workers), not by
# calendar year. This is automatic because the benefit calculation pipeline
# looks up parameters at the worker's eligibility age.
#
# Mutual Exclusivity Groups:
# - Reforms #2-4: PIA formula changes (pick one)
# - Reforms #5-7: NRA changes (pick one)
# - Reforms #8-10: COLA index changes (pick one)
#
# =============================================================================


# =============================================================================
# TIER 1: PARAMETER-ONLY REFORMS (Simplest)
# =============================================================================

# -----------------------------------------------------------------------------
# Reform #1: PIA Multiplier (Across-the-Board Benefit Change)
# -----------------------------------------------------------------------------

#' Create Reform: PIA Multiplier (Benefit Change)
#'
#' Creates a reform that applies a multiplier to all PIAs, effectively changing
#' benefits by a percentage. This is the simplest way to model across-the-board
#' benefit changes.
#'
#' @param multiplier Numeric multiplier for benefits. 0.95 = 5% cut, 1.10 = 10% increase.
#' @param effective_year Year when the reform takes effect (by eligibility cohort).
#'   Only workers whose eligibility year >= this value are affected; existing
#'   beneficiaries keep their unmodified PIA.
#' @param phase_in_years Number of years to phase in the reform (0 = immediate).
#'   Default is 0.
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' # 5% benefit reduction starting in 2030
#' reform <- reform_reduce_benefits(multiplier = 0.95, effective_year = 2030)
#'
#' # 10% benefit increase phased in over 10 years
#' reform <- reform_reduce_benefits(multiplier = 1.10, effective_year = 2030,
#'                                   phase_in_years = 10)
#' }
#'
#' @export
reform_reduce_benefits <- function(multiplier, effective_year, phase_in_years = 0) {
  if (multiplier <= 0) {
    stop("'multiplier' must be positive")
  }

  pct_change <- (multiplier - 1) * 100
  change_desc <- if (pct_change >= 0) "Increase" else "Reduction"

  create_reform(
    name = sprintf("%.1f%% Benefit %s", abs(pct_change), change_desc),
    description = sprintf("Apply %.3f multiplier to all PIAs, %s benefits by %.1f%%",
                          multiplier, tolower(change_desc), abs(pct_change)),
    parameters = list(
      list(param = "pia_multiplier", value = multiplier, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = phase_in_years
  )
}


# -----------------------------------------------------------------------------
# Reform #4: Reduce fact3 to 5%
# -----------------------------------------------------------------------------

#' Create Reform: Reduce Third Replacement Factor
#'
#' Creates a reform that reduces the third PIA replacement factor (fact3) from
#' 15% to a target value. This reduces benefits for high earners.
#'
#' Wrapper around reform_benefit_formula() for convenience.
#'
#' @param target_fact3 Target value for fact3. Default is 0.05 (5%).
#' @param effective_year Year when the reform takes effect (by eligibility cohort).
#'   Only workers whose eligibility year >= this value are affected; existing
#'   beneficiaries keep their unmodified PIA.
#' @param phase_in_years Number of years to phase in. Default is 10.
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' # Reduce fact3 to 5% over 10 years starting in 2030
#' reform <- reform_reduce_fact3(target_fact3 = 0.05, effective_year = 2030)
#' }
#'
#' @export
reform_reduce_fact3 <- function(target_fact3 = 0.05, effective_year, phase_in_years = 10) {
  reform_benefit_formula(
    fact3 = target_fact3,
    effective_year = effective_year,
    phase_in_years = phase_in_years
  )
}


# -----------------------------------------------------------------------------
# Reform #5: NRA to 68 (capped)
# -----------------------------------------------------------------------------

#' Create Reform: Raise NRA to 68
#'
#' Creates a reform that raises the Normal Retirement Age from 67 to 68.
#' Phase-in: +1 month per 2 eligibility cohorts, capped at 68.
#'
#' @param effective_year Year when the reform takes effect (by eligibility cohort).
#'   Only workers whose eligibility year >= this value are affected; existing
#'   beneficiaries keep their current-law NRA.
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' reform <- reform_nra_to_68(effective_year = 2030)
#' }
#'
#' @export
reform_nra_to_68 <- function(effective_year = 2026) {
  # NRA trajectory: +1/12 per 2 birth years, cap at 68
  nra_func <- function(year) {
    cohorts_since <- max(0, year - effective_year)
    new_nra <- 67 + floor(cohorts_since / 2) / 12
    min(new_nra, 68)
  }

  create_reform(
    name = "Raise NRA to 68",
    description = "Gradually raise Normal Retirement Age from 67 to 68 (+1 month per 2 years, capped)",
    parameters = list(
      list(param = "nra", value = nra_func, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = 0  # Year-by-year function handles phase-in
  )
}


# -----------------------------------------------------------------------------
# Reform #6: Index NRA to Longevity (no cap)
# -----------------------------------------------------------------------------

#' Create Reform: Index NRA to Longevity
#'
#' Creates a reform that indexes the Normal Retirement Age to life expectancy
#' improvements. Phase-in: +1 month per 2 eligibility cohorts, no cap.
#'
#' @param effective_year Year when the reform takes effect (by eligibility cohort).
#'   Only workers whose eligibility year >= this value are affected; existing
#'   beneficiaries keep their current-law NRA.
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' reform <- reform_index_nra(effective_year = 2030)
#' }
#'
#' @export
reform_index_nra <- function(effective_year = 2026) {
  # NRA trajectory: +1/12 per 2 birth years, no cap
  nra_func <- function(year) {
    cohorts_since <- max(0, year - effective_year)
    67 + floor(cohorts_since / 2) / 12
  }

  create_reform(
    name = "Index NRA to Longevity",
    description = "Index Normal Retirement Age to life expectancy (+1 month per 2 years, uncapped)",
    parameters = list(
      list(param = "nra", value = nra_func, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = 0
  )
}


# -----------------------------------------------------------------------------
# Reform #7: NRA to 69, then index
# -----------------------------------------------------------------------------

#' Create Reform: Raise NRA to 69, Then Index
#'
#' Creates a reform that raises the Normal Retirement Age from 67 to 69 quickly,
#' then indexes to longevity. Phase 1: +2 months per year until 69. Phase 2:
#' +1 month per 2 years after 69.
#'
#' @param effective_year Year when the reform takes effect (by eligibility cohort).
#'   Only workers whose eligibility year >= this value are affected; existing
#'   beneficiaries keep their current-law NRA.
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' reform <- reform_nra_to_69_index(effective_year = 2030)
#' }
#'
#' @export
reform_nra_to_69_index <- function(effective_year = 2026) {
  # NRA trajectory: Phase 1 - 2 months per year until 69, Phase 2 - 1 month per 2 years
  nra_func <- function(year) {
    cohorts_since <- max(0, year - effective_year)
    if (cohorts_since < 12) {
      # Phase 1: 2 months per year until 69 (12 years to go from 67 to 69)
      67 + cohorts_since * 2 / 12
    } else {
      # Phase 2: 1 month per 2 years after reaching 69
      69 + floor((cohorts_since - 12) / 2) / 12
    }
  }

  create_reform(
    name = "Raise NRA to 69, Then Index",
    description = "Raise NRA to 69 (+2 months/year), then index to longevity (+1 month/2 years)",
    parameters = list(
      list(param = "nra", value = nra_func, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = 0
  )
}


# -----------------------------------------------------------------------------
# Reform #8: Chained CPI (C-CPI-U)
# -----------------------------------------------------------------------------

#' Create Reform: Index COLAs to Chained CPI
#'
#' Creates a reform that indexes COLAs to the Chained CPI-U (C-CPI-U) instead
#' of CPI-W. C-CPI-U grows about 0.3 percentage points slower than CPI-W.
#'
#' This is a calendar-year change: the reduced COLA applies to all
#' beneficiaries (current and future) starting in the effective year.
#'
#' @param effective_year Calendar year when the reduced COLA first applies.
#'   All beneficiaries receiving benefits in this year and beyond are affected.
#'   Default is 2026.
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' reform <- reform_chained_cpi(effective_year = 2030)
#' }
#'
#' @export
reform_chained_cpi <- function(effective_year = 2026) {
  create_reform(
    name = "Index COLAs to Chained CPI",
    description = "Reduce COLAs by 0.3 percentage points to simulate C-CPI-U indexing",
    parameters = list(
      list(param = "cola", value = -0.3, type = "add")
    ),
    effective_year = effective_year,
    phase_in_years = 0
  )
}


# -----------------------------------------------------------------------------
# Reform #10: CPI-E (Elderly Index)
# -----------------------------------------------------------------------------

#' Create Reform: Index COLAs to CPI-E
#'
#' Creates a reform that indexes COLAs to the CPI for the Elderly (CPI-E).
#' CPI-E grows about 0.2 percentage points faster than CPI-W due to higher
#' health care spending by seniors.
#'
#' This is a calendar-year change: the increased COLA applies to all
#' beneficiaries (current and future) starting in the effective year.
#'
#' @param effective_year Calendar year when the increased COLA first applies.
#'   All beneficiaries receiving benefits in this year and beyond are affected.
#'   Default is 2026.
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' reform <- reform_cpi_e(effective_year = 2030)
#' }
#'
#' @export
reform_cpi_e <- function(effective_year = 2026) {
  create_reform(
    name = "Index COLAs to CPI-E",
    description = "Increase COLAs by 0.2 percentage points to simulate CPI-E indexing",
    parameters = list(
      list(param = "cola", value = 0.2, type = "add")
    ),
    effective_year = effective_year,
    phase_in_years = 0
  )
}


# -----------------------------------------------------------------------------
# Reform #11: Change Payroll Tax Rate
# -----------------------------------------------------------------------------

#' Create Reform: Change Payroll Tax Rate
#'
#' Creates a reform that changes the OASI payroll tax rate by a specified
#' amount in percentage points.
#'
#' This is a calendar-year change: the new tax rate applies to all workers
#' earning in the effective year and beyond.
#'
#' @param rate_change Change in tax rate in percentage points. E.g., 1.0 for +1%.
#' @param effective_year Calendar year when the new tax rate first applies.
#'   All workers earning in this year and beyond are affected. Default is 2026.
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' # Increase payroll tax rate by 1 percentage point
#' reform <- reform_change_tax_rate(rate_change = 1.0, effective_year = 2030)
#' }
#'
#' @export
reform_change_tax_rate <- function(rate_change, effective_year = 2026) {
  change_desc <- if (rate_change >= 0) "Increase" else "Decrease"

  create_reform(
    name = sprintf("%s Tax Rate by %.2f pp", change_desc, abs(rate_change)),
    description = sprintf("%s OASI payroll tax rate by %.2f percentage points",
                          change_desc, abs(rate_change)),
    parameters = list(
      list(param = "oasi_tr", value = rate_change, type = "add")
    ),
    effective_year = effective_year,
    phase_in_years = 0
  )
}


# -----------------------------------------------------------------------------
# Reform #21: 40-Year Averaging
# -----------------------------------------------------------------------------

#' Create Reform: 40-Year Averaging Period
#'
#' Creates a reform that changes the AIME computation period from 35 years
#' (with 5 dropout years) to 40 years (with 0 dropout years).
#'
#' Phase-in: Reduce dropout years by 1 per year (5->4->3->2->1->0).
#'
#' @param effective_year Year when the reform takes effect (by eligibility cohort).
#'   Only workers whose eligibility year >= this value are affected; existing
#'   beneficiaries keep the standard 5 dropout years.
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' reform <- reform_40_year_averaging(effective_year = 2030)
#' }
#'
#' @export
reform_40_year_averaging <- function(effective_year = 2026) {
  # Dropout years trajectory: 5->4->3->2->1->0
  dropout_func <- function(year) {
    cohorts_since <- max(0, year - effective_year)
    max(0, 5 - cohorts_since)
  }

  create_reform(
    name = "40-Year Averaging",
    description = "Extend AIME computation period from 35 to 40 years by eliminating dropout years",
    parameters = list(
      list(param = "max_dropout_years", value = dropout_func, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = 0  # Function handles year-by-year phase-in
  )
}


# -----------------------------------------------------------------------------
# Reform #23: Repeal Retirement Earnings Test
# -----------------------------------------------------------------------------

#' Create Reform: Repeal Retirement Earnings Test
#'
#' Creates a reform that repeals the Retirement Earnings Test (RET), allowing
#' beneficiaries to receive full benefits regardless of earnings.
#'
#' This is a calendar-year change: the RET is repealed for all beneficiaries
#' starting in the effective year, regardless of when they first claimed.
#'
#' @param effective_year Calendar year when the RET is repealed. All
#'   beneficiaries with earnings in this year and beyond are unaffected by the
#'   RET. Default is 2026.
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' reform <- reform_repeal_ret(effective_year = 2030)
#' }
#'
#' @export
reform_repeal_ret <- function(effective_year = 2026) {
  create_reform(
    name = "Repeal Retirement Earnings Test",
    description = "Allow full benefits regardless of earnings before NRA",
    parameters = list(
      list(param = "ret_enabled", value = FALSE, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = 0
  )
}


# -----------------------------------------------------------------------------
# Reform #25: Phase Out Spousal Benefits
# -----------------------------------------------------------------------------

#' Create Reform: Phase Out Spousal Benefits
#'
#' Creates a reform that phases out spousal benefits by reducing the spousal
#' PIA share from 50% to 0%. The spousal PIA share is determined at the
#' worker's eligibility age, so this is an eligibility-cohort reform: only
#' workers reaching eligibility in or after the effective year are affected.
#' Existing beneficiaries keep their current-law 50% share.
#'
#' @param effective_year Year when the reform takes effect (by eligibility cohort).
#'   Only workers whose eligibility year >= this value see a reduced spousal share.
#' @param phase_in_years Number of years to phase out. Default is 10.
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' reform <- reform_phase_out_spousal(effective_year = 2030, phase_in_years = 20)
#' }
#'
#' @export
reform_phase_out_spousal <- function(effective_year = 2026, phase_in_years = 10) {
  create_reform(
    name = "Phase Out Spousal Benefits",
    description = sprintf("Phase out spousal benefits from 50%% to 0%% over %d years",
                          phase_in_years),
    parameters = list(
      list(param = "s_pia_share", value = 0, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = phase_in_years
  )
}


# =============================================================================
# TIER 2: MODERATE COMPLEXITY REFORMS
# =============================================================================

# -----------------------------------------------------------------------------
# Reform #9: COLA Cap
# -----------------------------------------------------------------------------

#' Create Reform: COLA Cap at Median PIA
#'
#' Creates a reform that caps COLAs for beneficiaries with PIAs above the
#' median. Beneficiaries above the threshold receive the same dollar COLA
#' increase as the median beneficiary, rather than a percentage increase.
#'
#' This is a calendar-year change: the cap applies to all beneficiaries
#' (current and future) whose PIA exceeds the median threshold in the
#' effective year and beyond.
#'
#' Requires cola_cap_median.csv data to be loaded in assumptions.
#'
#' @param effective_year Calendar year when the COLA cap first applies.
#'   All beneficiaries above the median PIA threshold are affected from this
#'   year forward. Default is 2026.
#'
#' @return A Reform object
#'
#' @details
#' This reform requires modifications to the cola() function to check if
#' the beneficiary's PIA exceeds the cola_cap threshold and apply dollar
#' capping instead of percentage increases.
#'
#' @examples
#' \dontrun{
#' reform <- reform_cola_cap(effective_year = 2030)
#' }
#'
#' @export
reform_cola_cap <- function(effective_year = 2026) {
  # The cola_cap column must be populated in assumptions (from cola_cap_median.csv)
  # The cola() function will check cola_cap_active flag and apply capping logic
  # The cola_cap values represent median PIA thresholds

  create_reform(
    name = "Cap COLAs at Median PIA",
    description = "Cap COLAs for beneficiaries above median PIA to same dollar increase as median",
    parameters = list(
      # Set cola_cap_active = TRUE for years >= effective_year
      # The cola() function checks this flag along with cola_cap values
      list(param = "cola_cap_active", value = TRUE, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = 0
  )
}


# -----------------------------------------------------------------------------
# Reform #12: Taxmax to 90% Coverage with 5% Credit
# -----------------------------------------------------------------------------

#' Create Reform: Taxmax to 90% Coverage with 5% Benefit Credit
#'
#' Creates a reform that raises the taxable maximum to cover 90% of earnings
#' and adds a fourth PIA bend point/bracket with 5% replacement rate for
#' earnings above the old taxmax.
#'
#' The higher taxable maximum applies by calendar year: all workers earning
#' above the old cap pay additional payroll taxes starting in the effective
#' year. The benefit credit (5% 4th bracket) effectively phases in for new
#' cohorts, since only workers who paid taxes on the higher earnings have
#' AIME above the old cap to receive credit for.
#'
#' @param effective_year Year when the reform takes effect. Default is 2026.
#' @param assumptions Optional assumptions data frame (e.g., tr2025). When provided,
#'   bp3 and the new taxmax are derived from the actual taxmax schedule via closures.
#'   When NULL, uses hardcoded fallback values.
#'
#' @return A Reform object
#'
#' @details
#' The 90% coverage taxmax is $330,500 in 2026 and is AWI-indexed thereafter.
#' The ratio 330500/184500 = 1.7913 is applied to each year's current-law taxmax
#' to produce the new higher cap. Creates a 4-bracket PIA formula: 90/32/15/5.
#'
#' @examples
#' \dontrun{
#' reform <- reform_taxmax_90_pct(effective_year = 2030)
#' reform <- reform_taxmax_90_pct(effective_year = 2030, assumptions = tr2025)
#' }
#'
#' @export
reform_taxmax_90_pct <- function(effective_year = 2026, assumptions = NULL) {
  # Reform #12: Raise taxmax to cover 90% of earnings
  # - taxmax increases to $330,500 in 2026 (AWI-indexed)
  # - 4th PIA bracket: 5% replacement rate for AIME above old taxmax
  # - bp3 = old taxmax / 12 (monthly terms)
  # - fact4 = 5%
  # - taxmax_benefit = NA (use taxmax for AIME capping, which is now higher)

  # Ratio of 90%-coverage taxmax to current-law taxmax in 2026
  taxmax_ratio <- 330500 / 184500  # ~1.7913

  if (!is.null(assumptions)) {
    # Closure over actual taxmax schedule from TR2025 (has values through 2100)
    taxmax_schedule <- setNames(assumptions$taxmax, assumptions$year)

    bp3_fn <- function(year) {
      val <- taxmax_schedule[as.character(year)]
      if (length(val) == 0 || is.na(val)) return(NA_real_)
      unname(val / 12)  # Old taxmax in monthly terms
    }

    new_taxmax_fn <- function(year) {
      if (year < effective_year) return(NA)  # Keep existing before effective year
      val <- taxmax_schedule[as.character(year)]
      if (length(val) == 0 || is.na(val)) return(NA)
      unname(val * taxmax_ratio)
    }

    new_taxmax_benefit_fn <- function(year) {
      if (year < effective_year) return(NA)  # Keep existing before effective year
      val <- taxmax_schedule[as.character(year)]
      if (length(val) == 0 || is.na(val)) return(NA)
      unname(val * taxmax_ratio)  # Same as new taxmax
    }
  } else {
    # Fallback: hardcoded values for unit tests without assumptions
    old_taxmax <- c(
      "2024" = 168600, "2025" = 176100, "2026" = 184500, "2027" = 190800,
      "2028" = 198900, "2029" = 207000, "2030" = 215400, "2031" = 223800,
      "2032" = 232500, "2033" = 241800, "2034" = 251100, "2035" = 261300,
      "2036" = 271200, "2037" = 281400, "2038" = 291600, "2039" = 302400,
      "2040" = 313500
    )

    bp3_fn <- function(year) {
      year_str <- as.character(year)
      if (year_str %in% names(old_taxmax)) {
        old_taxmax[[year_str]] / 12
      } else if (year >= 2040) {
        old_taxmax[["2040"]] * (1.038^(year - 2040)) / 12
      } else {
        NA_real_
      }
    }

    new_taxmax_fn <- function(year) {
      if (year < effective_year) return(NA)
      330500 * (1.038^(year - 2026))
    }

    new_taxmax_benefit_fn <- function(year) {
      if (year < effective_year) return(NA)
      330500 * (1.038^(year - 2026))
    }
  }

  create_reform(
    name = "Raise Taxmax to 90% Coverage with 5% Credit",
    description = "Raise taxable maximum to cover 90% of earnings; 5% replacement rate for new bracket",
    parameters = list(
      # bp3 = old taxmax in monthly terms (for 4th bracket calculation)
      list(param = "bp3", value = bp3_fn, type = "replace"),
      list(param = "fact4", value = 0.05, type = "replace"),
      # New taxmax at 90% coverage
      list(param = "taxmax", value = new_taxmax_fn, type = "replace"),
      # Set taxmax_benefit = new taxmax so AIME uses the higher cap
      # (aime_reform() uses benefit_cap = taxmax_benefit if not NA, otherwise taxmax)
      list(param = "taxmax_benefit", value = new_taxmax_benefit_fn, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = 0
  )
}


# -----------------------------------------------------------------------------
# Reform #13: Eliminate Taxmax with 15% Credit
# -----------------------------------------------------------------------------

#' Create Reform: Eliminate Taxmax with 15% Benefit Credit
#'
#' Creates a reform that eliminates the taxable maximum entirely and adds
#' a fourth PIA bracket with 15% replacement rate for all earnings above
#' the old taxmax.
#'
#' The unlimited taxable maximum applies by calendar year: all workers pay
#' payroll taxes on their full earnings starting in the effective year. The
#' benefit credit (15% 4th bracket) effectively phases in for new cohorts,
#' since only workers who paid taxes on earnings above the old cap have
#' AIME above the old cap to receive credit for.
#'
#' @param effective_year Year when the reform takes effect. Default is 2026.
#' @param assumptions Optional assumptions data frame (e.g., tr2025). When provided,
#'   bp3 is derived from the actual taxmax schedule (old cap / 12). When NULL,
#'   returns NA_real_ and relies on pia_reform() fallback (taxmax_benefit / 12).
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' reform <- reform_eliminate_taxmax(effective_year = 2030)
#' reform <- reform_eliminate_taxmax(effective_year = 2030, assumptions = tr2025)
#' }
#'
#' @export
reform_eliminate_taxmax <- function(effective_year = 2026, assumptions = NULL) {

  if (!is.null(assumptions)) {
    # Closure: bp3 = old (current-law) taxmax in monthly terms
    taxmax_schedule <- setNames(assumptions$taxmax, assumptions$year)
    bp3_fn <- function(year) {
      val <- taxmax_schedule[as.character(year)]
      if (length(val) == 0 || is.na(val)) return(NA_real_)
      unname(val / 12)
    }
  } else {
    # Fallback: NA triggers pia_reform() fallback (taxmax_benefit / 12)
    bp3_fn <- function(year) NA_real_
  }

  create_reform(
    name = "Eliminate Taxmax with 15% Credit",
    description = "Remove taxable maximum; 15% replacement rate for earnings above old cap",
    parameters = list(
      # bp3 = old taxmax in monthly terms (4th bracket starts here)
      list(param = "bp3", value = bp3_fn, type = "replace"),
      list(param = "fact4", value = 0.15, type = "replace"),
      # Very high taxmax effectively eliminates cap
      list(param = "taxmax", value = 10000000, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = 0
  )
}


# -----------------------------------------------------------------------------
# Reform #14: Eliminate Taxmax without Credit
# -----------------------------------------------------------------------------

#' Create Reform: Eliminate Taxmax without Benefit Credit
#'
#' Creates a reform that eliminates the taxable maximum for tax purposes
#' but keeps the old cap for benefit calculation purposes. This means
#' workers pay taxes on all earnings but benefits are still calculated
#' using earnings capped at the old taxmax.
#'
#' This is a calendar-year tax reform: all workers pay payroll taxes on
#' their full earnings starting in the effective year. Benefits are
#' completely unaffected — AIME is still computed using earnings capped
#' at the current-law taxable maximum.
#'
#' @param effective_year Calendar year when the unlimited tax cap first
#'   applies. All workers earning above the old taxmax pay additional
#'   taxes from this year forward. Default is 2026.
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' reform <- reform_eliminate_taxmax_no_credit(effective_year = 2030)
#' }
#'
#' @export
reform_eliminate_taxmax_no_credit <- function(effective_year = 2026) {
  create_reform(
    name = "Eliminate Taxmax without Credit",
    description = "Tax all earnings; benefits still calculated using capped earnings",
    parameters = list(
      # taxmax_tax = unlimited (for payroll tax calculation)
      list(param = "taxmax_tax", value = 10000000, type = "replace"),
      # taxmax_benefit remains at current law (for AIME calculation)
      # No change needed - taxmax_benefit defaults to taxmax
      list(param = "taxmax_benefit", value = function(year) {
        NA_real_  # Keep existing taxmax for benefit calculation
      }, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = 0
  )
}


# -----------------------------------------------------------------------------
# Reform #27: Basic Minimum Benefit
# -----------------------------------------------------------------------------

#' Create Reform: Basic Minimum Benefit
#'
#' Creates a reform that establishes a Basic Minimum Benefit (BMB) that
#' supplements low benefits at or after NRA. The BMB is the difference
#' between the BMB rate and 70% of the worker's actuarially adjusted benefit.
#'
#' @param individual_amount Monthly BMB amount for individuals in 2026 dollars.
#'   Default is $900.
#' @param couple_amount Monthly BMB amount for couples in 2026 dollars.
#'   Default is $1,342.
#' @param effective_year Calendar year when the BMB first applies. All
#'   beneficiaries at or past NRA in this year and beyond receive the
#'   supplement, including existing beneficiaries. Default is 2026.
#' @param phase_in_years Number of years to phase in. Default is 0 (immediate).
#' @param assumptions Optional assumptions data frame (e.g., tr2025). When provided,
#'   BMB amounts are AWI-indexed with a two-year lag (same convention as bend
#'   points). When NULL, uses constant nominal amounts.
#'
#' @return A Reform object
#'
#' @details
#' The BMB kicks in at NRA and is calculated AFTER actuarial adjustments:
#' BMB_supplement = max(BMB_rate - 0.70 * actuarially_adjusted_benefit, 0)
#'
#' **Indexing**: When assumptions are provided, BMB amounts grow with AWI
#' year-over-year (two-year lag, same convention as bend points). At each
#' worker's eligibility age, the AWI-indexed BMB rate is locked in as their
#' cohort-specific base amount. After eligibility, that base is COLA'd forward
#' (same cumulative COLA as regular benefits), not further AWI-indexed.
#'
#' **Calendar-year application**: All beneficiaries at or past NRA receive the
#' supplement starting in the effective year, including existing beneficiaries.
#'
#' @examples
#' \dontrun{
#' reform <- reform_basic_minimum(effective_year = 2030)
#' reform <- reform_basic_minimum(effective_year = 2030, assumptions = tr2025)
#' }
#'
#' @export
reform_basic_minimum <- function(individual_amount = 900, couple_amount = 1342,
                                  effective_year = 2026, phase_in_years = 0,
                                  assumptions = NULL) {
  if (!is.null(assumptions)) {
    # AWI-index with two-year lag (same convention as bend points)
    # Base amounts are in 2026$, so AWI base year is 2024
    awi_schedule <- setNames(assumptions$awi, assumptions$year)
    awi_base <- awi_schedule[["2024"]]

    indiv_fn <- function(year) {
      awi_ref <- awi_schedule[as.character(year - 2)]
      if (length(awi_ref) == 0 || is.na(awi_ref)) return(individual_amount)
      unname(individual_amount * awi_ref / awi_base)
    }

    couple_fn <- function(year) {
      awi_ref <- awi_schedule[as.character(year - 2)]
      if (length(awi_ref) == 0 || is.na(awi_ref)) return(couple_amount)
      unname(couple_amount * awi_ref / awi_base)
    }

    # Set effective_year to earliest year so AWI-indexed values exist for ALL
    # years.  The actual start year is stored in bmb_start_year, which
    # basic_minimum_benefit() checks to determine when the supplement applies.
    min_year <- min(assumptions$year)
  } else {
    # Fallback: constant nominal amounts (for unit tests without assumptions)
    indiv_fn <- individual_amount
    couple_fn <- couple_amount
    min_year <- effective_year
  }

  create_reform(
    name = "Basic Minimum Benefit",
    description = sprintf("Establish BMB: $%.0f/month individual, $%.0f/month couple (2026$, AWI-indexed)",
                          individual_amount, couple_amount),
    parameters = list(
      list(param = "bmb_individual", value = indiv_fn, type = "replace"),
      list(param = "bmb_couple", value = couple_fn, type = "replace"),
      list(param = "bmb_start_year", value = effective_year, type = "replace")
    ),
    effective_year = min_year,
    phase_in_years = 0
  )
}


# -----------------------------------------------------------------------------
# Reform #29: Child Care Credit
# -----------------------------------------------------------------------------

#' Create Reform: Child Care Credit
#'
#' Creates a reform that provides credited earnings for years when a worker
#' had a child under age 6. Credited earnings equal max(actual earnings, 0.5 * AWI)
#' for up to 5 years, chosen to maximize the AIME increase.
#'
#' @param effective_year Year when the reform takes effect. Default is 2026.
#' @param max_years Maximum years of credit. Default is 5.
#'
#' @return A Reform object
#'
#' @details
#' This reform requires modification to the earnings generation or AIME
#' calculation to identify years with children under 6 and apply the
#' earnings floor.
#'
#' @examples
#' \dontrun{
#' reform <- reform_child_care_credit(effective_year = 2030)
#' }
#'
#' @export
reform_child_care_credit <- function(effective_year = 2026, max_years = 5) {
  # Reform #29: Child Care Credit
  # Credits earnings up to 0.5*AWI for years when worker had child under age 6
  # Up to max_years years, selecting years that maximize AIME gain

  create_reform(
    name = "Child Care Credit",
    description = sprintf("Credit earnings up to 0.5*AWI for years with child under 6 (max %d years)",
                          max_years),
    parameters = list(
      list(param = "child_care_credit_active", value = TRUE, type = "replace"),
      list(param = "max_child_care_years", value = max_years, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = 0
  )
}


# =============================================================================
# TIER 3: COMPLEX REFORMS
# =============================================================================

# -----------------------------------------------------------------------------
# Reform #2: Flat Benefit
# -----------------------------------------------------------------------------

#' Create Reform: Flat Benefit
#'
#' Creates a reform that establishes a flat benefit floor equal to 125% of
#' the federal poverty level, phased in over 25 years. The PIA formula is
#' also modified: fact2 phases from 32% to 4%, fact3 phases from 15% to 0%.
#'
#' @param flat_amount Monthly flat benefit in 2025 dollars. Default is $19,300/12
#'   (125% of poverty line, monthly).
#' @param effective_year Year when the reform takes effect. Default is 2026.
#' @param phase_in_years Number of years to phase in. Default is 25.
#' @param assumptions Assumptions data frame (must contain \code{year} and \code{awi}
#'   columns). When provided, the flat benefit is AWI-indexed with a two-year lag
#'   (same as bend points). When NULL, a constant flat_amount is used.
#'
#' @return A Reform object
#'
#' @details
#' Worker's PIA = max(formula_pia, flat_benefit) if they have 40 QCs.
#' The flat benefit is AWI-indexed with a two-year lag: for eligibility year Y,
#' flat_benefit(Y) = flat_amount * AWI(Y-2) / AWI(2023).
#'
#' @examples
#' \dontrun{
#' reform <- reform_flat_benefit(effective_year = 2030)
#' }
#'
#' @export
reform_flat_benefit <- function(flat_amount = 19300 / 12, effective_year = 2026,
                                 phase_in_years = 25, assumptions = NULL) {
  # flat_amount is monthly in 2025$; AWI-index with two-year lag (same as bend points)
  if (!is.null(assumptions)) {
    awi_schedule <- setNames(assumptions$awi, assumptions$year)
    awi_base <- awi_schedule[["2023"]]  # AWI(2023) = base year for 2025$ amounts
    flat_fn <- function(year) {
      awi_ref <- awi_schedule[as.character(year - 2)]
      if (length(awi_ref) == 0 || is.na(awi_ref)) return(flat_amount)
      unname(flat_amount * awi_ref / awi_base)
    }
  } else {
    # Fallback: constant (for backward compatibility when assumptions not available)
    flat_fn <- flat_amount
  }

  create_reform(
    name = "Flat Benefit Floor",
    description = sprintf("Establish flat benefit floor of $%.0f/month (2025$), phase in formula changes over %d years",
                          flat_amount, phase_in_years),
    parameters = list(
      list(param = "flat_benefit", value = flat_fn, type = "replace"),
      # Phase fact2 from 32% to 4%
      list(param = "fact2", value = 0.04, type = "replace"),
      # Phase fact3 from 15% to 0%
      list(param = "fact3", value = 0.00, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = phase_in_years
  )
}


# -----------------------------------------------------------------------------
# Reform #3: Simpson-Bowles 4-Bracket PIA
# -----------------------------------------------------------------------------

#' Create Reform: Simpson-Bowles 4-Bracket PIA
#'
#' Creates a reform that adds a fourth PIA bracket at AWI and modifies
#' replacement factors: fact2: 32%->30%, fact3: 15%->10%, fact4: 15%->5%.
#' Phased in over 10 eligibility cohorts.
#'
#' @param effective_year Year when the reform takes effect. Default is 2026.
#' @param phase_in_years Number of years to phase in. Default is 10.
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' reform <- reform_simpson_bowles(effective_year = 2030)
#' }
#'
#' @export
reform_simpson_bowles <- function(effective_year = 2026, phase_in_years = 10) {
  create_reform(
    name = "Simpson-Bowles 4-Bracket PIA",
    description = "Add 4th bracket at AWI; adjust factors: 90/30/10/5",
    parameters = list(
      # bp3 = AWI (set via function)
      list(param = "bp3", value = function(year) {
        NA_real_  # Implementation in pia() to use AWI
      }, type = "replace"),
      list(param = "fact2", value = 0.30, type = "replace"),
      list(param = "fact3", value = 0.10, type = "replace"),
      list(param = "fact4", value = 0.05, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = phase_in_years
  )
}


# -----------------------------------------------------------------------------
# Reform #22: Mini-PIA
# -----------------------------------------------------------------------------

#' Create Reform: Mini-PIA
#'
#' Creates a reform that changes the PIA calculation to "mini-PIA" method:
#' Instead of averaging earnings then applying the formula, this applies
#' the formula to each year's earnings then averages the results.
#'
#' Current law: PIA = formula(average(top 35 indexed earnings) / 12)
#' Mini-PIA: PIA = average(formula(each year's indexed earnings / 12))
#'
#' @param effective_year Year when the reform takes effect. Default is 2026.
#' @param phase_in_years Number of years to phase in. Default is 10.
#'
#' @return A Reform object
#'
#' @details
#' During phase-in: PIA = (1 - blend) * current_pia + blend * mini_pia
#'
#' @examples
#' \dontrun{
#' reform <- reform_mini_pia(effective_year = 2030)
#' }
#'
#' @export
reform_mini_pia <- function(effective_year = 2026, phase_in_years = 10) {
  # mini_pia_blend phases from 0 to 1 over phase_in_years
  create_reform(
    name = "Mini-PIA Calculation",
    description = "Apply PIA formula to each year's earnings, then average (vs. average earnings first)",
    parameters = list(
      list(param = "mini_pia_blend", value = 1.0, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = phase_in_years
  )
}


# -----------------------------------------------------------------------------
# Reform #28: 75% Widow Benefit
# -----------------------------------------------------------------------------

#' Create Reform: 75% Combined Widow Benefit
#'
#' Creates a reform that provides an alternative widow benefit calculation:
#' 75% of the combined worker benefits (survivor's own + deceased's), capped
#' at the medium worker's PIA. Widow receives the higher of current law or
#' this alternative.
#'
#' @param effective_year Year when the reform takes effect. Default is 2026.
#'
#' @return A Reform object
#'
#' @details
#' Alternative = min(75% * (survivor_wrk_ben + deceased_wrk_ben), medium_worker_pia)
#' Final = max(current_law_widow_ben, alternative)
#'
#' @examples
#' \dontrun{
#' reform <- reform_widow_75_pct(effective_year = 2030)
#' }
#'
#' @export
reform_widow_75_pct <- function(effective_year = 2026) {
  # Reform #28: 75% Combined Widow Benefit
  # Alternative = min(75% * (survivor_wrk_ben + deceased_wrk_ben), medium_worker_pia)
  # Final widow benefit = max(current_law_widow_ben, alternative)

  create_reform(
    name = "75% Combined Widow Benefit",
    description = "Widow benefit = max(current law, 75% of combined benefits capped at medium worker PIA)",
    parameters = list(
      list(param = "widow_75_pct_active", value = TRUE, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = 0
  )
}
