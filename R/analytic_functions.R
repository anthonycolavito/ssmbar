# =============================================================================
# ANALYTIC FUNCTIONS
# =============================================================================
#
# This file contains the core analytical functions for the ssmbar package.
#
# =============================================================================


# =============================================================================
# Tax Calculation Functions
# =============================================================================

#' Calculate Social Security Taxes Paid
#'
#' Calculates the nominal Social Security payroll taxes paid on a worker's earnings
#' for each year. Uses the OASI trust fund rate, DI trust fund rate, and taxable
#' maximum from the assumptions data.
#'
#' @param worker Data frame with worker earnings by year. Must contain columns:
#'   \code{id}, \code{year}, \code{age}, \code{earnings}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year}, \code{oasi_tr}, \code{di_tr}, \code{taxmax}.
#'
#' @return Data frame with the worker data plus additional tax columns:
#'   \itemize{
#'     \item \code{ss_taxable_earn}: Earnings subject to SS tax (capped at taxmax)
#'     \item \code{oasi_tax}: OASI tax paid (employee share)
#'     \item \code{di_tax}: DI tax paid (employee share)
#'     \item \code{ss_tax}: Total SS tax paid (employee share = oasi_tax + di_tax)
#'   }
#'
#' @details
#' The tax rates in the assumptions represent the EMPLOYEE portion of the payroll tax.
#' Employers pay an equal matching amount. The rates are expressed as percentages
#' (e.g., 5.3 means 5.3%), not decimals.
#'
#' The taxable maximum (taxmax) caps the amount of earnings subject to Social Security
#' taxes in each year. For example, in 2025 only the first $176,100 of earnings is taxable.
#'
#' @examples
#' \dontrun{
#' # Generate worker earnings
#' worker <- earnings_generator(birth_yr = 1960, sex = "male", type = "medium",
#'   age_claim = 67, factors = sef2025, assumptions = tr2025)
#'
#' # Calculate taxes paid
#' worker_with_taxes <- calculate_taxes(worker, tr2025)
#'
#' # View tax columns
#' worker_with_taxes[worker_with_taxes$age %in% c(25, 35, 45, 55),
#'   c("year", "age", "earnings", "ss_taxable_earn", "ss_tax")]
#' }
#'
#' @importFrom dplyr %>% mutate select left_join
#' @export
calculate_taxes <- function(worker, assumptions) {

  # Validate required columns in worker data
  worker_cols_needed <- c("id", "year", "age", "earnings")
  if (!all(worker_cols_needed %in% names(worker))) {
    stop(paste("worker data must contain:", paste(worker_cols_needed, collapse = ", ")))
  }

  # Validate required columns in assumptions
  assumption_cols_needed <- c("year", "oasi_tr", "di_tr", "taxmax")
  if (!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions data must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  # Join tax parameters from assumptions if not already present
  cols_needed <- c("oasi_tr", "di_tr", "taxmax")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>%
      left_join(assumptions %>% select(all_of(cols_to_join)), by = "year")
  } else {
    dataset <- worker
  }

  # Calculate taxes
  # Rates are percentages (e.g., 5.3 means 5.3%), so divide by 100
  dataset <- dataset %>%
    mutate(
      ss_taxable_earn = pmin(earnings, taxmax, na.rm = TRUE),  # Cap earnings at taxmax
      oasi_tax = ss_taxable_earn * oasi_tr / 100,              # OASI tax (employee share)
      di_tax = ss_taxable_earn * di_tr / 100,                  # DI tax (employee share)
      ss_tax = oasi_tax + di_tax                               # Total SS tax (employee share)
    )

  # Remove joined columns that were only needed for calculation
  if (length(cols_missing) > 0) {
    dataset <- dataset %>%
      select(-any_of(cols_missing))
  }

  return(dataset)
}


# =============================================================================
# Replacement Rate Functions
# =============================================================================

# TODO: Document - rep_rates() needs full roxygen documentation:
#   - Function description explaining replacement rate concept
#   - Parameter documentation for worker and assumptions
#   - Return value documentation (pivoted data frame with multiple RR types)
#   - Explanation of different replacement rate methods (pv_rr, real_all, wage_all, high-N, last-N)
#   - SSA handbook citations for indexing methodology
#   - Examples showing typical usage
rep_rates <- function(worker, assumptions) {

  #Error Prevention
  worker_cols_needed <- c("id","year","age","earnings","annual_ind")
  if(!all(worker_cols_needed %in% names(worker))){
      stop(paste("worker file must contain:", paste(worker_cols_needed, collapse = ", ")))
    }

  assumption_cols_needed <- c("year","gdp_pi","awi","real_df")
  if(!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions file must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  dataset <- worker %>% left_join(assumptions %>% select(year, gdp_pi, awi, real_df),
                                  by = "year") %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
           #Initial benefit (numerator in the replacement rate)
           init_ben = annual_ind[which(age == 65)],

           #Scalars
           wage_scalar = awi[which(age == 65)] / awi,
           price_scalar = gdp_pi[which(age == 65)] / gdp_pi,

           #Indexed Earnings
           wage_earnings = earnings * wage_scalar,
           real_earnings = earnings * price_scalar,

          #Discount factors normalized to age 21
          real_df_norm = real_df / real_df[which(age == 21)],

          #Present value of real earnings at age 21
          pv_real_earn = real_earnings / real_df_norm

           ) %>% filter(age <= 64) %>%
          summarise(
            #Initial benefit
            init_ben = first(init_ben),

            #PV of earnings at age 21 (in real dollars)
            pv_real_earnings = sum(pv_real_earn),

            #Constant real payment with same PV as career earnings
            real_annuity = pv_real_earnings / sum(1 / real_df_norm),

            #Replacement Rates -- All
            pv_rr = init_ben / real_annuity,
            real_all = init_ben / mean(real_earnings),
            wage_all = init_ben / mean(wage_earnings),

             #High-N Year Replacement Rates
            real_sorted = list(sort(real_earnings, decreasing = TRUE)),
            wage_sorted = list(sort(wage_earnings, decreasing = TRUE)),

            real_h35 = init_ben / mean(real_sorted[[1]][1:35]),
            wage_h35 = init_ben / mean(wage_sorted[[1]][1:35]),
            real_h10 = init_ben / mean(real_sorted[[1]][1:10]),
            wage_h10 = init_ben / mean(wage_sorted[[1]][1:10]),
            real_h5 = init_ben / mean(real_sorted[[1]][1:5]),
            wage_h5 = init_ben / mean(wage_sorted[[1]][1:5]),

            #Last-N Years Replacement Rates
            n_years = n(),

            real_l35 = init_ben / mean(real_earnings[(n_years - 34): n_years]),
            wage_l35 = init_ben / mean(wage_earnings[(n_years - 34): n_years]),
            real_l10 = init_ben / mean(real_earnings[(n_years - 9): n_years]),
            wage_l10 = init_ben / mean(wage_earnings[(n_years - 9): n_years]),
            real_l5 = init_ben / mean(real_earnings[(n_years - 4): n_years]),
            wage_l5 = init_ben / mean(wage_earnings[(n_years - 4): n_years])

           ) %>% select(id, pv_rr, real_all, wage_all,
                        real_h35, wage_h35, real_h10, wage_h10, real_h5, wage_h5,
                        real_l35, wage_l35, real_l10, wage_l10, real_l5, wage_l5) %>%
    pivot_longer(cols = !"id",
                 names_to = "type",
                 values_to = "rep_rate")

  return(dataset)


}

