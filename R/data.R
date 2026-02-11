# Data Documentation for ssmbar
# This file contains roxygen2 documentation for package data objects.

#' 2025 Social Security Trustees Report Assumptions
#'
#' A dataset containing economic assumptions and program parameters from the
#' 2025 Social Security Trustees Report, processed through \code{prep_assumptions()}.
#' Contains 59 columns: 23 from the raw Trustees Report data, and 36 added by
#' \code{prep_assumptions()} (projected parameters, program rule constants, and
#' reform scaffolding).
#'
#' @name tr2025
#' @docType data
#' @usage data(tr2025)
#' @format A data frame with 150 rows (years 1951--2100) and 59 variables:
#'
#' \strong{Raw Trustees Report Data (from \code{inst/extdata/2025TR_assumptions.csv})}
#'
#' Assembled from the 2025 Trustees Report and SSA historical program data.
#' Historical values are published actuals; future values are intermediate-cost
#' projections.
#'
#' \describe{
#'   \item{year}{Calendar year (1951--2100)}
#'   \item{awi}{Average Wage Index}
#'   \item{cpi_w}{Consumer Price Index for Urban Wage Earners and Clerical
#'     Workers (CPI-W). Source: 2025 Trustees Report}
#'   \item{gdp_pi}{GDP Price Index. Derived from historical and projected annual
#'     growth rates in the Trustees Report, with base year calculated to 2025}
#'   \item{df}{Nominal discount factor. Derived from historical and projected
#'     nominal interest rates in the Trustees Report}
#'   \item{real_df}{Real discount factor. Derived from historical and projected
#'     real interest rates in the Trustees Report}
#'   \item{taxmax}{Social Security taxable maximum earnings. Historical values
#'     from SSA; NA values projected by \code{prep_assumptions()}}
#'   \item{oasi_tr}{OASI payroll tax rate (employee share, e.g., 0.053 = 5.3\%)}
#'   \item{di_tr}{DI payroll tax rate (employee share)}
#'   \item{bp1}{First PIA bend point (monthly). Earnings below this receive
#'     90\% replacement. Historical from SSA; NA values projected}
#'   \item{bp2}{Second PIA bend point (monthly). Earnings between bp1 and bp2
#'     receive 32\% replacement}
#'   \item{qc_rec}{Earnings required for one quarter of coverage. Historical
#'     from SSA; NA values projected}
#'   \item{fact1}{PIA formula factor for earnings below first bend point (0.90)}
#'   \item{fact2}{PIA formula factor for earnings between bend points (0.32)}
#'   \item{fact3}{PIA formula factor for earnings above second bend point (0.15)}
#'   \item{rf1}{Early retirement reduction factor per month, first 36 months
#'     before NRA (5/9 of 1\%). 42 USC 402(q)}
#'   \item{rf2}{Early retirement reduction factor per month, months beyond 36
#'     before NRA (5/12 of 1\%). 42 USC 402(q)}
#'   \item{drc}{Delayed retirement credit rate per month. 42 USC 402(w)}
#'   \item{nra}{Normal Retirement Age. 42 USC 416(l)}
#'   \item{ret1}{Retirement Earnings Test exempt amount (under NRA, annual)}
#'   \item{ret2}{Retirement Earnings Test exempt amount (year of NRA, annual)}
#'   \item{le_m}{Male cohort life expectancy at age 65 (total expected age at
#'     death), by cohort turning 62. From the cohort life expectancy tables in
#'     the Trustees Report}
#'   \item{le_f}{Female cohort life expectancy at age 65 (total expected age at
#'     death), by cohort turning 62. From the cohort life expectancy tables in
#'     the Trustees Report}
#' }
#'
#' \strong{Projected Program Parameters (computed by \code{prep_assumptions()})}
#'
#' Historical values loaded from supplementary CSV files in \code{inst/extdata/};
#' future values projected using statutory indexing formulas.
#'
#' \describe{
#'   \item{cola}{Cost-of-Living Adjustment percentage (e.g., 2.8 = 2.8\%).
#'     Historical from \code{inst/extdata/cola.csv}; projected =
#'     max(0, (CPI-W_Y / CPI-W_{Y-1} - 1) * 100). The COLA in year Y adjusts
#'     benefits starting January Y+1. 42 USC 415(i)}
#'   \item{s_pia_share}{Spousal PIA share: 0.50 (50\% of worker's PIA).
#'     42 USC 402(b)-(c)}
#'   \item{s_rf1}{Spousal early retirement reduction factor per month, first 36
#'     months (25/36 of 1\%). SSA Handbook \S 724}
#'   \item{s_rf2}{Spousal early retirement reduction factor per month, beyond 36
#'     months. Equal to rf2. SSA Handbook \S 724}
#'   \item{child_pia_share}{Child benefit as share of worker's PIA: 0.50 (50\%).
#'     42 USC 402(d)(2). (Needs verification)}
#'   \item{old_law_base}{Old-law contribution and benefit base. Projected:
#'     max(round(45000 * AWI_{Y-2} / AWI_1992 / 300) * 300, prev_year).
#'     Historical from \code{inst/extdata/yoc.csv} (assembled from SSA
#'     historical program data)}
#'   \item{yoc_threshold}{Years-of-coverage earnings threshold for special
#'     minimum PIA. Post-1990: 15\% of old_law_base. Historical from
#'     \code{inst/extdata/yoc.csv}. 42 USC 415(a)(1)(C)(i)}
#'   \item{special_min_rate}{COLA-adjusted special minimum PIA rate per year of
#'     coverage over 10. Base: $11.50 in 1979, adjusted by each year's COLA and
#'     rounded down to nearest $0.10. 42 USC 415(a)(1)(C)(i)}
#'   \item{min_yoc_for_special_min}{Minimum years of coverage to qualify for
#'     special minimum PIA: 11. 42 USC 415(a)(1)(C)(i)}
#'   \item{fm_bp1}{Family maximum first bend point. Indexed from 1979 base ($230)
#'     using AWI_{Y-2} / AWI_1977. Historical from
#'     \code{inst/extdata/family_max_bp.csv} (assembled from SSA historical
#'     program data). 42 USC 403(a)(1)}
#'   \item{fm_bp2}{Family maximum second bend point. 1979 base: $332. Same
#'     indexing as fm_bp1. 42 USC 403(a)(1)}
#'   \item{fm_bp3}{Family maximum third bend point. 1979 base: $433. Same
#'     indexing as fm_bp1. 42 USC 403(a)(1)}
#' }
#'
#' \strong{Program Rule Constants}
#'
#' Fixed parameters representing current-law rules. Stored in the assumptions
#' data frame so they can be modified for policy reform modeling.
#'
#' \describe{
#'   \item{qc_required}{Quarters of coverage required for fully insured status:
#'     40. SSA Handbook \S 203}
#'   \item{elig_age_retired}{Earliest eligibility age for retirement benefits:
#'     62. SSA Handbook \S 300}
#'   \item{index_age_offset}{Offset for wage indexing year: 2. Indexing age =
#'     elig_age - offset (age 60). SSA Handbook \S 700.3}
#'   \item{max_dropout_years}{Maximum dropout years in AIME computation: 5.
#'     SSA Handbook \S 703}
#'   \item{min_comp_period}{Minimum AIME computation period in years: 2.
#'     SSA Handbook \S 703}
#'   \item{max_qc_per_year}{Maximum quarters of coverage per year: 4.
#'     SSA Handbook \S 212}
#'   \item{max_drc_age}{Maximum age for delayed retirement credit accrual: 70.
#'     DRC months = (max_drc_age - NRA) * 12, computed dynamically in
#'     \code{rf_and_drc()}. 42 USC 402(w); SSA Handbook \S 720}
#'   \item{ret_phaseout_rate}{RET phaseout rate: 0.5 ($1 withheld per $2 of
#'     excess earnings). SSA Handbook \S 1803}
#' }
#'
#' \strong{Reform Scaffolding}
#'
#' Parameters that support policy reform modeling via
#' \code{\link{calculate_benefits_reform}}. Default values represent current law
#' (no reform). See reform documentation for details on each parameter.
#'
#' \describe{
#'   \item{pia_multiplier}{PIA multiplier (default 1.0). Reform #1}
#'   \item{ret_enabled}{Whether RET applies (default TRUE). Reform #23}
#'   \item{bp3}{Third PIA bend point for 4-bracket formula (default NA). Reforms #3, #12--14}
#'   \item{fact4}{Fourth PIA replacement factor (default NA). Reforms #3, #12--14}
#'   \item{cola_cap}{Median PIA threshold for COLA cap (default NA). Loaded from
#'     \code{inst/extdata/cola_cap_median.csv}. Reform #9}
#'   \item{cola_cap_active}{Whether COLA cap applies (default FALSE). Reform #9}
#'   \item{bmb_individual}{Monthly Basic Minimum Benefit, individual (default NA). Reform #27}
#'   \item{bmb_couple}{Monthly Basic Minimum Benefit, couple (default NA). Reform #27}
#'   \item{mini_pia_blend}{Mini-PIA blend factor, 0--1 (default 0). Reform #22}
#'   \item{flat_benefit}{Monthly flat benefit floor (default NA). Reform #2}
#'   \item{taxmax_tax}{Taxable max for payroll tax (default = taxmax). Reform #14}
#'   \item{taxmax_benefit}{Taxable max for benefit calc (default = taxmax). Reform #14}
#'   \item{child_care_credit_active}{Whether child care credits apply (default FALSE). Reform #29}
#'   \item{max_child_care_years}{Max years of child care credit (default 5). Reform #29}
#'   \item{child_care_earnings_rate}{Fraction of AWI credited (default 0.5). Reform #29}
#'   \item{widow_75_pct_active}{Whether 75\% combined widow benefit applies (default FALSE). Reform #28}
#' }
#'
#' @section Projection Formulas:
#' Where raw data contains NA values for future years, \code{prep_assumptions()}
#' projects parameters using statutory indexing rules
#' (\url{https://www.ssa.gov/OP_Home/comp2/G-APP-A.html}):
#' \itemize{
#'   \item \strong{taxmax}: round(base_1994 * AWI_{Y-2} / AWI_1992 / 300) * 300,
#'     floored at previous year
#'   \item \strong{bp1, bp2}: round(base_1979 * AWI_{Y-2} / AWI_1977)
#'   \item \strong{qc_rec}: round(base_1978 * AWI_{Y-2} / AWI_1976 / 10) * 10,
#'     floored at previous year
#'   \item \strong{ret1}: floor(base_1994 * AWI_{Y-2} / AWI_1992 * 10) / 10,
#'     floored at previous year
#'   \item \strong{ret2}: floor(base_2002 * AWI_{Y-2} / AWI_2000 * 10) / 10,
#'     floored at previous year
#' }
#'
#' @section Hypothetical Worker Birth Date Convention:
#' SSA's hypothetical workers (scaled earners) are assumed to be born on
#' \strong{January 2} of their birth year. This convention has important
#' implications for wage indexing:
#' \itemize{
#'   \item A worker born January 2 "attains" each age on January 1 of the
#'     following year (SSA uses the day before the birthday for age attainment)
#'   \item The indexing year is the year the worker turns 60
#'   \item For a 1960 birth cohort: indexing year = 2020 (turns 60 in 2020)
#'   \item Earnings through age 59 are indexed to the 2020 AWI
#'   \item Earnings at age 60+ are used at nominal value (no indexing)
#' }
#' This matches SSA's methodology in actuarial publications and Table V.C7.
#'
#' @source Social Security Administration, 2025 Trustees Report.
#' Raw data: \code{inst/extdata/2025TR_assumptions.csv}.
#' Supplementary files: \code{inst/extdata/cola.csv},
#' \code{inst/extdata/yoc.csv}, \code{inst/extdata/family_max_bp.csv},
#' \code{inst/extdata/cola_cap_median.csv}.
#' @seealso \code{\link{prep_assumptions}} for the function that processes this data
#' @examples
#' # Load and view the data
#' data(tr2025)
#' head(tr2025)
#'
#' # Use in benefit calculations
#' \dontrun{
#' worker <- earnings_generator(
#'   birth_yr = 1960,
#'   type = "medium",
#'   age_claim = 67,
#'   factors = sef2025,
#'   assumptions = tr2025
#' )
#' }
"tr2025"

#' 2025 Scaled Earnings Factors
#'
#' A dataset containing scaled earnings factors from the 2025 Social Security
#' Trustees Report. These factors are used to generate stylized lifetime
#' earnings profiles for different worker types.
#'
#' The factors represent the ratio of a worker's earnings to the Average Wage
#' Index (AWI) at each age, based on actual earnings patterns observed in
#' Social Security administrative data.
#'
#' @name sef2025
#' @docType data
#' @usage data(sef2025)
#' @format A data frame with the following variables:
#' \describe{
#'   \item{age}{Age of the worker (21-64)}
#'   \item{worker}{Worker type: "raw", "very_low", "low", "medium", "high", or "max"}
#'   \item{factor}{Scaled earnings factor (ratio of earnings to AWI)}
#' }
#'
#' @details
#' Worker types correspond to different earnings levels relative to the AWI:
#' \itemize{
#'   \item \strong{raw}: Raw unadjusted factors (used for custom earnings calculations)
#'   \item \strong{very_low}: Very low earner (approximately 25 percent of AWI)
#'   \item \strong{low}: Low earner (approximately 45 percent of AWI)
#'   \item \strong{medium}: Medium earner (approximately 100 percent of AWI)
#'   \item \strong{high}: High earner (approximately 160 percent of AWI)
#'   \item \strong{max}: Maximum earner (earnings at or above taxable maximum)
#' }
#'
#' @source Social Security Administration, 2025 Trustees Report
#' @seealso \code{\link{earnings_generator}} for how these factors are used
#' @examples
#' # Load and view the data
#' data(sef2025)
#' head(sef2025)
#'
#' # View factors for medium earner
#' sef2025[sef2025$worker == "medium", ]
"sef2025"
