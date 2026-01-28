# Data Documentation for ssmbar
# This file contains roxygen2 documentation for package data objects.

#' 2025 Social Security Trustees Report Assumptions
#'
#' A dataset containing economic assumptions and program parameters from the
#' 2025 Social Security Trustees Report, processed through \code{prep_assumptions()}.
#'
#' This dataset includes historical values and projections for key economic
#' variables and Social Security program parameters used in benefit calculations.
#'
#' @name tr2025
#' @docType data
#' @usage data(tr2025)
#' @format A data frame with the following variables:
#' \describe{
#'   \item{year}{Calendar year}
#'   \item{awi}{Average Wage Index}
#'   \item{cpi_w}{Consumer Price Index for Urban Wage Earners (CPI-W)}
#'   \item{gdp_pi}{GDP Price Index}
#'   \item{df}{Discount factor}
#'   \item{taxmax}{Social Security taxable maximum earnings}
#'   \item{oasi_tr}{OASI tax rate}
#'   \item{di_tr}{DI tax rate}
#'   \item{bp1}{First bend point for PIA formula}
#'   \item{bp2}{Second bend point for PIA formula}
#'   \item{qc_rec}{Earnings required for one quarter of coverage}
#'   \item{fact1}{PIA formula factor for earnings below first bend point (0.90)}
#'   \item{fact2}{PIA formula factor for earnings between bend points (0.32)}
#'   \item{fact3}{PIA formula factor for earnings above second bend point (0.15)}
#'   \item{rf1}{Early retirement reduction factor (first 36 months)}
#'   \item{rf2}{Early retirement reduction factor (months beyond 36)}
#'   \item{drc}{Delayed retirement credit rate}
#'   \item{nra}{Normal Retirement Age}
#'   \item{ret1}{Retirement Earnings Test exempt amount (under NRA)}
#'   \item{ret2}{Retirement Earnings Test exempt amount (year of NRA)}
#'   \item{s_pia_share}{Spousal PIA share (0.50)}
#'   \item{s_rf1}{Spousal early retirement reduction factor (first 36 months)}
#'   \item{s_rf2}{Spousal early retirement reduction factor (months beyond 36)}
#'   \item{qc_required}{Quarters of coverage required for fully insured status (40)}
#'   \item{elig_age_retired}{Earliest age for retirement benefit eligibility (62)}
#'   \item{index_age_offset}{Offset for wage indexing year; indexing age = elig_age - offset (2)}
#'   \item{max_dropout_years}{Maximum dropout years in AIME computation period (5)}
#'   \item{min_comp_period}{Minimum computation period in years for AIME calculation (2)}
#'   \item{max_qc_per_year}{Maximum quarters of coverage that can be earned per year (4)}
#'   \item{drc_max_months}{Maximum months of delayed retirement credits (36)}
#'   \item{ret_phaseout_rate}{RET phaseout rate - reduction per dollar of excess earnings (0.5)}
#'   \item{cola}{Cost-of-Living Adjustment percentage for the year (e.g., 2.8 for 2.8\%).
#'     Historical values from SSA publications; projected values calculated from CPI-W ratios.
#'     The COLA announced in year Y applies to benefits starting in January Y+1.}
#'   \item{le_m}{Male cohort life expectancy at age 65 (years remaining)}
#'   \item{le_f}{Female cohort life expectancy at age 65 (years remaining)}
#' }
#'
#' @section TODO - Documentation:
#' The following items need documentation review:
#' \itemize{
#'   \item Verify le_m and le_f data source citations
#' }
#'
#' @source Social Security Administration, 2025 Trustees Report
#' @seealso \code{\link{prep_assumptions}} for the function used to process this data
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
