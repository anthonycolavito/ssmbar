#' Prepare Program Assumptions
#'
#' Function that imports the Social Security Trustees Report's assumptions and
#' sets and projections Social Security's main parameters.
#'
#' @param dataset Dataframe with the latest Social Security Trustees Report assumptions.
#'
#' @return assume Dataframe with TR assumptions and projected program parameters.
#' @examples
#' \dontrun{
#' tr2025 <- prep_assumptions(tr2025)
#' }
#'
#' @export

prep_assumptions <- function(dataset) {

  #Rules for Updating Parameters: https://www.ssa.gov/OP_Home/comp2/G-APP-A.html
  # Tax max: Indexed forward from 1994 (1992 AWI) and rounded to the nearest $300
  # Bend points: Indexed forward from 1979 (1977 AWI) and rounded to the nearest dollar
  # QCs: Indexed forward from 1978 (1976 AWI) and rounded to the nearest dollar
  # RET Exempt Amount 1: Indexed forward from 1994 (1992 AWI) and rounded to the lowest $10
  # Ret Exempt Amount 2: Indexed forward from 2002 (2000 AWI) and rounded to the lowest $10

  assume <- dataset

  #AWI bases
  awi_1976 <- assume[assume$year == 1976, "awi"]
  awi_1977 <- assume[assume$year == 1977, "awi"]
  awi_1992 <- assume[assume$year == 1992, "awi"]
  awi_2000 <- assume[assume$year == 2000, "awi"]

  #Parameter base amounts
  qc_base <- assume[assume$year == 1978, "qc_rec"]
  bp1_base <- assume[assume$year == 1979, "bp1"]
  bp2_base <- assume[assume$year == 1979, "bp2"]
  taxmax_base <- assume[assume$year == 1994, "taxmax"]
  ret1_base <- assume[assume$year == 1994, "ret1"]
  ret2_base <- assume[assume$year == 2002, "ret2"]

  for (i in 1978:max(assume$year)) {

    #Gather parameters
    taxmax_i <- assume[assume$year == i, "taxmax"]
    bp1_i <- assume[assume$year == i, "bp1"]
    bp2_i <- assume[assume$year == i, "bp2"]
    qc_rec_i <- assume[assume$year == i, "qc_rec"]
    ret1_i <- assume[assume$year == i, "ret1"]
    ret2_i <- assume[assume$year == i, "ret2"]

    awi_end <- assume[assume$year == i - 2, "awi"]

    # Project out tax max
    if (is.na(taxmax_i) == T){

      prev_taxmax <- assume[assume$year == i - 1, "taxmax"]


      taxmax_i <- max(round((taxmax_base * awi_end / awi_1992)/300)*300, prev_taxmax) #Tax max is not allowed to decline from the previous year

      assume[assume$year == i, "taxmax"] <- taxmax_i

    }

    #Project out bend points
    if (is.na(bp1_i) == T) {

      bp1_i <- round(bp1_base * awi_end / awi_1977)
      bp2_i <- round(bp2_base * awi_end / awi_1977)

      assume[assume$year == i, "bp1"] <- bp1_i
      assume[assume$year == i, "bp2"] <- bp2_i
    }

    #Project out QC requirements
    if (is.na(qc_rec_i) == T) {
      prev_qc_rec <- assume[assume$year == i - 1, "qc_rec"]

      qc_rec_i <- max(round(qc_base * awi_end / awi_1976 / 10)*10,prev_qc_rec)

      assume[assume$year == i, "qc_rec"] <- qc_rec_i
    }

    #Project out RET exempt amounts
    if (is.na(ret1_i)) {
      prev_ret1 <- assume[assume$year == i - 1, "ret1"]

      ret1_i <- max(floor(ret1_base * awi_end / awi_1992 * 10) / 10, prev_ret1)

      assume[assume$year == i, "ret1"] <- ret1_i
    }

    if (is.na(ret2_i)) {
      prev_ret2 <- assume[assume$year == i - 1, "ret2"]

      ret2_i <- max(floor(ret2_base * awi_end / awi_2000 * 10) / 10, prev_ret2)

      assume[assume$year == i, "ret2"] <- ret2_i

    }

  }

  # Spousal PIA share
  # SSA Handbook Section 320: https://www.ssa.gov/OP_Home/handbook/handbook.03/handbook-0320.html
  # Spousal benefit is 50% of worker's PIA (before any reductions for early claiming)
  assume$s_pia_share <- 0.5

  # Spousal reduction factors
  # SSA Handbook Section 724: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  # s_rf1: 25/36 of 1% per month for first 36 months (vs 5/9 of 1% for workers)
  # s_rf2: 5/12 of 1% per month beyond 36 months (same as worker rf2)
  # Maximum spousal reduction is 35% at age 62 with NRA 67 (vs 30% for workers)
  assume$s_rf1 <- 25 / 36 / 100
  assume$s_rf2 <- assume$rf2

  #Retirement Earnings Test Exempt Amounts

  # =============================================================================
  # Program Rule Parameters
  # =============================================================================
  # These parameters represent Social Security program rules that are currently

  # constant but may change under policy reforms. Storing them in the assumptions
  # data frame allows for policy modeling by modifying these values.

  # Quarters of Coverage required for fully insured status
  # SSA Handbook Section 203: https://www.ssa.gov/OP_Home/handbook/handbook.02/handbook-0203.html
  assume$qc_required <- 40

  # Earliest age for retirement benefit eligibility
  # SSA Handbook Section 300: https://www.ssa.gov/OP_Home/handbook/handbook.03/handbook-0300.html
  assume$elig_age_retired <- 62

  # Offset for wage indexing year (indexing year = elig_age - offset)
  # Earnings are indexed to AWI two years before eligibility age (age 60 for age-62 eligibility)
  # SSA Handbook Section 700.3: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0700.html
  assume$index_age_offset <- 2

  # Maximum dropout years in AIME computation period
  # SSA Handbook Section 703: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0703.html
  assume$max_dropout_years <- 5

  # Minimum computation period (years) for AIME calculation
  # SSA Handbook Section 703
  assume$min_comp_period <- 2

  # Maximum quarters of coverage that can be earned per year
  # SSA Handbook Section 212: https://www.ssa.gov/OP_Home/handbook/handbook.02/handbook-0212.html
  assume$max_qc_per_year <- 4

  # Maximum months of delayed retirement credits (caps at age 70, which is 36 months past NRA 67)
  # SSA Handbook Section 720: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0720.html
  assume$drc_max_months <- 36

  # Retirement Earnings Test phaseout rate (reduction per dollar of excess earnings)
  # SSA Handbook Section 1803: https://www.ssa.gov/OP_Home/handbook/handbook.18/handbook-1803.html
  # $1 withheld for every $2 of excess earnings = 0.5 rate
  assume$ret_phaseout_rate <- 0.5

  return(assume)

}
