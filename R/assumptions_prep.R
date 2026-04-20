
prep_assumptions <- function(dataset) {

  #Rules for Updating Parameters: https://www.ssa.gov/OP_Home/comp2/G-APP-A.html
  # Tax max: Indexed forward from 1994 (1992 AWI) and rounded to the nearest $300
  # Bend points: Indexed forward from 1979 (1977 AWI) and rounded to the nearest dollar
  # QCs: Indexed forward from 1978 (1976 AWI) and rounded to the nearest dollar
  # RET Exempt Amount 1: Indexed forward from 1994 (1992 AWI) and rounded to the lowest $10
  # Ret Exempt Amount 2: Indexed forward from 2002 (2000 AWI) and rounded to the lowest $10

  assume <- dataset

  #AWI bases
  awi_1976 <- assume[assume$year == 1976, "awi"] #Used for QC req indexing
  awi_1977 <- assume[assume$year == 1977, "awi"] #Used for BP indexing
  awi_1992 <- assume[assume$year == 1992, "awi"] #Used for tax max and RET 1 indexing
  awi_2000 <- assume[assume$year == 2000, "awi"] #Used for RET 2 indexing.

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

    # Project out RET exempt amounts
    # Per 42 USC 403(f)(8)(A): ret1 rounds to nearest $120, ret2 rounds to nearest $480
    if (is.na(ret1_i)) {
      prev_ret1 <- assume[assume$year == i - 1, "ret1"]

      ret1_i <- max(round(ret1_base * awi_end / awi_1992 / 120) * 120, prev_ret1)

      assume[assume$year == i, "ret1"] <- ret1_i
    }

    if (is.na(ret2_i)) {
      prev_ret2 <- assume[assume$year == i - 1, "ret2"]

      ret2_i <- max(round(ret2_base * awi_end / awi_2000 / 480) * 480, prev_ret2)

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
  assume$eea <- 62

  # Offset for wage indexing year (indexing year = elig_age - offset)
  # Earnings are indexed to AWI two years before eligibility age (age 60 for age-62 eligibility)
  # SSA Handbook Section 700.3: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0700.html
  assume$index_age_offset <- 2

  # Maximum dropout years in AIME computation period
  # SSA Handbook Section 703: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0703.html
  assume$max_dropout_years <- 5

  # Minimum computation period (years) for AIME calculation
  # SSA Handbook Section 703: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0703.html
  assume$min_comp_period <- 2

  # Maximum quarters of coverage that can be earned per year
  # SSA Handbook Section 212: https://www.ssa.gov/OP_Home/handbook/handbook.02/handbook-0212.html
  assume$max_qc_per_year <- 4

  # Maximum age for delayed retirement credits (DRCs stop accruing at age 70)
  # 42 USC 402(w); SSA Handbook Section 720
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0720.html
  # DRC months = (max_drc_age - NRA) * 12, computed dynamically in rf_and_drc()
  assume$max_drc_age <- 70

  # Retirement Earnings Test phaseout rate (reduction per dollar of excess earnings)
  # SSA Handbook Section 1803: https://www.ssa.gov/OP_Home/handbook/handbook.18/handbook-1803.html
  # $1 withheld for every $2 of excess earnings = 0.5 rate
  assume$ret_phaseout_rate <- 0.5

  

  # =============================================================================
  # Years of Coverage (YOC) for Special Minimum PIA
  # =============================================================================
  # Per 42 USC 415(a)(1)(C)(i), special minimum PIA = $11.50 × (years of coverage - 10)
  # where $11.50 is COLA-adjusted from its 1979 base.
  #
  # Years of coverage threshold (from SSA OACT):
  # - 1951-1978: 25% of contribution and benefit base
  # - 1979-1990: 25% of old-law contribution base
  # - 1991+: 15% of old-law contribution base
  #
  # Old-law contribution base projection (from SSA):
  # max(45000 * AWI_2yrs_before / AWI_1992, current_old_law_base), rounded to $300

  assume$yoc_threshold <- NA_real_
  assume$old_law_base <- NA_real_
  assume$special_min_rate <- NA_real_

  # Try to load YOC data
  yoc_file <- system.file("extdata", "yoc.csv", package = "ssmbar")
  if (yoc_file == "") {
    # Try relative path for development
    if (file.exists("inst/extdata/yoc.csv")) {
      yoc_file <- "inst/extdata/yoc.csv"
    }
  }

  if (file.exists(yoc_file)) {
    yoc_hist <- read.csv(yoc_file)
    names(yoc_hist) <- c("year", "old_law_base", "yoc_threshold")

    # Merge historical YOC data
    for (i in seq_len(nrow(assume))) {
      yr <- assume$year[i]
      hist_row <- yoc_hist[yoc_hist$year == yr, ]
      if (nrow(hist_row) == 1) {
        assume$old_law_base[i] <- hist_row$old_law_base
        assume$yoc_threshold[i] <- hist_row$yoc_threshold
      }
    }
  }

  # Project old-law base and YOC threshold for future years
  # Old-law base: max(45000 * AWI_end / AWI_1992, prev_old_law_base), rounded to $300
  old_law_base_1994 <- 45000
  for (i in seq_len(nrow(assume))) {
    yr <- assume$year[i]
    if (yr >= 1994 && is.na(assume$old_law_base[i])) {
      awi_end <- assume$awi[assume$year == yr - 2]
      prev_old_law <- assume$old_law_base[assume$year == yr - 1]

      if (length(awi_end) == 1 && !is.na(awi_end) && length(prev_old_law) == 1 && !is.na(prev_old_law)) {
        # Project old-law base: max of formula result or previous year, rounded to $300
        projected <- round(old_law_base_1994 * awi_end / awi_1992 / 300) * 300
        assume$old_law_base[i] <- max(projected, prev_old_law)

        # YOC threshold: 15% of old-law base (post-1990 rule)
        assume$yoc_threshold[i] <- assume$old_law_base[i] * 0.15
      }
    }
  }

  # =============================================================================
  # Special Minimum PIA Rate (COLA-adjusted)
  # =============================================================================
  # Per 42 USC 415(a)(1)(C)(i): $11.50 per year of coverage over 10, COLA-adjusted
  # The $11.50 was established in 1979 and is adjusted by each year's COLA.
  # Rounding: Per 42 USC 415(a)(2)(C), round to next lower $0.10 after each COLA

  special_min_base <- 11.50  # 1979 base amount per year of coverage over 10

  # Calculate COLA-adjusted rate for each year
  # The rate for year Y is the 1979 base adjusted by COLAs through year Y-1
  for (i in seq_len(nrow(assume))) {
    yr <- assume$year[i]
    if (yr < 1979) {
      # Before 1979, special minimum didn't exist in current form
      assume$special_min_rate[i] <- NA_real_
    } else if (yr == 1979) {
      # 1979 is the base year
      assume$special_min_rate[i] <- special_min_base
    } else {
      # Apply COLA from previous year and round to $0.10
      prev_rate <- assume$special_min_rate[i - 1]
      cola_pct <- assume$cola[i]  # COLA applied in December of year i, effective for year i+1

      if (!is.na(prev_rate) && !is.na(cola_pct)) {
        # Apply COLA and round to next lower $0.10
        new_rate <- prev_rate * (1 + cola_pct / 100)
        assume$special_min_rate[i] <- floor(new_rate * 10) / 10
      } else {
        assume$special_min_rate[i] <- prev_rate
      }
    }
  }

  # Minimum years of coverage required for special minimum PIA
  assume$min_yoc_for_special_min <- 11

  return(assume)

}
