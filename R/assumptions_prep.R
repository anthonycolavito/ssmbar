
prep_assumptions <- function(dataset) {

  #Rules for Updating Parameters: https://www.ssa.gov/OP_Home/comp2/G-APP-A.html
  # Tax max: Indexed forward from 1994 (1992 AWI) and rounded to the nearest $300
  # Bend points: Indexed forward from 1979 (1977 AWI) and rounded to the nearest dollar
  # QCs: Indexed forward from 1978 (1976 AWI) and rounded to the nearest dollar
  # RET Exempt Amount 1: Indexed forward from 1994 (1992 AWI) and rounded to the lowest $10
  # Ret Exempt Amount 2: Indexed forward from 2002 (2000 AWI) and rounded to the lowest $10
  # Old Law Base: Indexed from 1994 (1992 AWI) and rounded to the nearest $300
  # Special Minimum PIA Base Rate: $11.50 in 1979, COLA'd out each year

  assume <- dataset
  
  # Extend the projection horizon by 10 years.
  # AWI, GDP price index, CPI-W, and COLA are set explicitly; derived
  # parameters are wiped so the projection loop below fills them in.
  # Extend the projection horizon by 50 years.
  # AWI, GDP price index, CPI-W, and COLA are set explicitly; derived
  # parameters are wiped so the projection loop below fills them in.
  if (max(assume$year) < 2150) {
    last_yr  <- max(assume$year)
    ext_yrs  <- (last_yr + 1):2150
    n_ext    <- length(ext_yrs)
    last_row <- assume[assume$year == last_yr, ]
    
    extension <- last_row[rep(1, n_ext), ]
    extension$year   <- ext_yrs
    extension$awi    <- last_row$awi    * 1.0355^(ext_yrs - last_yr)
    extension$gdp_pi <- last_row$gdp_pi * 1.0205^(ext_yrs - last_yr)
    extension$cpi_w  <- last_row$cpi_w  * 1.024 ^(ext_yrs - last_yr)
    extension$cola   <- 0.024
    
    extension$taxmax           <- NA_real_
    extension$bp1              <- NA_real_
    extension$bp2              <- NA_real_
    extension$qc_rec           <- NA_real_
    extension$ret1             <- NA_real_
    extension$ret2             <- NA_real_
    extension$old_law_base     <- NA_real_
    extension$special_min_rate <- NA_real_
    extension$payable <- assume$payable[which(assume$year == 2100)]
    
    assume <- rbind(assume, extension)
  }

  #AWI bases
  awi_1976 <- assume[assume$year == 1976, "awi"] #Used for QC req indexing
  awi_1977 <- assume[assume$year == 1977, "awi"] #Used for BP indexing
  awi_1992 <- assume[assume$year == 1992, "awi"] #Used for tax max, RET 1, and old law contribution base indexing
  awi_2000 <- assume[assume$year == 2000, "awi"] #Used for RET 2 indexing.

  #Parameter base amounts
  qc_base <- assume[assume$year == 1978, "qc_rec"]
  bp1_base <- assume[assume$year == 1979, "bp1"]
  bp2_base <- assume[assume$year == 1979, "bp2"]
  taxmax_base <- assume[assume$year == 1994, "taxmax"]
  ret1_base <- assume[assume$year == 1994, "ret1"]
  ret2_base <- assume[assume$year == 2002, "ret2"]
  old_law_base_base <- assume[assume$year == 1994, "old_law_base"]

  for (i in 1979:max(assume$year)) {

    #Gather parameters
    taxmax_i <- assume[assume$year == i, "taxmax"]
    bp1_i <- assume[assume$year == i, "bp1"]
    bp2_i <- assume[assume$year == i, "bp2"]
    qc_rec_i <- assume[assume$year == i, "qc_rec"]
    ret1_i <- assume[assume$year == i, "ret1"]
    ret2_i <- assume[assume$year == i, "ret2"]
    old_law_base_i <- assume[assume$year == i, "old_law_base"]
    spec_min_rate_i <- assume[assume$year == i, "special_min_rate"]

    awi_end <- assume[assume$year == i - 2, "awi"]

    # Project out tax max
    if (is.na(taxmax_i)){

      taxmax_i <- round((taxmax_base * awi_end / awi_1992)/300)*300 

      assume[assume$year == i, "taxmax"] <- taxmax_i

    }

    #Project out bend points
    if (is.na(bp1_i)) {

      bp1_i <- round(bp1_base * awi_end / awi_1977)
      bp2_i <- round(bp2_base * awi_end / awi_1977)

      assume[assume$year == i, "bp1"] <- bp1_i
      assume[assume$year == i, "bp2"] <- bp2_i
    }

    #Project out QC requirements
    if (is.na(qc_rec_i)) {
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
    
    #Project out old law contribution base
    if (is.na(old_law_base_i)) {
      
      old_law_base_i <- round((old_law_base_base * awi_end / awi_1992)/300)*300 #Tax max is not allowed to decline from the previous year
      
      assume[assume$year == i, "old_law_base"] <- old_law_base_i
    }
    
    #Project out special_min_pia bases
    if (is.na(spec_min_rate_i)) {
      # Per 42 USC 415(a)(1)(C)(i): $11.50 per year of coverage over 10, COLA-adjusted
      # The $11.50 was established in 1979 and is adjusted by each year's COLA (which comes from the previous year).
      # Rounding: Per 42 USC 415(a)(2)(C), round to next lower $0.10 after each COLA
      
      prev_smb <- assume$special_min_rate[assume$year == i-1]
      cola_i <- assume$cola[assume$year == i-1]
      
      cur_smb <- floor(prev_smb * (1+cola_i) * 10)/10
      
      assume[assume$year == i, "special_min_rate"] <- cur_smb
      
    }

  }
  
  # Years of coverage threshold (from SSA OACT):
  # - 1951-1978: 25% of contribution and benefit base
  # - 1979-1990: 25% of old-law contribution base
  # - 1991+: 15% of old-law contribution base
  assume$yoc_threshold <- ifelse(assume$year < 1991,
                                 assume$old_law_base * 0.25,
                                 assume$old_law_base * 0.15)
  
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

  # Quarters of Coverage required for fully insured status
  # SSA Handbook Section 203: https://www.ssa.gov/OP_Home/handbook/handbook.02/handbook-0203.html
  assume$qc_required <- 40

  # Earliest eligibility age for retirement benefits
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

  # Minimum years of coverage required for special minimum PIA
  assume$min_yoc_for_special_min <- 11

  return(assume)

}
