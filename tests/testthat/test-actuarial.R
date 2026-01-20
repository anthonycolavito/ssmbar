# Unit tests for rf_and_drc() actuarial adjustment function
# Tests actuarial reduction factors and delayed retirement credits

# SSA rules for someone with NRA of 67 (born 1960 or later):
# - rf1: 5/9 of 1% per month for first 36 months early (5/900 = 0.005556)
# - rf2: 5/12 of 1% per month for months 37-60 early (5/1200 = 0.004167)
# - drc: 8% per year = 2/3 of 1% per month (0.006667) for delayed claiming

# Standard actuarial factors:
RF1 <- 5 / 900   # 0.005555556
RF2 <- 5 / 1200  # 0.004166667
DRC <- 0.08 / 12 # 0.006666667

test_that("Claiming at NRA gives factor of 1.0", {
  # When claim_age equals NRA, there should be no adjustment
  result <- rf_and_drc(
    claim_age = 67,
    nra = 67,
    rf1 = RF1,
    rf2 = RF2,
    drc = DRC
  )
  expect_equal(result, 1.0)
})

test_that("Claiming at 62 with NRA 67 gives factor of 0.70", {
  # 60 months early: 36 months at rf1, 24 months at rf2
  # Reduction = (36 * 5/900) + (24 * 5/1200) = 0.20 + 0.10 = 0.30
  # Factor = 1 - 0.30 = 0.70
  result <- rf_and_drc(
    claim_age = 62,
    nra = 67,
    rf1 = RF1,
    rf2 = RF2,
    drc = DRC
  )
  expect_equal(result, 0.70, tolerance = 1e-10)
})

test_that("Claiming at 70 with NRA 67 gives factor of 1.24", {
  # 36 months late: DRCs apply
  # DRC = 36 * (8%/12) = 36 * 0.006667 = 0.24
  # Factor = 1 + 0.24 = 1.24
  result <- rf_and_drc(
    claim_age = 70,
    nra = 67,
    rf1 = RF1,
    rf2 = RF2,
    drc = DRC
  )
  expect_equal(result, 1.24, tolerance = 1e-10)
})

test_that("DRCs cap at age 70 (36 months of credits)", {
  # Claiming at 71 should give same factor as claiming at 70
  # because DRCs don't accrue past age 70
  result_70 <- rf_and_drc(
    claim_age = 70,
    nra = 67,
    rf1 = RF1,
    rf2 = RF2,
    drc = DRC
  )
  result_71 <- rf_and_drc(
    claim_age = 71,
    nra = 67,
    rf1 = RF1,
    rf2 = RF2,
    drc = DRC
  )
  # Both should equal 1.24 (36 months of DRC)
  expect_equal(result_70, result_71)
  expect_equal(result_70, 1.24, tolerance = 1e-10)
})

test_that("Claiming 1 year early gives correct reduction", {
  # 12 months early, all at rf1 rate
  # Reduction = 12 * 5/900 = 0.06667
  # Factor = 1 - 0.06667 = 0.93333
  result <- rf_and_drc(
    claim_age = 66,
    nra = 67,
    rf1 = RF1,
    rf2 = RF2,
    drc = DRC
  )
  expected <- 1 - (12 * RF1)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("Claiming 3 years early uses only rf1", {
  # 36 months early, all at rf1 rate (boundary case)
  # Reduction = 36 * 5/900 = 0.20
  # Factor = 1 - 0.20 = 0.80
  result <- rf_and_drc(
    claim_age = 64,
    nra = 67,
    rf1 = RF1,
    rf2 = RF2,
    drc = DRC
  )
  expected <- 1 - (36 * RF1)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("Claiming 1 year late gives correct DRC", {
  # 12 months late
  # DRC = 12 * 0.006667 = 0.08
  # Factor = 1.08
  result <- rf_and_drc(
    claim_age = 68,
    nra = 67,
    rf1 = RF1,
    rf2 = RF2,
    drc = DRC
  )
  expected <- 1 + (12 * DRC)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("rf_and_drc is vectorized", {
  # Test that function works with vector inputs
  claim_ages <- c(62, 67, 70)
  nras <- c(67, 67, 67)

  result <- rf_and_drc(
    claim_age = claim_ages,
    nra = nras,
    rf1 = RF1,
    rf2 = RF2,
    drc = DRC
  )

  expect_equal(length(result), 3)
  expect_equal(result[1], 0.70, tolerance = 1e-10)  # Early claiming
  expect_equal(result[2], 1.00, tolerance = 1e-10)  # At NRA
  expect_equal(result[3], 1.24, tolerance = 1e-10)  # Delayed claiming
})

test_that("Different NRAs work correctly", {
  # NRA of 66 (born 1943-1954)
  # Claiming at 62 = 48 months early: 36 at rf1, 12 at rf2
  result <- rf_and_drc(
    claim_age = 62,
    nra = 66,
    rf1 = RF1,
    rf2 = RF2,
    drc = DRC
  )
  expected <- 1 - (36 * RF1) - (12 * RF2)
  expect_equal(result, expected, tolerance = 1e-10)
})
