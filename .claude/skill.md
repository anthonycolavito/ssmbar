# ssmbar Package - Claude Skill Guide

## Package Purpose

**ssmbar** (Social Security Microsimulation Benefit Calculator) is an R package that calculates Social Security retirement benefits for hypothetical workers using the exact SSA formulas and parameters.

## Owner

Anthony Colavito (colavito@crfb.org) - Committee for a Responsible Federal Budget

## Goals (Priority Order)

1. **Benefit Calculation**: Calculate annual Social Security benefits for any hypothetical worker configuration through their lifetime
2. **Distributional Analysis**: Answer questions like "What replacement rate would a $50k earner born in 1960 vs 1980 receive?"
3. **Policy Reform Modeling** (Future): Model hypothetical SS reforms and their distributional impact

## Technical Architecture

### Benefit Calculation Pipeline
```
Earnings → AIME → PIA → COLA → Worker Benefit → Spousal PIA → Spouse Benefit
        → Widow PIA → Widow Benefit → RET → Final Benefit
```

### Key Functions

| Function | Purpose |
|----------|---------|
| `calculate_benefits()` | Convenience wrapper - full pipeline in one call |
| `earnings_generator()` | Creates lifetime earnings (vectorized for multiple workers) |
| `aime()` | Average Indexed Monthly Earnings |
| `pia()` | Primary Insurance Amount (bend point formula) |
| `spousal_pia()` | Spousal benefit calculation (uses spouse_spec) |
| `cola()` | Cost-of-Living Adjustments |
| `worker_benefit()` | Applies early retirement/delayed credits |
| `spouse_benefit()` | Spousal benefit with actuarial adjustments |
| `widow_pia()` | Survivor PIA from deceased spouse's record |
| `widow_benefit()` | Survivor benefit with widow actuarial adjustment |
| `ret()` | Retirement Earnings Test - reduces benefits if earnings > exempt amount |
| `final_benefit()` | Combines worker + max(spousal, survivor) benefits |

### Worker ID Format
`{type}-{sex}-{birthyr}-{claimage}`
- Examples: `"medium-male-1960-67"`, `"custom50000-female-1970-65"`

### spouse_spec Encoding
`{type}-{sex}-{birthyr}-{claimage}`
- Example: `"low-female-1962-65"`
- Stored as column in worker data; NA if no spouse
- Used for on-the-fly spousal PIA calculation

### Data Objects
- `tr2025` - Trustees Report assumptions (AWI, bend points, COLA factors, ret1/ret2, etc.)
- `sef2025` - Scaled Earnings Factors for worker types

### Worker Types
- `very_low`, `low`, `medium`, `high`, `max` - Preset SSA scaled workers
- `custom` - Specify average real earnings directly

### Sex Parameter
- `"male"`, `"female"`, `"all"` (gender-neutral)

## Usage Examples

```r
# Single worker
worker <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium",
  age_claim = 67, factors = sef2025, assumptions = tr2025
)

# Worker with spouse
couple <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "high", age_claim = 67,
  spouse_type = "low", spouse_sex = "female",
  spouse_birth_yr = 1962, spouse_age_claim = 65,
  factors = sef2025, assumptions = tr2025
)

# Multiple workers (vectorized)
multiple <- calculate_benefits(
  birth_yr = c(1960, 1970, 1980),
  sex = c("male", "female", "all"),
  type = c("low", "medium", "high"),
  age_claim = c(62, 67, 70),
  factors = sef2025, assumptions = tr2025
)

# Disabled worker (disabled at age 45)
disabled <- calculate_benefits(
  birth_yr = 1970, sex = "male", type = "medium",
  age_claim = 45, disabled_age = 45,  # Both set to disability age
  factors = sef2025, assumptions = tr2025
)
# Note: Disabled workers get act_factor = 1.0 (no reduction)
# Earnings stop at disability age, benefits start at disability age
```

## Development Notes

### System Paths (Work Computer - CRFB)
- **R 4.5.0**: `C:/Users/AnthonyColavito/AppData/Local/Programs/R/R-4.5.0/bin/R.exe`
- **Rscript**: `C:/Users/AnthonyColavito/AppData/Local/Programs/R/R-4.5.0/bin/Rscript.exe`
- **Package root**: `C:/Users/AnthonyColavito/ssmbar`
- **GitHub repo**: `https://github.com/anthonycolavito/ssmbar.git`
- **Rtools**: Not installed on this computer

### Package File Locations
- R source: `R/`
- Package data: `data/` (tr2025.rda, sef2025.rda)
- Raw data: `inst/extdata/`
- Data processing: `data-raw/process_data.R`
- Tests: `tests/testthat/`
- Test fixtures: `tests/testthat/fixtures/`

### Running R Commands
Use Git Bash path format with the Rscript executable:
```bash
cd /c/Users/AnthonyColavito/ssmbar
"/c/Users/AnthonyColavito/AppData/Local/Programs/R/R-4.5.0/bin/Rscript.exe" -e "devtools::test()"
"/c/Users/AnthonyColavito/AppData/Local/Programs/R/R-4.5.0/bin/Rscript.exe" -e "devtools::check()"
"/c/Users/AnthonyColavito/AppData/Local/Programs/R/R-4.5.0/bin/Rscript.exe" path/to/script.R
```

### Known Issues
- Rtools not installed on work computer (CRFB)

### R CMD Check Status
- Passes with 0 errors, 0 warnings, 2 notes (expected for source directory check)

### Working Rules
- Track all changes with meaningful git commits
- Never delete files without explicit permission
- Ask permission if uncertain about any action
- CLAUDE.md contains project context (in repo root)
- **Objective evaluation**: Do not be eager to validate the user's point of view. If the user is correct, confirm it. But if they may not be correct, explain how and why. Apply rigorous standards to all ideas and respectfully disagree when necessary - objective guidance is more valuable than false agreement.

## Recent Changes (Jan 2026)

1. Added vectorized inputs to `earnings_generator()` for multiple workers
2. Added `sex` parameter and updated ID format
3. Implemented `spouse_spec` encoding for on-the-fly spousal benefit calculation
4. Simplified `calculate_benefits()` by removing dual-path logic
5. Fixed `final_benefit()` bug where `ben` column was missing
6. Added proper NAMESPACE imports and global variable declarations
7. Added `ret1` and `ret2` (RET exempt amounts) to assumptions dataset
8. Implemented `ret()` function for Retirement Earnings Test:
   - Reduces benefits when earnings > ret1 (before NRA)
   - $1 reduction for every $2 of excess earnings
   - Handles workers with dependent spouses (spouse_spec)
   - Calculates DRC payback at NRA based on months withheld
9. Added `generate_spouse_dependent_benefit()` helper for RET calculations
10. Added `annual_ind` and `annual_couple` output columns to `calculate_benefits()`:
    - `annual_ind`: Individual worker's annual benefit (ben * 12)
    - `annual_couple`: Combined couple's annual benefit (worker + spouse's full benefit)
    - Added `calculate_spouse_full_benefit()` helper to compute spouse's total benefit

### Changes on 2026-01-23: Spousal Benefit Calculation Analysis & Reversion

**Summary:** Investigated an alternative spousal benefit calculation order, determined the ORIGINAL method is correct, and reverted all changes.

**Original (CORRECT) formula:**
```
spouse_pia = (50% × spouse_PIA - own_PIA)
spouse_ben = floor(spouse_pia × spousal_actuarial_factor)
```

**Alternative method tested (REVERTED):**
```
spouse_pia = 50% × spouse_PIA
spouse_ben = max(floor(spouse_pia × factor) - wrk_ben, 0)
```

**Key mathematical insight discovered:** The two methods produce different results because worker reduction factors (rf1, rf2) differ from spousal reduction factors (s_rf1, s_rf2). If the factors were identical, both methods would be mathematically equivalent.

**Added test cases:** Created 3 new spousal benefit test scenarios to catch future calculation order issues:
- Low earner early (62) with high spouse (67)
- Low earner NRA (67) with high spouse (67)
- Medium earner early (62) with medium spouse early (62)

**Tests:** All 76 tests passing (13 actuarial + 63 regression).

## RET (Retirement Earnings Test) Details

The `ret()` function applies the Retirement Earnings Test per SSA Handbook Chapter 18:

- **Who**: Workers with earnings > ret1 exempt amount, between claim_age and NRA
- **Reduction**: (earnings - ret1) / 2 annually
- **Spouse handling**: If worker has spouse_spec, includes spouse's dependent benefit in total pot; reduction allocated proportionally
- **DRC payback**: At NRA, actuarial factor is recalculated as if worker claimed later (by months withheld / 12)
- **Debug output**: `excess_earnings`, `ret_reduction`, `months_withheld`, `cum_months_withheld`, `ret_adj_factor`, `spouse_dep_ben`

---

## Refactoring To-Do List

This section contains specific, actionable items for improving the ssmbar package. Items are organized in **execution order** — complete them sequentially. Each item includes the specific files, line numbers, and code changes needed.

**Important**: Run `devtools::test()` and `devtools::check()` after completing each phase to catch regressions early.

---

### Phase 1: Establish Testing Baseline

Before any refactoring, create tests that verify current behavior. These tests will catch any unintended changes during refactoring.

#### 1.1 Create regression test data

- [x] **Create `tests/testthat/fixtures/` directory** for storing expected outputs.

- [x] **Generate baseline test cases**: Run the following code and save outputs as RDS files in `tests/testthat/fixtures/`:

```r
library(ssmbar)

# Test case 1: Medium earner, no spouse, claiming at 67
baseline_medium_67 <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)
saveRDS(baseline_medium_67, "tests/testthat/fixtures/medium_1960_claim67.rds")

# Test case 2: Low earner, early claiming at 62
baseline_low_62 <- calculate_benefits(
  birth_yr = 1960, sex = "female", type = "low", age_claim = 62,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)
saveRDS(baseline_low_62, "tests/testthat/fixtures/low_1960_claim62.rds")

# Test case 3: High earner, delayed claiming at 70
baseline_high_70 <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "high", age_claim = 70,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)
saveRDS(baseline_high_70, "tests/testthat/fixtures/high_1960_claim70.rds")

# Test case 4: Worker with spouse
baseline_with_spouse <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "high", age_claim = 67,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "low", spouse_sex = "female",
  spouse_birth_yr = 1962, spouse_age_claim = 65,
  debugg = TRUE
)
saveRDS(baseline_with_spouse, "tests/testthat/fixtures/high_1960_with_spouse.rds")

# Test case 5: Custom earnings
baseline_custom <- calculate_benefits(
  birth_yr = 1970, sex = "male", type = "custom", age_claim = 65,
  custom_avg_earnings = 50000,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)
saveRDS(baseline_custom, "tests/testthat/fixtures/custom_50k_1970.rds")

# Test case 6: Max earner
baseline_max <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "max", age_claim = 67,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)
saveRDS(baseline_max, "tests/testthat/fixtures/max_1960_claim67.rds")
```

#### 1.2 Create regression test file

- [x] **Create `tests/testthat/test-regression.R`** that loads each fixture and compares key columns (ben, earnings, aime, basic_pia, spouse_ben, spouse_pia) to current output. See full template in detailed notes below.

#### 1.3 Create unit tests for core calculations

- [x] **Create `tests/testthat/test-actuarial.R`** testing `rf_and_drc()`:
  - Claiming at 62 with NRA 67 should give factor of 0.70
  - Claiming at 70 with NRA 67 should give factor of 1.24
  - Claiming at NRA should give factor of 1.0
  - DRCs should cap at age 70

#### 1.4 Run and commit

- [x] **Run `devtools::test()`** — all tests must pass
- [x] **Commit**: "Add regression and unit tests to establish baseline before refactoring"

---

### Phase 2: Parameterize Hardcoded Constants

#### 2.1 Add new columns to assumptions

- [x] **Edit `R/assumptions_prep.R`**, add at end of `prep_assumptions()`:

```r
assume$qc_required <- 40
assume$elig_age_retired <- 62
assume$index_age_offset <- 2
assume$max_dropout_years <- 5
assume$min_comp_period <- 2
assume$max_qc_per_year <- 4
assume$drc_max_months <- 36
assume$ret_phaseout_rate <- 0.5
```

#### 2.2 Update data documentation

- [x] **Edit `R/data.R`**: Add roxygen documentation for each new column in the `tr2025` docblock.

#### 2.3 Regenerate data

- [x] **Run `source("data-raw/process_data.R")`**
- [x] **Verify**: `names(tr2025)` includes new columns
- [x] **Run `devtools::test()`** — should still pass
- [x] **Commit**: "Add parameterized program rules to assumptions data frame"

#### 2.4 Update eligibility.R

- [x] **Edit `qc_comp()`**: Change `pmin(..., 4)` to `pmin(..., max_qc_per_year)`.

- [x] **Edit `comp_period()`**: Change `pmin(5, ...)` to `pmin(max_dropout_years, ...)` and `pmax(2, ...)` to `pmax(min_comp_period, ...)`.

- [x] **Run `devtools::test()`**
- [x] **Commit**: "Parameterize QC and computation period in eligibility.R"

#### 2.5 Update benefit_calculations.R

- [x] **Edit `aime()`**: Change `>= 40` to `>= qc_required`. Fix indexing age calculation (was hardcoded age 60, now uses `elig_age - index_age_offset`). Renamed `awi_age60` to `awi_index_age`.

- [x] **Edit `pia()`**: Replace hardcoded `62` with `elig_age_retired` (stored as `elig_age_ret`).

- [x] **Edit `cola()`**: Replace `age == 62` with `age == elig_age_ret`.

- [x] **Edit `spousal_pia()`**: Replace `age >= 62` with `age >= elig_age_ret` in both branches.

- [x] **Edit `spouse_benefit()`**: Replace `yr_62` with `yr_elig` using `elig_age_retired`.

- [x] **Edit `rf_and_drc()`**: Add `drc_max_months = 36` parameter. Replace `36*drc` with `drc_max_months * drc`.

- [x] **Edit `ret()`**: Replace `/ 2` with `* ret_rate` (from `ret_phaseout_rate`). Use `elig_age_retired` for parameter lookup.

- [x] **Edit `generate_spouse_data()` and `generate_spouse_dependent_benefit()`**: Use `assumptions$elig_age_retired[1]` instead of hardcoded 62.

- [x] **Run `devtools::test()`**
- [x] **Commit**: "Parameterize eligibility age and other constants in benefit_calculations.R"

#### 2.6 Update earnings.R

- [x] **Add constants at top of file**:
```r
FIRST_WORKING_AGE <- 21
MAX_AGE <- 119
```

- [x] **Update `generate_single_worker()`**: Use constants instead of hardcoded 21 and 119.

- [x] **Run `devtools::test()`**
- [x] **Commit**: "Define working age constants in earnings.R"

#### 2.7 Update globalVariables

- [x] **Edit `R/ssmbar-package.R`**: Add new variable names to `globalVariables()`.
- [x] **Run `devtools::check()`**
- [x] **Commit**: "Update globalVariables with new parameter names"

---

### Phase 3: Refactor Spouse Handling

#### 3.1 Create R/spousal.R

- [x] **Create new file `R/spousal.R`**

- [x] **Move from benefit_calculations.R to spousal.R**:
  - `parse_spouse_spec()`
  - `spousal_pia()`
  - `spouse_benefit()`

- [x] **Create `generate_spouse()` in spousal.R**: Consolidates spouse generation. Takes `spouse_spec`, `factors`, `assumptions`. Returns data frame with `year`, `s_age`, `s_birth_yr`, `s_claim_age`, `s_pia` columns, or NULL.

- [x] **Delete from benefit_calculations.R**:
  - `generate_spouse_data()`
  - `generate_spouse_dependent_benefit()`

- [x] **Commit**: "Phase 3.1: Create R/spousal.R and move spouse functions" (c91fbd5)

#### 3.2 Simplify spousal functions

- [x] **Rewrite `spousal_pia()`**: Single code path using `spouse_data` parameter (list keyed by spouse_spec).

- [x] **Rewrite `spouse_benefit()`**: Single code path using `spouse_data` parameter.

- [x] **Added `calculate_spouse_dep_benefit()`**: Helper for RET to calculate spouse's dependent benefit based on worker's record.

- [x] **Run `devtools::test()`**: All 55 tests pass

#### 3.3 Update calculate_benefits() and ret()

- [x] **Edit `R/CL_benefit_calculator.R`**:
  - Generate spouse data ONCE at start using `generate_spouse()`
  - Pass `spouse_data` to `spousal_pia()`, `spouse_benefit()`, `ret()`

- [x] **Edit `R/benefit_calculations.R`**:
  - Updated `ret()` to accept `spouse_data` parameter
  - Uses `calculate_spouse_dep_benefit()` from spousal.R instead of internal function

- [x] **Run `devtools::test()`**: All 55 tests pass
- [x] **Commit**: "Phase 3: Refactor spouse handling - consolidate spouse data generation" (0a5697f)

---

### Phase 4: Decompose RET Function

#### 4.1 Create R/ret.R

- [x] **Create new file `R/ret.R`**

- [x] **Create helper functions**:
  - `calculate_excess_earnings(earnings, ret_threshold, age, claim_age, nra)`
  - `calculate_ret_reduction(excess_earnings, phaseout_rate, total_monthly_benefits)`
  - `allocate_ret_reduction(total_reduction, wrk_ben, spouse_ben, spouse_dep_ben)`
  - `calculate_months_withheld(annual_reduction, monthly_benefit, age, claim_age, nra)`
  - `calculate_drc_payback(claim_age, cum_months_withheld, nra, rf1, rf2, drc, s_rf1, s_rf2, drc_max_months)`

#### 4.2 Rewrite ret()

- [x] **Rewrite `ret()` to use helpers**: ~85 lines (was 183 lines). Uses spouse_data parameter.

- [x] **Move `ret()` from benefit_calculations.R to ret.R**

- [x] **Run `devtools::test()`**: All 55 tests pass
- [x] **Verify all intermediate calculations**: All 6 test cases pass (38-40 columns each)
- [x] **Commit**: "Phase 4: Decompose RET function into focused helpers in R/ret.R"

---

### Phase 5: Create Reform Infrastructure

#### 5.1 Create R/reform.R

- [x] **Create `create_reform()`**: S3 class constructor. Parameters: `name`, `description`, `parameters` (named list), `effective_year`, `phase_in_years`, `affected_cohorts`.

- [x] **Create `print.Reform()`**: Print method.

- [x] **Create `apply_reform(assumptions, reform)`**: Returns modified assumptions. Handle immediate, step change, and phase-in cases.

- [x] **Create `compare_benefits(baseline, reformed)`**: Returns data frame with `_baseline`, `_reform`, `_diff` columns.

- [x] **Create `reform_impact_summary(comparison)`**: Returns ReformImpact object with winners/losers/mean change stats.

- [x] **Create `print.ReformImpact()`**: Print method.

#### 5.2 Update calculate_benefits()

- [x] **Add `reform = NULL` parameter**
- [x] **At start**: `if (!is.null(reform)) assumptions <- apply_reform(assumptions, reform)`

#### 5.3 Create reform tests

- [x] **Create `tests/testthat/test-reform.R`**:
  - `create_reform()` validates parameters
  - `apply_reform()` modifies correct years
  - `apply_reform()` handles phase-in correctly
  - `compare_benefits()` calculates differences

- [x] **Run `devtools::test()`**
- [x] **Commit**: "Add reform infrastructure"

---

### Phase 6: Performance Optimization

#### 6.1 Benchmark

- [x] **Create `inst/benchmarks/benchmark_scaling.R`**: Test with 10, 100, 500, 1000 workers.
- [x] **Run and record baseline times**

#### 6.2 Optimize AIME

- [x] **Profile `aime()`** with profvis
- [x] **Rewrite loop** to avoid repeated sorting (O(n log n) instead of O(n²))
- [x] **Re-benchmark**
- [x] **Commit**: "Optimize AIME calculation"

#### 6.3 Reduce joins

- [x] **Create `join_all_assumptions()`** helper
- [x] **Call once in `calculate_benefits()`** before pipeline
- [x] **Update functions** to skip joins if columns present
- [x] **Re-benchmark**
- [x] **Commit**: "Reduce redundant joins"

#### 6.4 Bug Fix (discovered during benchmarking)

- [x] **Fix duplicate worker ID bug**: Workers with identical configurations got same ID
- [x] **Add numeric suffix** to duplicate IDs in earnings_generator()

---

### Phase 7: Documentation and Cleanup

#### 7.1 File organization

- [ ] **Rename `CL_benefit_calculator.R` to `calculate_benefits.R`**
- [ ] **Run `devtools::document()`**
- [ ] **Run `devtools::check()`**
- [ ] **Commit**: "Finalize file organization"

#### 7.2 Vignettes

- [ ] **Create `vignettes/calculating-benefits.Rmd`**
- [ ] **Create `vignettes/modeling-reforms.Rmd`**
- [ ] **Create `vignettes/replacement-rates.Rmd`**
- [ ] **Run `devtools::build_vignettes()`**
- [ ] **Commit**: "Add package vignettes"

#### 7.3 Validation

- [ ] **Create `R/validation.R`** with `validate_worker_params()`, `validate_assumptions()`
- [ ] **Add validation to exported functions**
- [ ] **Commit**: "Add parameter validation"

---

### Completed Items

**Phase 1: Establish Testing Baseline** - Completed 2026-01-20
- Created `tests/testthat/fixtures/` directory with 6 baseline RDS files
- Created `tests/testthat/test-regression.R` with 6 regression tests (48 assertions)
- Created `tests/testthat/test-actuarial.R` with 13 unit tests for `rf_and_drc()`
- All 61 tests passing
- Commit: 469edcd "Add regression and unit tests to establish baseline before refactoring"

**Decisions made:**
- Regression tests compare key columns: ben, earnings, aime, basic_pia, spouse_ben, spouse_pia
- Used tolerance of 1e-10 for floating point comparisons
- Actuarial tests use standard rf1=5/900, rf2=5/1200, drc=8%/12 values
- Kept generate_fixtures.R script in fixtures/ for future reference/regeneration

**Phase 2: Parameterize Hardcoded Constants** - Completed 2026-01-20
- Added 8 program rule parameters to assumptions_prep.R with full SSA Handbook references
- Updated eligibility.R to use max_qc_per_year, max_dropout_years, min_comp_period
- Updated benefit_calculations.R: aime(), pia(), cola(), spousal_pia(), spouse_benefit(), rf_and_drc(), ret()
- Fixed indexing age bug: was hardcoded age 60, now correctly uses (elig_age - index_age_offset)
- Renamed awi_age60 → awi_index_age (more descriptive for variable eligibility ages)
- Added FIRST_WORKING_AGE and MAX_AGE constants to earnings.R
- All 55 tests passing (regression tests regenerated with new column names)
- Commit: 8df17e1 "Phase 2: Parameterize hardcoded constants for policy reform modeling"

**Decisions made:**
- Did NOT add comp_period_base=35 because computation period is calculated (elapsed_years - dropout_years), not a fixed constant
- Added index_age_offset=2 to parameterize the wage indexing calculation (indexing age = elig_age - offset)
- Added min_comp_period=2 for the minimum computation period rule
- Removed ncol check from regression tests since column names changed (row count + key values still verified)
- Updated test fixtures with new column names (awi_index_age instead of awi_age60, added index_age)
- All SSA Handbook references added inline in code comments

**Phase 3: Refactor Spouse Handling** - Completed 2026-01-20
- Created R/spousal.R with parse_spouse_spec(), spousal_pia(), spouse_benefit(), generate_spouse(), calculate_spouse_dep_benefit()
- Deleted generate_spouse_data() and generate_spouse_dependent_benefit() from benefit_calculations.R
- Rewrote spousal_pia() and spouse_benefit() with unified code paths using spouse_data parameter
- Updated ret() to accept spouse_data parameter and use calculate_spouse_dep_benefit()
- Updated calculate_benefits() to generate spouse data ONCE and pass through pipeline
- Performance improvement: Spouse data is now generated once per unique spouse_spec instead of multiple times
- All 55 tests passing
- Commits: c91fbd5 "Phase 3.1: Create R/spousal.R and move spouse functions"
           0a5697f "Phase 3: Refactor spouse handling - consolidate spouse data generation"

**Decisions made:**
- spouse_data is passed as a list keyed by spouse_spec (e.g., {"low-female-1962-65": df, ...})
- generate_spouse() returns: year, s_age, s_birth_yr, s_claim_age, s_pia
- calculate_spouse_dep_benefit() calculates spouse's dependent benefit based on worker's record (for RET)
- Functions still support on-the-fly generation if spouse_data is NULL (for backward compatibility)

**Phase 4: Decompose RET Function** - Completed 2026-01-20
- Created R/ret.R with 5 helper functions:
  - calculate_excess_earnings(): Calculates excess earnings above RET threshold
  - calculate_ret_reduction(): Calculates reduction with phaseout rate, caps at annual benefits
  - allocate_ret_reduction(): Allocates reduction proportionally between worker/spouse benefits
  - calculate_months_withheld(): Calculates months of benefits withheld for DRC payback
  - calculate_drc_payback(): Calculates adjusted actuarial factors after withheld months
- Rewrote ret() from 183 lines to ~85 lines using helper functions
- Moved ret() from benefit_calculations.R to ret.R
- All 55 tests passing
- All 6 test cases verified with 38-40 intermediate calculation columns each
- Commit: "Phase 4: Decompose RET function into focused helpers in R/ret.R"

**Decisions made:**
- Helper functions are internal (@keywords internal), not exported
- Each helper has single responsibility and clear documentation
- ret() main function now follows clear 5-step process (commented in code)
- Maintained backward compatibility: spouse_data can be NULL for on-the-fly generation

**Spousal Benefit Calculation - Session 2026-01-23**

**CORRECT FORMULA (confirmed):**
```
spouse_pia = (50% × spouse_PIA - own_PIA)
spouse_ben = floor(spouse_pia × spousal_actuarial_factor)
```

This session initially attempted to change the calculation order, but after analysis it was determined the ORIGINAL method is correct.

**Attempted change (REVERTED):**
An alternative calculation order was tested:
- spouse_pia = 50% × spouse_PIA (no subtraction)
- spouse_ben = max(floor(spouse_pia × factor) - wrk_ben, 0)

This was reverted because the original formula is the correct SSA calculation.

**Key insight documented:** The two methods produce mathematically different results because worker reduction factors (rf1, rf2) differ from spousal reduction factors (s_rf1, s_rf2). If the factors were identical, both methods would produce the same result.

**Mathematical analysis (preserved for reference):**

Let:
- S = spouse's PIA, P = own PIA
- A_s = spousal actuarial factor, A_w = worker actuarial factor

Method 1 (CORRECT): `(0.5 × S - P) × A_s`
Method 2 (alternative): `(0.5 × S × A_s) - (P × A_w)`

These are equivalent only if A_s = A_w. Since rf1 ≠ s_rf1 and rf2 ≠ s_rf2, the methods produce different results.

**Files reverted:**
- `R/spousal.R`: spousal_pia(), spouse_benefit(), generate_spouse(), calculate_spouse_dep_benefit()
- `R/ret.R`: spouse_ben calculation at NRA

**Test fixtures regenerated:**
- high_1960_with_spouse.rds
- low_early_62_high_spouse_67.rds
- low_nra_67_high_spouse_67.rds
- medium_early_62_medium_spouse_62.rds

All 76 tests passing (13 actuarial + 63 regression).

**Phase 5: Create Reform Infrastructure** - Completed 2026-01-23
- Created `R/reform.R` with full reform modeling infrastructure:
  - `create_reform()`: S3 class constructor for policy reforms
  - `print.Reform()`: Print method for Reform objects
  - `apply_reform()`: Applies reform to assumptions (immediate or phase-in)
  - `apply_single_parameter()`: Internal helper for parameter modifications
  - `compare_benefits()`: Compares baseline vs reformed benefits
  - `reform_impact_summary()`: Calculates winners/losers/mean change statistics
  - `print.ReformImpact()`: Print method for impact summaries
  - `reform_raise_nra()`: Template for NRA increase reforms
  - `reform_benefit_formula()`: Template for benefit formula changes
  - `reform_benefit_cut()`: Template for across-the-board cuts
- Updated `calculate_benefits()` with `reform` parameter
- Created `tests/testthat/test-reform.R` with 67 tests
- All 143 tests passing (13 actuarial + 67 reform + 63 regression)
- Commit: 449d83e "Revert spousal benefit calculation & add Phase 5 reform infrastructure"

**Decisions made:**
- Reform objects use S3 class system for simplicity
- Three modification types: "replace", "add", "multiply"
- Phase-in uses linear interpolation between original and target values
- Reform metadata stored as attribute on modified assumptions
- Template functions provided for common reform types (NRA, benefit formula, cuts)
- Comprehensive validation in `create_reform()` for early error detection

**Phase 6: Performance Optimization** - Completed 2026-01-23
- Created `inst/benchmarks/` directory with benchmarking infrastructure:
  - `benchmark_scaling.R`: Tests scaling with 10-1000 workers
  - `pipeline_benchmark.R`: Times each step in the benefit calculation pipeline
  - Baseline results saved in `baseline_results.rds` and `pipeline_baseline.rds`
- AIME optimization: Changed from full sort to partial sort (O(n) vs O(n log n) per iteration)
- Join optimization: Created `join_all_assumptions()` helper to join all columns once
- Updated `aime()`, `pia()`, `cola()`, `worker_benefit()`, `spousal_pia()`, `spouse_benefit()`,
  and `ret()` to skip redundant joins if columns already present
- Fixed duplicate worker ID bug: Workers with identical configurations now get unique IDs
  with numeric suffix (e.g., "low-male-1966-63-1", "low-male-1966-63-2")
- All 143 tests passing
- Commit: 40bb940 "Phase 6: Performance optimization"

**Performance Results:**
- 100 workers: ~1.7 sec (0.017 sec/worker)
- Scaling exponent: 0.68 (sub-linear, improved from 0.71)
- Pipeline bottlenecks: earnings_generator (37%), ret (24%), aime (10%)

**Decisions made:**
- Used partial sorting in AIME for performance (sort(x, partial=k) is O(n) vs O(n log n))
- Join optimization done at pipeline level, not individual function level
- Duplicate IDs fixed by adding numeric suffix rather than throwing error
- Benchmark scripts kept in inst/benchmarks/ for future performance regression testing

**Survivor Benefits Integration** - Completed 2026-01-24
- Created `R/survivor.R` with widow(er) benefit functions:
  - `widow_pia()`: Calculates survivor PIA based on deceased spouse's record
  - `widow_benefit()`: Applies actuarial adjustment to survivor benefits
- Integrated survivor functions into `calculate_benefits()` pipeline:
  - Order: worker_benefit → spousal_pia → spouse_benefit → widow_pia → widow_benefit → ret → final_benefit
- Updated `final_benefit()` to handle dual entitlement per SSA Handbook Sections 733-734:
  - Worker receives: wrk_ben + max(spouse_ben, survivor_ben)
  - Cannot receive both spousal AND survivor benefits simultaneously
  - Spousal benefits stop when spouse dies; survivor benefits begin
- Updated `generate_spouse()` in spousal.R to include `s_death_age` column
- Updated `generate_single_worker()` in earnings.R to calculate `death_age` from life expectancy
- Added life expectancy columns (`le_m`, `le_f`) to assumptions dataset
- Added survivor-related global variables to ssmbar-package.R
- Regenerated all 10 regression test baselines to include survivor benefits
- All 158 tests passing
- Commit: a078607 "Integrate survivor benefits into benefit calculation pipeline"

**Key SSA Rules Implemented:**
- Survivor eligibility: Age 60 (elig_age_retired - 2) per SSA Handbook Section 401
- Survivor PIA: Per Section 202, uses 82.5% floor if spouse claimed early
- Dual entitlement: Per Sections 733-734, worker gets own benefit plus EXCESS auxiliary benefit
- Widow actuarial adjustment: w_rf = 0.285 / ((NRA - 60) * 12)

**Technical Decisions:**
- `death_age` calculated from cohort life expectancy at age 65 (rounded to integer)
- Spouse data generated once at start and passed through pipeline (performance optimization preserved)
- Column naming conflicts in widow_pia() resolved using `surv_*` prefix for temporary columns
- `spouse_ben_adj` introduced in final_benefit() to zero out spousal benefits after spouse dies

**Disability Benefits Implementation** - Completed 2026-01-25
- Added `disabled_age` parameter to `calculate_benefits()` and `earnings_generator()`
- When `disabled_age` is provided:
  - Worker's `elig_age` and `claim_age` are set to disability age
  - Disabled workers receive 100% of PIA (actuarial factor = 1.0) at all ages
  - Earnings stop at disability age
  - PIA bend points determined at year of disability (birth_yr + disabled_age)
  - COLA indexing starts from year of disability
  - At NRA, benefits seamlessly convert to retired worker benefits (no amount change)
- Fixed bugs in initial implementation from user's commit:
  - Fixed function signature mismatch (`age_elig` → `disabled_age`)
  - Fixed `if_else(is.null())` issue (use standard `if()` for scalar check)
  - Fixed scope issue with `elig_age_ret` variable lookup
- Updated `ret()` to preserve actuarial factor = 1.0 for disabled workers at NRA
- Updated `generate_spouse()` to call `join_all_assumptions()` before pipeline
- All 289 tests passing
- Commit: 58ff227 "Implement disability benefits for worker beneficiaries"

**Key SSA Rules for Disability (Disabled Worker Benefits):**
- Disabled worker receives 100% of PIA - no actuarial adjustment
- AIME computation period: From age 22 to year of disability (handled by `comp_period()`)
- PIA bend points: Determined at year worker becomes disabled (not age 62)
- Earnings: Stop at disability age (no earnings after becoming disabled)
- Conversion at NRA: Disability benefits seamlessly convert to retirement benefits (same amount)
- **Important constraint**: Worker cannot claim disability benefits at or after NRA

**Disability Rules for Auxiliary Benefits (implemented 2026-01-25):**
- **Spousal benefits on disabled worker's record**: Spouse can claim dependent benefits, but must meet standard eligibility (age 62+ AND disabled worker has already claimed)
- **Disabled worker receiving spousal benefits**: Can receive spousal benefits if eligible (age 62+ AND spouse has claimed), with actuarial adjustment on spousal portion
- **Survivor benefits**: Standard survivor rules apply (special rules deferred to later)
  - If disabled worker dies: Standard survivor benefits for surviving spouse
  - If disabled worker's spouse dies: Disabled worker can receive survivor benefits if eligible (age 60+ AND spouse died)
- **Important constraint**: Worker cannot claim disability benefits at or after NRA

**Auxiliary Benefits Fixes for Disability** - Completed 2026-01-25
- Fixed `spouse_benefit()` to use `elig_age_ret` (age 62) instead of `elig_age` for eligibility
- Fixed `yr_s_claim` calculation to use `s_birth_yr + s_claim_age` before mutate
- Survivor benefits (`widow_pia()`, `widow_benefit()`) already correctly use `elig_age_ret - 2` (age 60)
- Commit: 7a3d350 "Fix spousal benefit eligibility for disabled workers"

**Investigation Complete - Spousal Benefit Timing (2026-01-25):**

The apparent "issue" where spouse_ben = 0 at early ages despite spouse_pia > 0 is **NOT a bug**.
It is correct application of the spouse's Retirement Earnings Test (RET).

**Root Cause:** When a worker receives spousal benefits (spouse_ben) based on the spouse's record,
and the spouse continues working with earnings above the RET threshold, benefits on the spouse's
record are reduced per SSA Handbook Section 1803. This includes the worker's spouse_ben.

**Example:** High-earning spouse claims at 62 but continues working until 65:
- Ages 62-64: Spouse has ~$128k earnings, triggering s_excess_earnings of ~$97k/year
- Spouse's RET reduction (~$45k/year) is allocated 1/3 to worker's spouse_ben, 2/3 to spouse's own benefit
- Worker's spouse_ben reduction (~$15k/year) exceeds the benefit amount (~$7.6k/year), zeroing it out
- Age 65+: Spouse stops working, no RET, spouse_ben is restored to normal calculated value

**Key insight:** Scaled earnings factors (high, medium, low) assume workers continue earning until
approximately age 65, regardless of when they claim. This creates a realistic scenario where
someone claims at 62 but continues working, triggering RET.

**Verified correct behaviors:**
- Disabled worker act_factor = 1.0 at all ages (no actuarial adjustment)
- Disabled worker survivor benefits start correctly when spouse dies
- Dual entitlement: worker receives wrk_ben + max(spouse_ben_adj, survivor_ben)
- All 289 tests passing

**Benefit Class (bc) Column Implementation** - Completed 2026-01-25

Added Composite Benefit Class (`bc`) column to `final_benefit()` following the SSA BEPUF classification system.

**Supported benefit classes:**
| Code | Description |
|------|-------------|
| AR | Retired Worker (not dually entitled) |
| ARB | Retired Worker dually entitled to Spouse benefit |
| ARD | Retired Worker dually entitled to Widow(er) benefit |
| ARF | Retired Worker dually entitled to Disabled Widow(er) benefit |
| AD | Disabled Worker (not dually entitled) |
| ADB | Disabled Worker dually entitled to Spouse benefit |
| ADD | Disabled Worker dually entitled to Widow(er) benefit |
| ADF | Disabled Worker dually entitled to Disabled Widow(er) benefit |
| BR | Spouse of Retired Worker (no own worker benefit, only spousal benefit) |
| D | Widow(er) only (no own worker benefit) |
| F | Disabled Widow(er) only (no own worker benefit) |

**Implementation details:**
- `bc` column added to `final_benefit()` output
- Classification logic uses `elig_age` vs `elig_age_retired` to distinguish disabled vs retired
- Disabled workers (AD*) transition to retired workers (AR*) at NRA
- Dual entitlement determined by `spouse_ben_adj > 0` (spousal) or `survivor_ben > 0` (widow/widower)
- `is_disabled_widow` flag distinguishes disabled widow(er) from standard widow(er) benefits
- NA when not yet receiving benefits (wrk_ben <= 0 AND survivor_ben <= 0)
- Added `bc` to globalVariables in ssmbar-package.R
- Documentation updated with BEPUF reference and supported classes

**Not yet implemented:** BD (Spouse of Disabled Worker - requires disabled spouse support), E (other Survivor-only), CR, CD, CS (Child benefits)

**Commit:** 3ef82ce "Add benefit class (bc) column to final_benefit()"

**Disabled Widow(er) Benefits Implementation** - Completed 2026-01-25

Added disabled widow(er) benefits (BC codes F, ADF, ARF) following SSA rules.

**Eligibility Requirements (ALL must be met):**
1. Worker is disabled (`disabled_age` is NOT null, i.e., `elig_age < elig_age_retired`)
2. Worker is between ages 50-59 at time of first claiming (60+ qualifies for standard widow benefits)
3. Spouse has died and had a PIA
4. Disability occurred no more than 7 years after spouse's death

**Key Rules:**
- Actuarial reduction calculated as if claiming at age 60, regardless of actual claim age (50-59)
- All disabled widow(er)s receive the same reduction factor as a non-disabled widow claiming at 60
- Benefit starts at the latest of: age 50, disability onset, spouse's death
- BC code does NOT change at age 60 - once a disabled widow(er), always classified as such
- Disabled workers transition from AD* to AR* at NRA (affects BC code only, not benefit amount)

**Benefit Classes Added:**
- `F`: Disabled Widow(er) only (no own worker benefit)
- `ADF`: Disabled Worker dually entitled to Disabled Widow(er) benefit (before NRA)
- `ARF`: Retired Worker dually entitled to Disabled Widow(er) benefit (at/after NRA)

**Implementation Details:**
- Modified `widow_pia()` in survivor.R:
  - Added `is_disabled_widow` flag output (constant for all ages once qualified)
  - Lowered eligibility age to 50 for qualifying disabled workers
  - Added 7-year rule check: `(birth_yr + elig_age) <= yr_s_death + 7`
- Modified `widow_benefit()` in survivor.R:
  - Uses age 60 for actuarial factor calculation for disabled widow(er)s
  - Benefit starts at `pmax(50, elig_age, worker_age_at_spouse_death)`
  - Added `actual_widow_claim_age`, `effective_widow_claim_age`, `benefit_start_age` columns
- Modified `final_benefit()` in benefit_calculations.R:
  - Added `is_originally_disabled` and `is_currently_disabled` flags
  - Disabled workers transition to retired at NRA: `age >= nra_ind`
  - Added BC codes F, ADF, ARF for disabled widow(er) benefits
- Added new globalVariables: `is_disabled_widow`, `disabled_widow_claim_age`, `benefit_start_age`,
  `actual_widow_claim_age`, `is_originally_disabled`, `is_currently_disabled`

**Scope Limitation (documented):**
- Disabled workers are assumed to remain disabled for the remainder of their life
- Disability recovery scenario is outside the current implementation scope

**SSA References:**
- SSA Handbook Section 401.1: Disabled widow(er) eligibility
- POMS RS 00615.301: Widow(er) benefit reductions

**Tests:** All 331 tests passing (42 new disabled widow(er) tests added)

**BR (Spouse of Retired Worker) Benefit Class Fix** - Completed 2026-01-25

Fixed bug where workers with no own earnings but receiving spousal benefits were
incorrectly assigned BC = NA instead of BR.

**Issue:** The BC logic checked `wrk_ben <= 0 & survivor_ben <= 0` for NA, but didn't
check `spouse_ben_adj`. Workers with `wrk_ben = 0`, `survivor_ben = 0`, `spouse_ben_adj > 0`
were incorrectly classified as NA instead of BR.

**Fix:**
- Changed NA condition to: `wrk_ben <= 0 & spouse_ben_adj <= 0 & survivor_ben <= 0`
- Added BR for: `wrk_ben <= 0 & survivor_ben <= 0 & spouse_ben_adj > 0`

**Note:** BD (Spouse of Disabled Worker) is not yet implemented because spouses in
ssmbar are always retired workers (`disabled_age = NULL` in `generate_spouse()`).

**Tests:** All 336 tests passing (5 new BR tests added)

---

### Notes for Claude Code

**Before each phase**: Run `devtools::test()` and `devtools::check()`

**After each task**: Run tests, commit with descriptive message, check off item

**MANDATORY: Regression Testing After Major Changes**
Any major code changes (refactoring, parameterization, bug fixes) to the following files MUST be verified against baseline fixtures BEFORE finalizing:
- `R/earnings.R` (earnings_generator, generate_single_worker)
- `R/eligibility.R` (qc_comp, comp_period)
- `R/benefit_calculations.R` (aime, pia, cola, worker_benefit, rf_and_drc, final_benefit)
- `R/spousal.R` (spousal_pia, spouse_benefit, generate_spouse, calculate_spouse_dep_benefit)
- `R/survivor.R` (widow_pia, widow_benefit)
- `R/ret.R` (ret, calculate_excess_earnings, calculate_ret_reduction, allocate_ret_reduction, calculate_months_withheld, calculate_drc_payback)
- `R/reform.R` (create_reform, apply_reform, compare_benefits, reform_impact_summary)

**Verification Steps:**
1. Run `devtools::test()` to execute all regression tests
2. Verify ALL intermediate calculation columns match (not just final outputs):
   - **Earnings step**: earnings, factor, capped_earn
   - **Eligibility step**: qc_i, qc_tot, comp_period, elapsed_years, dropout_years
   - **AIME step**: aime, index_age, awi_index_age, index_factor, indexed_earn
   - **PIA step**: basic_pia, bp1_elig, bp2_elig, fact1_elig, fact2_elig, fact3_elig
   - **COLA step**: cola_basic_pia, cpi_elig, cpi_index_factor
   - **Worker benefit step**: wrk_ben, nra_ind, rf1_ind, rf2_ind, drc_ind, act_factor
   - **Spousal steps**: spouse_pia, s_pia, s_rf1_ind, s_rf2_ind, s_act_factor, spouse_ben
   - **RET step**: excess_earnings, ret_reduction, months_withheld, cum_months_withheld, ret_adj_factor
   - **Survivor steps**: survivor_pia, survivor_ben, worker_age_at_spouse_death
   - **Final step**: ben, annual_ind
3. If column names change, regenerate fixtures BUT first compare old vs new values to confirm calculations unchanged
4. Do NOT commit until all regression tests pass and ALL intermediate values verified
5. Document any intentional calculation changes explicitly in commit message

**Key rules**:
- Do NOT delete files without permission
- Preserve existing behavior unless intentionally changing
- Update roxygen docs when parameters change
- Add new variables to globalVariables
- Run `devtools::document()` after roxygen changes
- ALWAYS run regression tests after modifying benefit calculation functions
- ALWAYS document progress in this skill.md file at the end of each session (track what was completed, decisions made, and what remains)
- **MANDATORY: Preserve SSA Documentation** - Any changes to benefit calculation functions (in `R/benefit_calculations.R`, `R/spousal.R`, `R/ret.R`, `R/eligibility.R`) MUST retain existing SSA Handbook citations and references to current law Social Security rules. When adding new calculations or modifying existing ones, include appropriate SSA Handbook section references (e.g., `# https://www.ssa.gov/OP_Home/handbook/handbook.03/handbook-0320.html`). Never remove documentation links without explicit permission.

**File locations**:
- Source: `R/`
- Tests: `tests/testthat/`
- Fixtures: `tests/testthat/fixtures/`
- Raw data: `inst/extdata/`
- Data processing: `data-raw/`

---

## Future Enhancements (Lower Priority)

### Lifetime Measures
- **Real lifetime benefits**: Sum of inflation-adjusted annual benefits over lifetime
- **PV lifetime benefits**: Discounted sum using interest rate from assumptions
- **Real lifetime earnings**: Sum of inflation-adjusted earnings over working life
- **PV lifetime earnings**: Discounted sum using interest rate
- **Real lifetime SS taxes**: Sum of inflation-adjusted payroll taxes paid
- **PV lifetime taxes**: Discounted sum using interest rate

### Replacement Rate Measures
- Ratio of initial benefit to pre-retirement earnings
- Options: benefit at claim age vs. AIME, career-average earnings, final years earnings
- Consider both individual and couple replacement rates

### Annual Real Benefit Measures
- Add `real_ben` column: nominal benefit adjusted to constant dollars
- Use GDP price index or CPI-W for inflation adjustment
- Allow user to specify base year for real values

### Add Interest Rate to Assumptions Dataset
- Add `int_rate` (or `discount_rate`) column to tr2025
- Source from Trustees Report intermediate assumptions
- Used for PV calculations
