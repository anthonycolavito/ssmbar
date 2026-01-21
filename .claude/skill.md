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
Earnings → AIME → PIA → COLA → Actuarial Adjustment → RET → Final Benefit
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
| `ret()` | Retirement Earnings Test - reduces benefits if earnings > exempt amount |
| `final_benefit()` | Combines worker + spousal benefits |

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
- `legacy/worker_builder.R` is broken legacy code (moved out of R/ directory)
- Rtools not installed on work computer (CRFB)

### R CMD Check Status
- Passes with 0 errors, 0 warnings, 2 notes (expected for source directory check)

### Working Rules
- Track all changes with meaningful git commits
- Never delete files without explicit permission
- Ask permission if uncertain about any action
- CLAUDE.md contains project context (in repo root)

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

- [ ] **Create new file `R/ret.R`**

- [ ] **Create helper functions**:
  - `calculate_excess_earnings(earnings, ret_threshold, age, claim_age, nra)`
  - `calculate_ret_reduction(excess_earnings, phaseout_rate, total_annual_benefits)`
  - `allocate_ret_reduction(total_reduction, worker_ben, spouse_ben, spouse_dep_ben)`
  - `calculate_months_withheld(annual_reduction, monthly_benefit)`
  - `calculate_drc_payback(claim_age, months_withheld, nra, rf1, rf2, drc, drc_max_months)`

#### 4.2 Rewrite ret()

- [ ] **Rewrite `ret()` to use helpers**: Target under 80 lines. Accept `spouse` parameter.

- [ ] **Move `ret()` from benefit_calculations.R to ret.R**

- [ ] **Run `devtools::test()`**
- [ ] **Commit**: "Decompose RET function into focused helpers in R/ret.R"

---

### Phase 5: Create Reform Infrastructure

#### 5.1 Create R/reform.R

- [ ] **Create `create_reform()`**: S3 class constructor. Parameters: `name`, `description`, `parameters` (named list), `effective_year`, `phase_in_years`, `affected_cohorts`.

- [ ] **Create `print.Reform()`**: Print method.

- [ ] **Create `apply_reform(assumptions, reform)`**: Returns modified assumptions. Handle immediate, step change, and phase-in cases.

- [ ] **Create `compare_benefits(baseline, reformed)`**: Returns data frame with `_baseline`, `_reform`, `_diff` columns.

- [ ] **Create `reform_impact_summary(comparison)`**: Returns ReformImpact object with winners/losers/mean change stats.

- [ ] **Create `print.ReformImpact()`**: Print method.

#### 5.2 Update calculate_benefits()

- [ ] **Add `reform = NULL` parameter**
- [ ] **At start**: `if (!is.null(reform)) assumptions <- apply_reform(assumptions, reform)`

#### 5.3 Create reform tests

- [ ] **Create `tests/testthat/test-reform.R`**:
  - `create_reform()` validates parameters
  - `apply_reform()` modifies correct years
  - `apply_reform()` handles phase-in correctly
  - `compare_benefits()` calculates differences

- [ ] **Run `devtools::test()`**
- [ ] **Commit**: "Add reform infrastructure"

---

### Phase 6: Performance Optimization

#### 6.1 Benchmark

- [ ] **Create `inst/benchmarks/benchmark_scaling.R`**: Test with 10, 100, 500, 1000 workers.
- [ ] **Run and record baseline times**

#### 6.2 Optimize AIME

- [ ] **Profile `aime()`** with profvis
- [ ] **Rewrite loop** to avoid repeated sorting (O(n log n) instead of O(n²))
- [ ] **Re-benchmark**
- [ ] **Commit**: "Optimize AIME calculation"

#### 6.3 Reduce joins

- [ ] **Create `join_all_assumptions()`** helper
- [ ] **Call once in `calculate_benefits()`** before pipeline
- [ ] **Update functions** to skip joins if columns present
- [ ] **Re-benchmark**
- [ ] **Commit**: "Reduce redundant joins"

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

---

### Notes for Claude Code

**Before each phase**: Run `devtools::test()` and `devtools::check()`

**After each task**: Run tests, commit with descriptive message, check off item

**MANDATORY: Regression Testing After Major Changes**
Any major code changes (refactoring, parameterization, bug fixes) to the following files MUST be verified against baseline fixtures BEFORE finalizing:
- `R/earnings.R` (earnings_generator, generate_single_worker)
- `R/eligibility.R` (qc_comp, comp_period)
- `R/benefit_calculations.R` (aime, pia, cola, worker_benefit, spousal_pia, spouse_benefit, rf_and_drc, ret, final_benefit)

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
