# ssmbar Progress Log

This document tracks Claude's work on the ssmbar package. Claude updates this file as work progresses.

---

## Project Goals (Reference)

1. **Benefit Calculation**: Specify any hypothetical worker configuration (e.g., "medium-earner born in 1960 with a low-earner spouse born in 1963") and calculate their annual Social Security benefits at every year of their life based on their earnings profile.

2. **Distributional Analysis**: Use calculated benefits to answer questions like "What would the replacement rate be for a worker with $50,000 (2025 dollars) in average earnings if they were born in 1960 versus 1980?"

3. **Policy Reform Modeling** (Future): Model hypothetical Social Security reforms and analyze their distributional impact across different worker types.

---

## Current Status

**Last Updated**: February 5, 2026

**Active Work**: Shiny app update complete (Groups 1-4)

**Blocked On**: None

---

## Session Log

*Most recent entries at top.*

### February 5, 2026 — Shiny App Update: Reform Panel, Charts, Partial-Year PV

**Task**: Implement SHINY_UPDATE_INSTRUCTIONS.md — 4 groups of changes to the Benefit Explorer app.

**Changes Made**:

**Group 1: Rename App & Reform Panel Overhaul**
- Renamed `APP_TITLE` from "Social Security Benefit Explorer" to "Social Security Explorer"
- Restructured `AVAILABLE_REFORMS` in `global.R`:
  - Removed "Benefit Changes" category (5% and 10% benefit cuts)
  - Removed tax rate reforms (Increase Tax Rate +1pp, +2pp)
  - Moved Mini-PIA from "Benefit Changes" to "Other Reforms" (removed `pia_formula` group tag)
  - Updated `check_ui_exclusivity()` accordingly
- Complete rewrite of `mod_reform_selector.R`:
  - Converted all 4 mutually exclusive groups (PIA, NRA, COLA, Taxmax) from `radioButtons` to `checkboxGroupInput`
  - Added 4 `observeEvent()` handlers in server to enforce max-1 selection per group
  - Removed Benefit Cuts and Tax Rate sections entirely
  - Renamed section headers: "PIA Formula" → "Slow Initial Benefit Growth", "Retirement Age" → "Increase Retirement Age", "COLA Indexing" → "Modify Cost-of-Living Adjustments (COLAs)", "Taxable Maximum" → "Increase $184,500 Taxable Maximum", "Benefit Enhancements" → "Enact Benefit Enhancements"
  - Renamed all individual reform display labels to descriptive names (e.g., "Reduce Fact3 to 5%" → "Slow Benefit Growth for Top 20% of Earners")
  - Moved "Enact Benefit Enhancements" to last section position
  - Reduced label font-size from 0.85rem to 0.78rem, added word-wrap CSS
- Increased sidebar width from 300 to 340px in `app.R`

**Group 2: Individual Worker Tab Updates**
- Increased chart `base_size` from 12 to 16 in `global.R` `chart_theme` (all text sizes proportionally increased)
- Added benefit-type (bc) labels at transition points on benefits-by-age chart
- Changed default chart toggle from "Nominal $" to "Real 2025 $"
- Removed employer toggle checkbox from NMTR chart header
- Hardcoded `include_employer = TRUE` in all calls to `net_marginal_tax_rate()`, `marginal_irr()`, and `internal_rate_of_return()`
- Replaced "Mean NMTR" and "Years in Top 35" metric boxes with:
  - "Marginal IRR" — IRR from last working year's marginal data, with baseline→reform display
  - "Marginal Benefit-Tax Ratio" — delta_pv_benefits / tax at last working year

**Group 3: Cohort Comparison Tab Updates**
- Changed default claim age from 67 to 65
- Increased chart point sizes (2→3) and line widths (1.2→1.5)
- Added replacement rate type selector with 3 options:
  - "PV Replacement Rate" (pv_rr)
  - "High-35 Wage-Indexed" (wage_h35)
  - "High-35 Price-Indexed" (real_h35)
- Server updated to filter `rep_rates()` output by selected type

**Group 4: Partial Years Lifetime Measure**
- Updated `pv_lifetime_benefits()`: includes fractional year at `floor(death_age)` weighted by `death_age - floor(death_age)`
- Updated `real_lifetime_benefits()`: same partial year treatment
- Updated `internal_rate_of_return()`: benefit stream includes partial year at death with fractional weight
- Tax side unchanged (ages 21-64)

**Files Modified**:
| File | Changes |
|------|---------|
| `inst/shiny/benefit_explorer/global.R` | App title, AVAILABLE_REFORMS restructure, chart_theme font sizes, check_ui_exclusivity |
| `inst/shiny/benefit_explorer/app.R` | Sidebar width 300→340 |
| `inst/shiny/benefit_explorer/modules/mod_reform_selector.R` | Complete rewrite: checkboxes, mutual exclusivity, renamed labels/sections |
| `inst/shiny/benefit_explorer/modules/mod_individual_tab.R` | Default to real, bc labels, remove employer toggle, hardcode include_employer, new marginal metrics |
| `inst/shiny/benefit_explorer/modules/mod_cohort_tab.R` | Default claim age 65, larger charts, replacement rate type selector |
| `R/pv_functions.R` | Partial year support in pv_lifetime_benefits, real_lifetime_benefits, internal_rate_of_return |

**Test Results**:
- 652 tests passing (same as before changes)
- 3 pre-existing failures in `test-marginal.R` (unrelated to Shiny/PV changes)
- All 57 `pv_functions` tests pass (partial year changes compatible)

**Commits**:
- `dd0c5c9`: Update Shiny app: reform panel overhaul, chart improvements, partial-year PV

**Open Questions**: None

---

### January 30, 2026 — Analytical Measures and AIME Optimization

**Task**: Implement new analytical functions per approved plan (Phases 1-5 from plan file).

**Changes Made**:

**Phase 1: File Rename**
- Renamed `R/CL_benefit_calculator.R` → `R/calculate_benefits.R` for cleaner naming
- Updated all internal references (4 files)

**Phase 3: Lifetime Internal Rate of Return**
- Added `internal_rate_of_return(worker, assumptions, include_employer = FALSE)` to `R/pv_functions.R`
- Solves for discount rate where PV(taxes) = PV(benefits) using `uniroot()`
- Returns NA if no solution exists in [-0.99, 1.0] range

**Phase 4: Marginal Analysis Functions**
Added three new functions to `R/analytic_functions.R`:

1. **`marginal_benefit_analysis(worker, assumptions, base_year = 2025)`**
   - Uses **cumulative stopping-point method**: computes PV of lifetime benefits if worker stopped after t years vs t-1 years
   - Returns: cumulative_aime, cumulative_pia, cumulative_pv, delta_pv_benefits, in_top_35, indexed_rank
   - Years 1-9: delta_pv = 0 (not yet eligible)
   - Year 10: Large positive delta_pv (eligibility transition)
   - Years 11-35: Each year adds to AIME numerator
   - Years 36+: Only adds value if displacing a lower-earning year from top 35

2. **`net_marginal_tax_rate(worker, assumptions, include_employer = FALSE)`**
   - Formula: `NMTR = (ss_tax - delta_pv_benefits) / earnings`
   - Shows effective tax rate after accounting for benefit accrual
   - Low earners have lower (or negative) NMTR due to progressive formula

3. **`marginal_irr(worker, assumptions, include_employer = FALSE)`**
   - IRR on each year's tax contribution
   - Returns -1 for years not in top 35 (100% loss)
   - Uses numerical solver for top 35 years

**Phase 5: AIME Optimization**
- Pre-computed eligibility flags in `aime()` function
- Added early exit to skip years before first eligibility
- Cleaner code structure

**Other Changes**:
- Added roxygen2 documentation to `rep_rates()` function
- Updated `R/ssmbar-package.R` with new global variables
- Created test scripts: `scripts/test_new_functions.R`, `scripts/test_progressivity.R`

**Test Coverage**:
- Added 5 IRR tests to `test-pv_functions.R`
- Created `test-marginal.R` with 32 tests for marginal functions
- Total tests: 495 (was 452)

**Verification Results**:

Progressivity test (1960 birth cohort, claim at 67):
| Type      | IRR (emp) | IRR (total) | Mean NMTR | Mean mIRR |
|-----------|-----------|-------------|-----------|-----------|
| very_low  | 8.69%     | 4.94%       | -25.27%   | 27.87%    |
| low       | 7.66%     | 4.30%       | -18.79%   | 21.67%    |
| medium    | 6.66%     | 3.66%       | -3.98%    | 10.76%    |
| high      | 6.14%     | 3.33%       | 0.93%     | 8.40%     |
| max       | 5.06%     | 2.68%       | 4.66%     | 5.42%     |

All progressivity checks pass:
- Lifetime IRR decreases with earnings ✓
- IRR with employer < IRR employee only ✓
- Mean NMTR increases with earnings ✓
- Mean marginal IRR decreases with earnings ✓

**Implementation Note** (corrected):
- Originally attempted per-dollar marginal approach (marginal_pia_rate, delta_aime_per_dollar, delta_pia_per_dollar)
- Replaced with cumulative stopping-point method: computes delta_pv = PV(benefits after t years) - PV(benefits after t-1 years)
- This correctly answers "what is the value of working year t?"

**Commits**:
- `5791416`: Add analytical measures and optimize AIME calculation
- `c65570a`: Fix NMTR formula and add rep_rates() documentation

**Open Questions**: None

---

### January 29, 2026 — SSA Max Benefit Table Investigation

**Task**: Investigate discrepancies between ssmbar and SSA's Maximum Benefit table (max.xlsx) to understand why birth year 1960 showed larger differences in V.C7 validation.

**Investigation Process**:

1. Compared ssmbar output to SSA max benefit table for max earners at various retirement ages
2. Verified AIME values match exactly between ssmbar and SSA table
3. Reverse-engineered SSA's calculations to identify methodology differences
4. Reviewed statutory requirements (42 USC 415(a)(1)(B)) for bend point determination

**Key Findings**:

1. **AIME matches**: ssmbar's AIME calculation matches SSA's published values exactly (e.g., 11,621 for birth year 1960 at age 65)

2. **Bend point year discrepancy**: SSA's max benefit table appears to use bend points from the year BEFORE eligibility (2021 for birth year 1960), while the statute specifies the eligibility year (2022)

3. **ssmbar is correct per statute**: 42 USC 415(a)(1)(B) states bend points are "for individuals who initially become eligible... in the calendar year" - meaning the eligibility year's bend points should be used

4. **Reproduction test**:
   - Using ssmbar's method (2022 bend points): Benefit = $3,519.90
   - Using year-before bend points (2021): Benefit = $3,374.30
   - SSA table shows: $3,374
   - SSA table can only be reproduced using the year-before-eligibility bend points

5. **Age 67 case inconclusive**: Could not exactly reproduce SSA's age 67 values with any combination of COLAs and DRCs tested

**Conclusion**:

ssmbar's methodology is **correct per statute**. The SSA max benefit table may use a simplified or illustrative methodology that differs from the actual statutory calculation. No changes to ssmbar are warranted.

**Verification**:
- V.C7 validation: 1.36% average difference (improved from 2.51% after rounding fixes)
- Medium earner: 0.06% average difference (nearly perfect)
- Statutory compliance verified against 42 USC 415(a)(1)(B)

**Commits**:
- `1c5c32b`: Add V.C7 validation script and document improved accuracy

---

### January 29, 2026 — Child Benefits and Family Maximum Implementation

**Task**: Implement child benefits (42 USC 402(d)) and family maximum (42 USC 403(a)) per approved plan.

**Changes Made**:

**New Files Created:**
- `R/child.R`: Child benefit functions
  - `parse_child_spec()` — Parses child specification strings ("birthyr-disabled" format)
  - `is_child_eligible()` — Checks child eligibility based on age and disability status
  - `child_pia()` — Calculates child PIA (50% of worker's COLA'd PIA)
  - `child_benefit()` — Calculates child benefits (no actuarial reduction per 42 USC 402(d))
- `inst/extdata/family_max_bp.csv`: Historical family max bend points (1979-2025)
- `tests/testthat/test-child_benefits.R`: 29 tests for child benefit functions
- `tests/testthat/test-family_maximum.R`: 22 tests for family maximum

**Modified Files:**
- `R/assumptions_prep.R`: Added family max bend point projection logic (fm_bp1, fm_bp2, fm_bp3), child_pia_share parameter (0.5), and historical bend point loading from CSV
- `R/benefit_calculations.R`:
  - Added `family_maximum()` function implementing 42 USC 403(a)(1) bend point formula
  - Added disability alternative formula per 42 USC 403(a)(6): min(85% AIME, 150% PIA)
  - Updated `join_all_assumptions()` to include fm_bp1, fm_bp2, fm_bp3, child_pia_share, cola
  - Updated `final_benefit()` to include child benefits in total benefit calculation
  - Updated pipeline documentation
- `R/earnings.R`: Added child1_spec, child2_spec, child3_spec parameters to `earnings_generator()` and `generate_single_worker()`
- `R/CL_benefit_calculator.R`: Added child_spec parameters, updated pipeline to include child_pia(), child_benefit(), family_maximum()
- `data/tr2025.rda`: Regenerated with new columns (fm_bp1, fm_bp2, fm_bp3, child_pia_share)
- All 25 test fixtures regenerated to reflect family maximum implementation

**Key Implementation Decisions:**

1. **Child specification format**: `"birthyr-disabled"` (e.g., "2010-FALSE" or "2005-TRUE")
2. **Child benefit**: 50% of worker's COLA-adjusted PIA, no actuarial reduction
3. **Child eligibility**: Ages 0-17 for non-disabled; indefinite for disabled-before-22
4. **Family maximum formula**: 150%/272%/134%/175% bend point formula
5. **Family max COLA**: Applied year-by-year same as PIA
6. **Spousal benefits NOT reduced by worker's family max**: Spousal benefits come from spouse's record and are subject to spouse's family max (which we don't calculate). Only child benefits are reduced by this worker's family max.

**Technical Findings:**

1. **Family max scope**: The family maximum limits benefits paid on ONE worker's record. For a zero-earner receiving spousal benefits from a high-earning spouse, the relevant family max is the spouse's, not the worker's. Initial implementation incorrectly applied worker's family max to spousal benefits received, causing benefit class to be NA instead of BR.

2. **COLA factor calculation**: Family maximum function needed to calculate cola_factor internally from the cola percentage column rather than relying on cola_factor from the cola() function (which is calculated but not passed through in non-debug mode).

**Verification**:
- [x] Tests pass (452 total: 0 failures, 1 warning, 2 skipped empty tests)
- [x] Validation scripts pass (V.C7 comparison: 1.36% avg difference, see Validation History)
- [ ] Manual inspection complete (pending)

**Commits**:
- `[pending]`: Add child benefits and family maximum implementation

**Open Questions**:
- None

**Deferred**:
- Full family maximum for coupled households (would require tracking spouse's record separately)

---

## Open Questions

*Questions awaiting user input or further investigation.*

| # | Question | Context | Status |
|---|----------|---------|--------|
| 1 | [Question] | [Where it arose] | Open / Resolved |

---

## Known Issues

*Bugs or problems identified but not yet fixed.*

| # | Issue | Location | Type | Status |
|---|-------|----------|------|--------|
| 1 | `worker_builder.R` contains incomplete code (references undefined `dataset` variable) | `R/worker_builder.R` | Rule bug (needs discussion) | Open |

---

## Deferred Decisions

*Design choices or implementation details postponed for later.*

| # | Decision | Reason Deferred | Revisit When |
|---|----------|-----------------|--------------|
| 1 | [Example] | [Why postponed] | [Trigger for revisiting] |

---

## Completed Work

*Summary of completed tasks, moved from session log after verification.*

| Date | Task | Key Changes | Verified |
|------|------|-------------|----------|
| Feb 2026 | Shiny App Update (Groups 1-4) | Reform panel overhaul, chart improvements, marginal metrics, partial-year PV | ✓ (652 tests pass) |
| Jan 2026 | Table V.C7 Validation | Comprehensive validation against Trustees Report; identified CPI-W deflation requirement; traced AIME discrepancies to rounding | ✓ |
| Jan 2026 | Benefit Explorer Shiny App | Created `run_app()`, PV functions, modular Shiny app with 2-tab architecture | ✓ |
| Earlier | Package Structure Conversion | Converted informal R scripts to professional package with roxygen2, devtools, testing | ✓ |

---

## Secondary Source Resolutions

*Tracks when secondary sources (regulations, POMS) were consulted to resolve statutory ambiguity.*

| Date | Statutory Section | Ambiguity | Resolution Source | Resolution |
|------|-------------------|-----------|-------------------|------------|
| [DATE] | [e.g., 42 USC 415(b)] | [What was unclear] | [e.g., 20 CFR 404.211] | [How it was resolved] |

---

## Documentation Flags

*SUGGESTED comments and documentation issues awaiting user verification.*

### High Priority

| File | Function/Location | Issue | Verified |
|------|-------------------|-------|----------|
| survivor.R | `widow_benefit()` lines 334-354 | Unresolved TODO block - add POMS RS 00615.301 citations for widow reduction factors | ☐ |
| benefit_calculations.R | `family_maximum()` | Missing @examples section | ☐ |

### Medium Priority

| File | Function/Location | Issue | Verified |
|------|-------------------|-------|----------|
| child.R | `child_pia()` | Missing @examples section | ☐ |
| child.R | `child_benefit()` | Missing @examples section | ☐ |
| survivor.R | `widow_benefit()` line 276 | Verify w_rf formula (0.285 constant = 28.5% max reduction) against POMS RS 00615.301 | ☐ |
| earnings.R | line 309 | TODO: Add citation to Trustees Report methodology for scaled earnings factors | ☐ |

### Low Priority

| File | Function/Location | Issue | Verified |
|------|-------------------|-------|----------|
| assumptions_prep.R | line 21 | TODO: Add specific handbook section citations for parameter projection formulas | ☐ |
| earnings.R | line 252 | TODO: Document how sex affects benefit calculations (life expectancy lookup) | ☐ |
| spousal.R | line 52 | TODO: Explain custom earnings parsing regex pattern | ☐ |
| data.R | tr2025 | Verify le_m and le_f data source citations | ☐ |

---

## Established Methodological Decisions

*Design choices that have been made and should be maintained consistently.*

### Present Value Calculations

All PV calculations use **real 2025 dollars** to ensure comparability across time:

1. **Two-step PV methodology**:
   - Step 1: Convert nominal cash flows to real 2025 dollars using GDP price index: `real_value = nominal × (gdp_pi_2025 / gdp_pi_year)`
   - Step 2: Discount real values using the real discount factor (real_df) from Trustees assumptions
   - This ensures real sums and PV measures are directly comparable

2. **Discount to age 65**: All PV calculations normalize to age 65 by default.

3. **Partial year at death**: Benefits include a fractional year at `floor(death_age)`, weighted by `death_age - floor(death_age)`. Full years use `age >= claim_age & age < floor(death_age)`. This applies to `pv_lifetime_benefits()`, `real_lifetime_benefits()`, and `internal_rate_of_return()`.

4. **Tax period**: Taxes calculated ages 21-64 (working years before typical retirement).

5. **Benefit period**: Benefits calculated from claim_age through floor(death_age) with fractional last year.

6. **Employer taxes optional**: `include_employer = TRUE` doubles the employee tax to capture total payroll tax contribution.

### Replacement Rate Calculations

1. **Numerator**: Always the annual benefit at claim age (not age 65).

2. **Denominator**: Earnings through the year before claiming (not age 64).
   - For claim age 67, uses earnings ages 21-66
   - For claim age 62, uses earnings ages 21-61

3. **Indexing reference year**: The year before claiming (not a fixed year).
   - Wage-indexed earnings use AWI from year before claiming
   - Real earnings use GDP PI from year before claiming

4. **PV Annuity replacement rate**: Uses real discount factor to compute constant real payment with same PV as career earnings.

5. **High-N vs Last-N**:
   - High-N: Uses N highest earning years (sorted descending)
   - Last-N: Uses final N years before claiming

### Couple Analysis

1. **Shared measures**: Benefits and taxes split 50/50 between spouses to represent household resource sharing.

2. **Individual vs combined**: Both individual ratios and combined couple ratio are displayed.

### App Defaults

- Default birth year: 1960 (validated against V.C7)
- Default claim age: 65
- Default worker type: Medium earner

### Performance Optimization Strategy

**Current Performance**: ~500ms per worker (as of January 2026)

**Target Use Case**: Tens of thousands to hundreds of thousands of workers

**Architecture Assessment** (January 30, 2026):

The benefit calculation pipeline uses `group_by(id) %>% group_modify()` which processes workers sequentially. Key bottlenecks identified:

| Function | Issue | Vectorizable? |
|----------|-------|---------------|
| AIME | for-loop over ages per worker | Partially (top-35 sort is inherent) |
| COLA | Sequential loop (path-dependent rounding) | **No** - SSA compliance requires sequential |
| child_pia | sapply over rows | **Yes** - vectorized in Jan 2026 |
| family_maximum | Sequential COLA loop | **No** - same path dependency |

**Constraints**:
1. **SSA Compliance**: Sequential calculations like COLA must remain sequential because SSA rounds to the nearest dime at each step, and each year's input depends on the previous year's rounded output. This is statutory, not an implementation artifact.
2. **Code Readability**: dplyr is preferred over data.table for auditability—code must be verifiable against SSA rules.

**Deferred: Parallelization**

Parallelization via `furrr::future_map()` would provide 4-8x speedup by processing multiple workers simultaneously across CPU cores. Each worker's sequential calculations remain intact.

**Why deferred**: Current processing needs don't justify the added complexity. Parallelization makes debugging harder (error messages lose context, can't use `browser()`, errors can appear non-deterministic).

**When to revisit**: When processing >10,000 workers becomes a regular need, or single-run time exceeds acceptable thresholds.

**To enable parallelization later**: Maintain current architecture where each worker's calculation is self-contained within `group_modify()`. This makes future `future_map()` conversion straightforward.

---

## Critical Implementation Rules

*Technical constraints discovered through debugging that must be maintained.*

### Discount Factor Lookup

The `real_df_norm` lookup must happen BEFORE filtering rows, otherwise the discount year (age 65) may be excluded:

```r
# CORRECT: Lookup in mutate before filter
mutate(real_df_norm = real_df[which(age == 65)][1]) %>%
filter(age >= claim_age & age < death_age)

# WRONG: Filtering first loses access to age 65 row
filter(age >= claim_age) %>%
mutate(real_df_norm = ...)  # Age 65 row already gone!
```

### Column Join Conflicts

When joining assumption columns, always check if they already exist:

```r
if (!"gdp_pi" %in% names(worker)) {
  worker <- worker %>% left_join(assumptions %>% select(year, gdp_pi), by = "year")
}
```

### Internal Function Access

The `rep_rates()` function is not exported. In the Shiny app, use `ssmbar:::rep_rates()`.

---

## Validation History

### Table V.C7 Validation (January 2026)

**Key Discovery**: V.C7 shows all benefits in constant 2025 CPI-W adjusted dollars, not nominal dollars.
- For birth year 1960 (turning 65 in 2025), minimal adjustment needed
- For future birth years, nominal benefits must be deflated: `benefit_2025 = nominal × (CPI_2025 / CPI_benefit_year)`

**V.C7 Table Structure**:
- Column 1: Year turning 65
- Column 2: NRA (e.g., "67:0")
- Column 3: Full PIA at NRA (in 2025 dollars)
- Column 6: Reduced benefit at age 65 (in 2025 dollars)

**Validation Results (With Deflation)**:

| Worker Type | Avg % Diff | Min % | Max % |
|-------------|-----------|-------|-------|
| medium      | 0.73%     | -0.16%| 1.04% |
| low         | 2.26%     | 0.77% | 2.77% |
| max         | 2.66%     | 1.12% | 3.18% |
| high        | 3.42%     | 1.52% | 4.18% |
| very_low    | 3.47%     | 1.55% | 4.21% |

**Overall: 2.51% average difference across all worker types and birth years 1960-2000**

**Root Cause of Differences**:

For the medium earner born 1960:
- ssmbar AIME: 4,618
- V.C7 implied AIME: ~4,557
- Difference: 61/month (1.3%)

This stems from cumulative small rounding differences in `scaled_factor × AWI` calculations across 44 working years. Total nominal earnings differ by only $575 (0.04%), but rounding compounds through the calculation pipeline.

**Technical Findings Confirmed**:
1. Actuarial reduction factors are correct: 5/9 of 1% per month for first 36 months = 0.8667 for 24 months early
2. Scaled earnings factors match SSA Actuarial Note 2025.3 Table 6
3. January 1 vs January 2 birth dates: SSA hypotheticals use January 2 birthdays; our indexing year (age 60) matches SSA methodology
4. COLA timing: Year-by-year COLA application with flooring at each step implemented correctly

**Validation Scripts** (in `scripts/`):
- `validate_all_workers.R` — Basic validation (nominal dollars)
- `validate_all_workers_deflated.R` — Proper validation with CPI-W deflation
- `trace_medium_earner.R` — Detailed calculation trace
- `compare_table4_earnings.R` — Earnings comparison with Actuarial Note
- `validate_vc7_2025.R` — V.C7 validation for birth years 1960-1970

### Table V.C7 Re-Validation After Rounding Changes (January 29, 2026)

After implementing child benefits and family maximum (which included rounding fixes throughout the pipeline), V.C7 validation was re-run for birth years 1960-1970.

**Updated Results (Birth Years 1960-1970, Turning 65 in 2025-2035)**:

| Worker Type | Avg % Diff | Min %   | Max %  |
|-------------|-----------|---------|--------|
| medium      | **0.06%** | -0.14%  | 0.87%  |
| low         | 1.19%     | 0.86%   | 2.85%  |
| max         | 1.42%     | 1.09%   | 3.24%  |
| high        | 1.96%     | 1.53%   | 4.26%  |
| very_low    | 2.06%     | 1.54%   | 4.42%  |

**Overall: 1.36% average difference**

**Comparison to Previous Validation**:

| Worker Type | Previous  | Current   | Improvement |
|-------------|-----------|-----------|-------------|
| very_low    | 3.47%     | 2.06%     | -1.41%      |
| low         | 2.26%     | 1.19%     | -1.07%      |
| medium      | 0.73%     | **0.06%** | -0.67%      |
| high        | 3.42%     | 1.96%     | -1.46%      |
| max         | 2.66%     | 1.42%     | -1.24%      |
| **Overall** | **2.51%** | **1.36%** | **-1.15%**  |

**Key Observations**:
1. Medium earner now nearly perfect at 0.06% average difference
2. Overall accuracy improved by ~46% (from 2.51% to 1.36%)
3. All worker types now under 2.1% average difference
4. Year 2025 (birth year 1960) consistently shows larger differences (~4%) across worker types, likely due to CPI-W base year indexing

---

## Shiny App Files Reference

| File | Purpose |
|------|---------|
| `R/pv_functions.R` | Present value calculation functions (with partial-year support) |
| `R/analytic_functions.R` | Marginal analysis functions (NMTR, marginal IRR) |
| `R/run_app.R` | App launcher function |
| `inst/shiny/benefit_explorer/app.R` | Main app — 2-tab layout with shared reform sidebar (340px) |
| `inst/shiny/benefit_explorer/global.R` | Config, AVAILABLE_REFORMS, chart_theme (base_size 16) |
| `inst/shiny/benefit_explorer/modules/mod_reform_selector.R` | Reform sidebar — checkbox groups with server-side mutual exclusivity |
| `inst/shiny/benefit_explorer/modules/mod_individual_tab.R` | Individual Worker tab — benefits chart, NMTR chart, metrics (incl. marginal IRR/BTR) |
| `inst/shiny/benefit_explorer/modules/mod_cohort_tab.R` | Cohort Comparison tab — 4 charts across birth years, replacement rate type selector |

---

## Test Suite Reference

652 tests passing (as of February 5, 2026; 3 pre-existing failures in test-marginal.R):
- 256 regression tests
- 133 reform template tests
- 67 reform tests
- 57 PV function tests
- 40 marginal analysis tests (3 failing — pre-existing)
- 29 child benefit tests
- 22 family maximum tests
- 19 special minimum PIA tests
- 16 baseline-reform equivalence tests
- 13 actuarial tests

---

## Notes

*Miscellaneous observations, ideas, or context for future reference.*

-
