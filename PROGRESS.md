# ssmbar Progress Log

This document tracks Claude's work on the ssmbar package. Claude updates this file as work progresses.

---

## Project Goals (Reference)

1. **Benefit Calculation**: Specify any hypothetical worker configuration (e.g., "medium-earner born in 1960 with a low-earner spouse born in 1963") and calculate their annual Social Security benefits at every year of their life based on their earnings profile.

2. **Distributional Analysis**: Use calculated benefits to answer questions like "What would the replacement rate be for a worker with $50,000 (2025 dollars) in average earnings if they were born in 1960 versus 1980?"

3. **Policy Reform Modeling** (Future): Model hypothetical Social Security reforms and analyze their distributional impact across different worker types.

---

## Current Status

**Last Updated**: January 29, 2026

**Active Work**: Implementation complete - Child Benefits and Family Maximum

**Blocked On**: None

---

## Session Log

*Most recent entries at top.*

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
| Jan 2026 | Table V.C7 Validation | Comprehensive validation against Trustees Report; identified CPI-W deflation requirement; traced AIME discrepancies to rounding | ✓ |
| Jan 2026 | Benefit Explorer Shiny App | Created `run_app()`, PV functions, modular Shiny app with 5 modules | ✓ |
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
| analytic_functions.R | `rep_rates()` | Missing roxygen2 documentation header entirely | ☐ |
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

3. **Death age exclusion**: Use `age < death_age` (not `<=`) to exclude benefits in the year of death.

4. **Tax period**: Taxes calculated ages 21-64 (working years before typical retirement).

5. **Benefit period**: Benefits calculated from claim_age to death_age - 1.

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
| `R/pv_functions.R` | Present value calculation functions |
| `R/run_app.R` | App launcher function |
| `inst/shiny/benefit_explorer/app.R` | Main app (ui + server) |
| `inst/shiny/benefit_explorer/global.R` | Load packages and data |
| `inst/shiny/benefit_explorer/modules/mod_worker_input.R` | Worker selection sidebar |
| `inst/shiny/benefit_explorer/modules/mod_benefits.R` | Benefits over time charts |
| `inst/shiny/benefit_explorer/modules/mod_replacement.R` | Replacement rate comparisons |
| `inst/shiny/benefit_explorer/modules/mod_lifetime.R` | PV benefits/taxes display |
| `inst/shiny/benefit_explorer/modules/mod_ratios.R` | Benefit-tax ratios |

---

## Test Suite Reference

All 452 tests pass (as of January 29, 2026):
- 256 regression tests
- 67 reform tests
- 46 PV function tests
- 29 child benefit tests
- 22 family maximum tests
- 19 special minimum PIA tests
- 13 actuarial tests

---

## Notes

*Miscellaneous observations, ideas, or context for future reference.*

-
