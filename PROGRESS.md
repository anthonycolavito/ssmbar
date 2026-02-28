# ssmbar Progress Log

This document tracks Claude's work on the ssmbar package. Claude updates this file as work progresses.

---

## Project Goals (Reference)

1. **Benefit Calculation**: Specify any hypothetical worker configuration (e.g., "medium-earner born in 1960 with a low-earner spouse born in 1963") and calculate their annual Social Security benefits at every year of their life based on their earnings profile.

2. **Distributional Analysis**: Use calculated benefits to answer questions like "What would the replacement rate be for a worker with $50,000 (2025 dollars) in average earnings if they were born in 1960 versus 1980?"

3. **Policy Reform Modeling** (Future): Model hypothetical Social Security reforms and analyze their distributional impact across different worker types.

---

## Current Status

**Last Updated**: February 28, 2026

**Active Work**: Static GitHub Pages Benefit Explorer

**Blocked On**: Data generation (requires running R pre-computation script)

---

## Session Log

*Most recent entries at top.*

### February 28, 2026 (Session 16) — Static GitHub Pages Benefit Explorer

**Task**: Build a static site that replicates the Shiny Benefit Explorer using pre-computed JSON data, deployable to GitHub Pages with no server required.

**Files Created**:

1. **`scripts/precompute_static_data.R`** (~650 lines): R script that runs ssmbar across all input combinations (11 worker types x 2 sexes x 9 claim ages = 198 configs, 22 reform scenarios each) and exports column-oriented JSON. Three computation phases: cohort data, individual metrics+benefits, marginal analysis. Supports `--cores N` parallelization, `--type` filtering, incremental runs (skips existing files).

2. **`docs/index.html`** (533 lines): Single-page app with Bootstrap 5 dark theme, CRFB branding. Two tabs (Individual Worker, Cohort Comparison), sidebar reform selector with 22 radio options across 7 categories, Chart.js charts, metric cards, collapsible data tables with CSV export.

3. **`docs/css/style.css`** (572 lines): CRFB dark theme (navy/light-blue/orange palette), responsive layout with mobile sidebar offcanvas, custom form controls, metric cards, chart containers, data tables.

4. **`docs/js/formatters.js`** (64 lines): Currency, percent, number formatting utilities.

5. **`docs/js/data-loader.js`** (212 lines): Fetch JSON with LRU cache (50 entries), request deduplication, data extraction helpers for cohort/individual/marginal data.

6. **`docs/js/chart-manager.js`** (341 lines): Chart.js v4 chart builders for 6 chart types: benefits-by-age (line), NMTR (bar), and 4 cohort charts (replacement rate, PV benefits, ratio, IRR).

7. **`docs/js/table-manager.js`** (286 lines): HTML table rendering with pagination, CSV export, table builders for benefits, marginal, and cohort data.

8. **`docs/js/ui-controls.js`** (216 lines): Dropdown population, sidebar toggle, reform selector, tab switching, range slider, benefit view toggle (nominal/real).

9. **`docs/js/app.js`** (338 lines): Main controller wiring inputs to data loader, chart manager, and table manager. Handles lazy loading of marginal data, metric card updates with baseline→reform comparison.

10. **`docs/data/manifest.json`** (104 lines): Metadata file indexing all dimensions, reform scenarios, and file path patterns.

**Architecture**: Pre-compute everything in R → export as JSON → vanilla JS reads JSON and renders charts. Changing birth year or reform within same worker config is instant (data already cached). Only changing worker type, sex, or claim age triggers a new file fetch.

**Next Steps**: Run pre-computation script to generate data files, then enable GitHub Pages.

### February 20, 2026 (User commits) — Documentation Cleanup

**Author**: Anthony (manual commits, not Claude session)

**Changes**:

1. **`R/earnings.R` documentation cleanup** (commit `5b6d9e3`):
   - Removed stale TODO comments (`Document - explain how sex affects benefit calculations`, `Document - add citation to Trustees Report methodology for scaled earnings factors`)
   - Removed redundant comment about fractional death age weighting (PV functions)
   - Added descriptive comment for max-earner earnings logic ("Workers who had earnings at the taxable maximum at every year of their careers")
   - Cleaned up inline comment on max-earner left join ("Left joins tax max by year")

2. **`R/benefit_calculations.R` documentation edits** (commit `375221e`):
   - Removed `DRC months = (max_drc_age - nra) * 12. Per 42 USC 402(w).` line from `rf_and_drc()` roxygen `@param max_drc_age`
   - Fixed comment: "January 1 claims" → "January 2 claims" in AIME computation notes
   - Minor wording: "onwards" → "onward"
   - Added clarifying comment on AIME earnings window: explains that current-year earnings are excluded because only earnings through the prior year are available to SSA at time of claim

### February 10, 2026 (Session 15) — Statutory Audit Fixes

**Task**: Fix discrepancies found in the second-pass statutory audit (Title II comparison). Six fixes implemented per user approval.

**Fixes Implemented**:

1. **drc_max_months → max_drc_age** (Finding #5): Replaced hardcoded `drc_max_months = 36` with `max_drc_age = 70` assumptions variable. `rf_and_drc()` now computes `(max_drc_age - nra) * 12` internally, producing correct DRC months for all NRA values (48 for NRA=66, 60 for NRA=65, etc.). Previously, pre-1960 birth cohorts claiming past NRA+3 years were undercomputed.
   - Files: `assumptions_prep.R`, `benefit_calculations.R` (rf_and_drc, join_all_assumptions), `ret.R` (calculate_drc_payback, calculate_spouse_ret_effect, ret), `reform_functions.R` (ret_reform), `data.R`, `ssmbar-package.R`

2. **Disability family max formula** (Finding #6): Changed `pmin(0.85 * aime, 1.50 * pia)` to `pmin(pmax(0.85 * aime, pia), 1.50 * pia)` per 42 USC 403(a)(6)(A). The inner `max()` ensures the disability FM is never less than the PIA.

3. **floor_dime() citation** (Finding #9): Corrected erroneous "42 USC 415(a)(2)(C)" to "42 USC 415(a)(1)(A)" in roxygen and code comments. Also fixed same citation in `reform_functions.R`.

4. **Years of coverage cap at 30** (Finding #11): Added `pmin(cumsum(is_coverage_year), 30L)` in `years_of_coverage()` per 42 USC 415(a)(1)(C)(ii). Special minimum PIA maxes out at 30 years of coverage.

5. **RET exempt amount rounding** (Finding #12): Changed ret1 projection from `floor(... * 10) / 10` (floor-to-dime) to `round(... / 120) * 120` (nearest $120) per 42 USC 403(f)(8)(A). Changed ret2 similarly from floor-to-dime to `round(... / 480) * 480` (nearest $480).

**Not Implemented (per user decision)**:
- Finding #7 (family max includes dependent spouse): After investigation, the `spouse_ben` column in ssmbar's data model represents benefits the worker receives FROM the spouse's record (as a dependent of the spouse), not benefits paid from the worker's own record. Therefore it is correctly excluded from the worker's family max. The existing code and comment are correct.
- Finding #8 (RET two-tier ret2/$1-per-$3): User noted that since all hypothetical workers are born January 2, they attain NRA on January 1 — there are no months in the NRA year before NRA attainment, so ret2 never applies.
- Finding #10 (years_of_coverage date range comment): Left as is per user.

**STATUTORY_AUDIT.md**: Updated with all second-pass findings (Findings #5-#12), detailed analysis, and resolution status.

**Regenerated**: tr2025.rda, all 25 regression test fixtures

**Test Results**: 648 pass, 0 fail

---

### February 10, 2026 (Session 14) — Generosity Charts and tr2025 Documentation

**Task**: (1) Create generosity analysis charts for custom workers across birth cohorts. (2) Document all 59 columns of tr2025 assumptions data.

**Charts Created** (`scripts/chart_generosity_extended.R`):
- Complete rewrite producing 7 charts + 1 combined image for 4 worker types ($25K, $59K, $100K, $250K in 2025 real earnings), birth cohorts 1940-2026, all claiming at age 65
- Charts: real replacement rate (all years), wage-indexed H35 replacement rate, real initial benefits (2025$), benefit-tax ratio, PV benefits, PV taxes, and IRR
- All charts show NRA transition bands (1938-1942, 1955-1960)
- Extended tr2025 assumptions from 2100 to 2120 within the script to cover death ages of workers born through 2026 (~year 2116)
- Set `qc_required = 0` to bypass QC eligibility for custom workers whose price-indexed earnings fall below AWI-indexed QC thresholds in far-future cohorts
- Used `patchwork` package to combine all 7 charts into a 4x2 grid with shared legend
- Output: 7 individual PNGs + `generosity_combined.png`, copied to Desktop

**Documentation** (`R/data.R`, `R/assumptions_prep.R`):
- Documented all 59 columns of tr2025, organized into 4 groups:
  1. Raw Trustees Report Data (23 columns) — from `inst/extdata/2025TR_assumptions.csv`
  2. Projected Program Parameters (13 columns) — computed by `prep_assumptions()`
  3. Program Rule Constants (8 columns) — fixed current-law parameters
  4. Reform Scaffolding (16 columns) — default values for policy reform modeling
- User-verified source information for: df/real_df (derived from TR interest rates), gdp_pi (base year user-calculated to 2025), le_m/le_f (cohort life expectancy tables, by cohort turning 62), cpi_w (from TR)
- Noted `child_pia_share` needs verification
- Reform scaffolding columns have brief descriptions; detailed reform assumption documentation deferred per user request
- Updated `prep_assumptions()` docstring to describe its role (23-column input → 59-column output)
- Added Projection Formulas section documenting statutory indexing rules
- Added Hypothetical Worker Birth Date Convention section
- Regenerated man pages via `devtools::document()`

**Packages Installed**: `patchwork`, `cowplot` (for chart combining)

**Test Results**: 648 pass, 0 fail

**Deferred**:
- Reform parameter documentation: User said "We need to work through the assumptions used for each reform" — to be done in a future session
- Verify `child_pia_share` (50% of worker's PIA per 42 USC 402(d)(2))

---

### February 10, 2026 (Session 13) — Charts: Extended Projections, Analytical Investigation

**Task**: Create generosity trend charts for extended birth cohort analysis (1940-2035, later revised to 1940-2026). Investigate and fix issues with far-future cohort projections.

**Investigation**:
- Charts initially showed benefits dropping to zero for far-future cohorts
- Root cause: QC threshold grows with AWI (~3.55%) while custom workers' price-indexed earnings grow with GDP PI (~2.05%), causing QC eligibility failure
- Fix: Set `qc_required = 0` for the thought experiment
- Secondary issue: tr2025 only covers through year 2100, but workers born 2026 die at ~age 90 = year 2116, causing NA in PV/COLA/discount calculations
- Fix: Extended assumptions by 20 years (2101-2120) with steady-state growth rates from the 2025 TR

**Key Finding** (user question about $25K worker real benefit plateau):
- Once a custom worker's AIME falls entirely below BP1 (the 90% bracket), PIA = 90% × AIME
- Since AIME grows with prices and we deflate to 2025$, the real benefit becomes constant
- This occurs around the 2005 birth cohort for $25K workers

**No permanent code changes committed** — charts script is in `scripts/` (untracked).

---

### February 10, 2026 (Session 12) — Export rep_rates(), Remove Stale Duplicate File, Resolve Known Issues

**Task**: Clean up analytic functions: export `rep_rates()`, delete stale duplicate file, resolve known issues.

**Investigation Findings**:
- `R/analytic_functions_new.R` was an untracked stale earlier version of `marginal_benefit_analysis()` and `compute_pv_to_year()`, both of which also exist (in updated form) in `R/analytic_functions.R`. The `_new` version still contained the `in_top_35` and `indexed_rank` columns that were removed in Session 2, and its duplicate `@export` tags caused roxygen2 to generate duplicate entries in man pages (`marginal_benefit_analysis.Rd` listed source from both files).
- `R/worker_builder.R` (Known Issue #1) no longer exists — it was deleted in a prior session. The known issues entry was stale.
- `rep_rates()` was marked `@keywords internal` despite being used extensively in analysis scripts and the Shiny app's cohort tab. All other analytic functions (`calculate_taxes`, `marginal_benefit_analysis`, `net_marginal_tax_rate`, `marginal_irr`) are exported.

**Changes Made**:
- **`R/analytic_functions.R`**: Changed `rep_rates()` from `@keywords internal` to `@export`. Removed `@note` about using `ssmbar:::rep_rates()`. Updated example to use `rep_rates()` directly.
- **`R/analytic_functions_new.R`**: Deleted (untracked file, not staged — was never committed).
- **`NAMESPACE`**: Regenerated via `devtools::document()` — now includes `export(rep_rates)`.
- **`man/rep_rates.Rd`**: Regenerated — now references only `analytic_functions.R`.
- **`man/marginal_benefit_analysis.Rd`**: Regenerated — duplicate source reference to `analytic_functions_new.R` removed.
- **`man/compute_pv_to_year.Rd`**: Regenerated — duplicate source reference removed.
- **`PROGRESS.md`**: Marked Known Issue #1 (`worker_builder.R`) as resolved.

**Test Results**: 648 pass, 0 fail

---

### February 10, 2026 (Session 11) — Fix Custom Earnings Date-Sensitive Bug

**Task**: Fix bug in `generate_single_worker()` where custom worker type failed for birth years 2006-2010 due to a date-sensitive GDP price index lookup.

**Root Cause**: In `R/earnings.R` line 321, the custom earnings code path looked up the current year's GDP price index (`pi_curr`) inside a `mutate()` on the worker dataframe:
```r
pi_curr = gdp_pi[which(year == as.numeric(format(Sys.Date(), "%Y")))]
```
This searched for `year == 2026` in the worker's own rows (ages 21-119). For workers born 2006+, the first row is year 2027, so 2026 doesn't exist in the data — the lookup returns an empty vector, causing dplyr to error with "pi_curr must be size 99 or 1, not 0."

Workers born 1939-2005 were unaffected because their lifetime spans include 2026. Medium/scaled workers were unaffected because they use pre-computed `awi * factor` and never reference the current year's price index.

**Changes Made**:
- **`R/earnings.R` (line 321)**: Moved `pi_curr` lookup outside the `mutate()`, pulling directly from the `assumptions` dataframe instead of the worker's row data:
  ```r
  pi_curr <- assumptions$gdp_pi[assumptions$year == as.numeric(format(Sys.Date(), "%Y"))]
  ```
  The `assumptions` dataframe (tr2025) always contains year 2026, so this lookup is reliable regardless of birth year.

**Test Results**: 648 pass, 0 fail

---

### February 6, 2026 (Session 10) — Investigate Remaining 0.17% V.C7 Gap

**Task**: Determine whether the remaining 0.17% gap between our benefit calculations and SSA's Table V.C7 can be closed to zero. User instruction: "Don't make any permanent changes to the benefit functions though."

**Investigation**: Systematic trace of every step in the benefit calculation pipeline for medium earner born 1960 (age 65 claim). Created diagnostic scripts to test every hypothesis.

**Key Findings**:

1. **V.C7 implies AIME = 4723** for medium earner born 1960. Our AIME = 4709. The gap of 14 produces exactly $56/year less benefit ($25,116 vs $25,172). Confirmed: AIME 4723 produces $25,172 annual at age 65 through our full COLA + actuarial reduction pipeline.

2. **AWI values match exactly** — Compared our AWI series against Actuarial Note 2025.3 Table 4 for all years 1981-2024. Every value matches within 1 cent.

3. **Indexing factors are correct** — Verified AWI₂₀₂₀/AWIᵧₑₐᵣ for all years matches our computation.

4. **Scaled factor precision is the root cause** — Actuarial Note 2025.3 Table 6 publishes final scaled factors to **3 decimal places**. SSA's internal computation uses higher precision:
   - Text reveals age-22 preliminary factor = 0.279779 (Table 4 rounds to 0.280)
   - Exact medium factor = 0.279779 × 1.221 = 0.341610 (Table 6 shows 0.341)
   - But even recovering exact preliminary factors from Table 4 earnings only improves AIME by 1 (4709→4710), because Table 4 earnings are themselves computed from rounded prelim factors

5. **Table 5 ratio (1.221) is rounded** from 69472/56921 = 1.22050. Using the exact ratio actually *lowers* AIME to 4708 (career-average hits $69,472 target instead of $69,501). Using the rounded 1.221 gives AIME 4710. Neither approaches 4723.

6. **No intermediate rounding closes the gap** — Tested: earnings rounded to cent, to dollar; indexed earnings rounded to cent; all combinations. Maximum AIME achieved: 4710.

7. **The 14 AIME gap requires ~$5,880 more in top-35 indexed earnings** (~$168/year, ~0.4% of average earnings). This is consistent with the cumulative effect of 3dp factor rounding across 44 working years.

**Conclusion**: The 0.17% gap is a **data precision limitation**, not a computational error. SSA's internal benefit estimation program uses full-precision scaled factors (6+ decimal places), while the published Actuarial Note provides only 3 decimal places. Our pipeline correctly implements the published methodology but cannot reproduce SSA's exact results without access to their internal factor precision.

**No code changes made** (per user instruction). Diagnostic scripts created in `scripts/`.

**Diagnostic Scripts Created**:
- `scripts/trace_factor_precision.R` — Compares Table 4/6 factors to our CSV, computes AIME under different precision levels
- `scripts/trace_ratio_precision.R` — Tests Table 5 ratio precision impact, verifies AWI and indexing factors
- `scripts/trace_vc7_methodology.R` — Tests V.C7 annual benefit computation methodology (floor_dime, round, CPI deflation)
- `scripts/trace_vc7_final.R` — Confirms AIME=4723 matches V.C7, summarizes all findings
- `scripts/trace_aime_computation.R` — Full indexed earnings trace showing top-35 selection and needed increase

---

### February 5, 2026 (Session 9) — Add Automatic Recomputation to COLA Function

**Task**: Implement SSA's automatic recomputation (Handbook Section 715) so that when a worker earns past eligibility age 62, the increasing AIME produces a higher PIA that is properly COLA'd.

**Root Cause of V.C7 Discrepancy**: The `cola()` function in both `benefit_calculations.R` and `reform_functions.R` applied each year's COLA to the previous year's COLA'd PIA (line 499/537) but never checked whether the current year's `basic_pia` (from recalculated AIME with new earnings) was higher. SSA recalculates PIA each year and gives the beneficiary whichever is higher: the COLA'd old PIA or the new PIA with all applicable COLAs replayed from eligibility.

**Changes Made**:
- **`R/benefit_calculations.R` (cola(), lines 490-510)**: After computing `cola_forward` (the standard COLA'd PIA), check if `basic_pia[i] > basic_pia[elig_idx]`. If so, replay all COLAs from eligibility on the new basic PIA and take `max(cola_forward, recomp_pia)`.
- **`R/reform_functions.R` (cola_reform(), lines 517-555)**: Same fix, with COLA cap logic replayed correctly during recomputation.
- **All 25 regression test fixtures regenerated**.

**Verification**: V.C7 comparison for medium earner born 1960:
- Before fix: $24,768/year (−1.60% vs V.C7's $25,172)
- After fix: $25,116/year (−0.22% vs V.C7's $25,172)

Full V.C7 comparison (birth years 1960-1970):

| Worker Type | Avg % Diff | Range |
|---|---|---|
| very_low | -0.15% | [-0.28%, +0.16%] |
| low | -0.15% | [-0.22%, +0.07%] |
| medium | -0.16% | [-0.24%, -0.07%] |
| high | -0.15% | [-0.22%, +0.11%] |
| max | -0.14% | [-0.22%, +0.08%] |

**Overall: 0.17% average |difference| (improved from 0.90%)**

**Test Results**: 648 pass, 0 fail

---

### February 5, 2026 (Session 8) — Correct PIA Bend Point Off-by-One Error

**Task**: Fix PIA bend points in assumptions CSV that were hardcoded one year early (1979 values in 1978 row, 1980 values in 1979 row, etc.).

**Root Cause**: When originally entering bend point data into `2025TR_assumptions.csv`, each year's PIA bend points (bp1, bp2) were placed one year too early. For example, the 1979 row had bp1=194 (actually the 1980 value), and the statutory 1979 base amount of $180 was in the 1978 row. This caused `prep_assumptions()` to project future bend points using bp1_base=194 instead of the correct 180, producing a ~7.8% discontinuity at 2034 (the first projected year after the Trustees Report's explicit values ended at 2033).

**Changes Made**:
- **`inst/extdata/2025TR_assumptions.csv`**: Replaced bp1/bp2 for years 1979-2026 with correct values from user's verified bend point spreadsheet. Cleared 1978 bp1/bp2 to NA (bend point formula starts at 1979). Corrected 2027-2034 by shifting old values forward one year (old year Y was actually year Y+1's correct value).
- **`inst/extdata/family_max_bp.csv`**: Added 2026 row (fm_bp1=1643, fm_bp2=2371, fm_bp3=3093). Family max bend points were already correct (no off-by-one error).
- **`data/tr2025.rda`**: Regenerated via `data-raw/process_data.R` to incorporate corrected bend points.
- **All 25 regression test fixtures regenerated**: Bend point correction changes benefit amounts for all workers.

**Verification**:
- PV replacement rates now smooth across birth years 1968-1978: 0.5526 → 0.5490 → 0.5454 (no discontinuity at 1972)
- BTR similarly smooth: no jump at the 2033/2034 boundary
- `prep_assumptions()` projection now uses correct 1979 base (bp1=180, bp2=1085), producing values consistent with Trustees Report published series

**Test Results**: 648 pass, 0 fail

---

### February 5, 2026 (Session 7) — Cohort Tab: Birth Year Range Fix and dplyr::n() Bug

**Task**: Fix cohort comparison tab birth year range to match valid data boundaries, and fix a latent namespace bug.

**Investigation findings**:
- **Minimum birth year**: 1939. Workers born 1939 start working in 1960, the first year with `real_df` data. Earlier cohorts produce NaN for PV replacement rates. Birth years before 1930 fail entirely (no AWI at indexing age).
- **Maximum birth year**: 2010. Female life expectancy (89.4 years for 2075 cohort) means death in year 2100 — the last year of assumptions data. Birth year 2011 would exceed the data boundary.
- **Bend point projection discontinuity** at birth year 1972 (eligibility year 2034): Identified root cause as incorrect base values in `prep_assumptions()`. The projection uses `bp1_base=194` (1979 published value) and `awi_1977=9779.44` instead of the statutory base of $180 with the correct AWI denominator (~9,409). This causes a ~7.8% jump in bend points at the boundary between Trustees Report published values (through 2033) and projected values (2034+). **Fix deferred** — requires further discussion about the correct formula.
- **`dplyr::n()` not imported**: `rep_rates()` uses `n()` in a `summarise()` call but it was missing from the package NAMESPACE. Works in the Shiny app (dplyr is attached) but fails when called via `library(ssmbar)`.

**Changes Made**:
- **`inst/shiny/benefit_explorer/global.R`**: Changed `BIRTH_YEAR_MIN` from 1940 to 1939
- **`inst/shiny/benefit_explorer/modules/mod_cohort_tab.R`**: Replaced hardcoded `min=1955, max=2010` with `min=BIRTH_YEAR_MIN, max=BIRTH_YEAR_MAX` to use global constants
- **`R/ssmbar-package.R`**: Added `n` to `@importFrom dplyr` declaration
- **`NAMESPACE`**: Regenerated with `devtools::document()` — now includes `importFrom(dplyr,n)`

**Test Results**: 648 pass, 0 fail

---

### February 5, 2026 (Session 6) — Mid-Year Death Proration in final_benefit()

**Task**: When a spouse dies mid-year (fractional death_age), prorate between spousal and survivor benefits in the death year instead of giving full survivor benefits for the entire year.

**Problem**: Previously, `final_benefit()` zeroed out `spouse_ben_adj` entirely in the death year (`age >= worker_age_at_spouse_death`) and awarded full `survivor_ben`. With fractional `s_death_age` (e.g., 84.3), the worker should receive spousal benefits for the fraction of the year the spouse is alive (0.3) and survivor benefits for the remainder (0.7).

**Changes Made**:
- **`R/benefit_calculations.R` (`final_benefit()`)**:
  - Added backwards-compatibility check for `s_death_age` column
  - Added `spouse_frac_alive` computation: `s_death_age - floor(s_death_age)` — fraction of year spouse is alive
  - Added `is_spouse_death_year` flag: TRUE only when `age == worker_age_at_spouse_death`
  - Changed `spouse_ben_adj` from binary cutoff to three-way `case_when`: full spousal before death year, prorated in death year (`spouse_ben_fm * frac`), zero after
  - Added `survivor_ben_adj`: zero before death year, prorated in death year (`survivor_ben * (1 - frac)`), full after
  - Changed `ben` to use `if_else(is_spouse_death_year, ...)`: in death year, sums prorated spousal + prorated survivor (covering different parts of the year); in non-death years, takes max of the two (original logic, since one is always zero)
  - Updated BC code logic to use `survivor_ben_adj` instead of raw `survivor_ben` — ensures death year gets post-death BC classification (e.g., ARD instead of ARB)
- **`R/ssmbar-package.R`**: Added `survivor_ben_adj`, `spouse_frac_alive`, `is_spouse_death_year` to global variables
- **9 regression test fixtures regenerated**: All spouse-containing fixtures updated to reflect prorated death-year benefits

**Verification**: Manual check on low earner with high spouse (born 1960, claim 62/67) confirmed:
- Age 83 (pre-death): ben = $1,834 (own $1,676 + spousal $158), BC = ARB
- Age 84 (death year, frac=0.3): ben = $3,778 (own $1,717 + spousal $162×0.3 + survivor $2,875×0.7), BC = ARD
- Age 85 (post-death): ben = $4,702 (own $1,758 + survivor $2,944), BC = ARD

**Test Results**: 648 pass, 0 fail

---

### February 5, 2026 (Session 5) — Fix Cohort Discontinuities from Life Expectancy Rounding

**Task**: Investigate and fix discontinuous jumps in Cohort Comparison charts at birth years 1976 and 1994.

**Root Cause**: `death_age` in `R/earnings.R` was computed using `round()`, which caused discrete +1 year jumps when the average life expectancy crossed a `.50` boundary (86.45→87 at birth year 1976, 87.45→88 at birth year 1994). One extra year of benefits caused visible jumps in PV benefits, BTR, and IRR.

**Changes Made**:
- **`R/earnings.R`**: Removed `round()` from `death_age` calculation. Life expectancy values are now kept fractional (e.g., 86.45 instead of 86). The PV functions already support partial years via `floor(death_age)` and fractional weighting, so this produces smooth transitions across birth cohorts.
- **`R/survivor.R`**: Fixed 3 equality comparisons (`surv_s_age == surv_s_death_age`) that broke with fractional values — now uses `floor()` to match integer age against fractional death_age.
- **`R/reform_analysis.R`**: Changed `sprintf("Age %d", ...)` to `sprintf("Age %.1f", ...)` to display fractional death_age.
- **`tests/testthat/fixtures/spouse_with_drcs.rds`**: Regenerated regression fixture for the 1965 spouse-with-DRCs test case, which had a boundary change from the fractional death_age.

**Test Results**: 648 pass, 0 fail

---

### February 5, 2026 (Session 4) — Spouse Constraints and Shared Summary Statistics

**Task**: Two changes to the Individual Worker tab: (1) spouse shares primary worker's birth year and claim age, (2) when spouse selected, show individual AND shared (50/50) summary statistics.

**Changes Made**:
- **`mod_individual_tab.R`**:
  - **UI restructure**: Extracted birth year and claim age inputs from primary worker card into a shared top row above both cards. Removed `spouse_birth_year` and `spouse_claim_age` inputs from spouse card entirely.
  - **Server sync**: Replaced all `input$spouse_birth_year` → `input$birth_year` and `input$spouse_claim_age` → `input$claim_age` (4 occurrences total).
  - **Reform spouse calculation**: Added `reform_spouse_data` computation in `worker_data()` — calls `calculate_benefits_reform()` for the spouse independently when both spouse and reform are active.
  - **`compute_couple_irr()` helper**: Inline function that combines primary + spouse tax/benefit streams (merging by age), then solves for joint IRR using `uniroot()`. Uses same partial-year-at-death treatment as `internal_rate_of_return()`.
  - **`couple_stats` reactive**: Computes shared (50/50) measures when spouse is present — shared monthly benefit, shared PV benefits, shared PV taxes, shared BTR (via `couple_measures()`), and shared IRR (via `compute_couple_irr()`). Includes reform variants when reforms are active.
  - **`format_value_row()` helper**: Reusable UI builder for "Label: value → reform_value" rows with configurable color classes.
  - **5 metric renderUIs updated**: `metric_monthly`, `metric_pv_benefits`, `metric_pv_taxes`, `metric_ratio`, `metric_irr` now show both "Individual" and "Shared" rows when spouse is present. Each row supports reform arrows. Fall back to original single-value display when no spouse.
  - **Marginal metrics unchanged**: `metric_marginal_irr` and `metric_marginal_btr` remain individual-only.

**Test Results**: 648 pass, 0 fail

---

### February 5, 2026 (Session 3b) — Fix Marginal IRR Showing N/A for -100% Returns

**Task**: Marginal IRR metric showed "N/A" instead of "-100.0%" when delta_pv_benefits was 0 (worker pays taxes but accrues no additional benefits).

**Root Cause**: The Shiny display layer was filtering out IRR values of -1.0 (which represents -100%) and converting them to NA. This happened in three places:
1. `marginal_extra_year()` reactive: `mirr_val > -1` guard converted -1 to NA
2. Reform IRR filter: `marginal_irr > -1` skipped last working year if it had -1
3. Data table: `ifelse(marginal_irr == -1, NA, ...)` hid -100% values

**Fix**: Removed all three `-1 → NA` conversions. The underlying `marginal_irr()` function correctly returns -1.0 for zero-accrual years; the display should show this as -100.0%.

**Test Results**: 648 pass, 0 fail

---

### February 5, 2026 (Session 3) — Fix Employer Tax Inclusion and NMTR Reform Chart

**Task**: Fix two bugs: (1) employer-paid taxes missing from BTR and IRR calculations, (2) NMTR chart switching from bars to lines when reform selected.

**Changes Made**:
- **`mod_individual_tab.R`**: Added `include_employer = TRUE` to all 4 `pv_lifetime_taxes()` calls (2 baseline, 2 reform) for both PV Taxes metric and BTR metric. Changed NMTR reform chart from `geom_line()` to overlapping `geom_col()` — baseline bars in back (wider, semi-transparent), reform bars in front (narrower).
- **`mod_cohort_tab.R`**: Added `include_employer = TRUE` to both `pv_lifetime_taxes()` calls and both `internal_rate_of_return()` calls (were explicitly `FALSE`).

**Test Results**: 648 pass, 0 fail

---

### February 5, 2026 (Session 2) — Remove in_top_35/indexed_rank from Marginal Analysis

**Task**: Remove unused `in_top_35` and `indexed_rank` informational columns from marginal analysis functions. These columns were not used by any computation (NMTR, IRR) and had a bug in the ranking logic causing 3 test failures.

**Changes Made**:
- **`R/analytic_functions.R`**: Removed `in_top_35`/`indexed_rank` from `marginal_benefit_analysis()` (initialization, computation block, output cols, roxygen), `net_marginal_tax_rate()` (output cols, roxygen), and `marginal_irr()` (output cols, roxygen)
- **`R/ssmbar-package.R`**: Removed from globalVariables
- **`tests/testthat/test-marginal.R`**: Removed 3 failing tests (top-35 ranking, IRR-by-top-35), replaced with 1 test verifying IRR reasonableness (positive delta_pv implies IRR > -1). Updated progressivity test to filter by IRR > -1 instead of `in_top_35`.
- **`inst/shiny/benefit_explorer/modules/mod_individual_tab.R`**: Removed `in_top_35`/`indexed_rank` from marginal data table and summary computations

**Test Results**: 648 pass, 0 fail (resolved all 3 prior failures)

---

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
- `af419c7`: Update PROGRESS.md with February 5 session log (committed separately — workflow error, see below)

**Post-session fix**: Added commit workflow rules to `CLAUDE.md` to prevent future violations (committing without PROGRESS.md, skipping pre-commit test run, not reading guidelines at session start).

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
   - Returns: cumulative_aime, cumulative_pia, cumulative_pv, delta_pv_benefits
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
| 1 | ~~`worker_builder.R` contains incomplete code~~ | `R/worker_builder.R` | Rule bug | **Resolved** — file no longer exists (deleted in prior session) |
| 2 | ~~Missing automatic recomputation~~ | `R/benefit_calculations.R` | Rule bug | **Fixed** (Session 9) |

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
| data.R | tr2025 | ~~Verify le_m and le_f data source citations~~ | ☑ (Session 14 — user confirmed: cohort life expectancy tables in TR, by cohort turning 62) |

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

### Table V.C7 Re-Validation After Bend Point Correction (February 5, 2026)

After correcting the PIA bend point off-by-one error, V.C7 validation was re-run for birth years 1960-2000.

**Updated Results (Birth Years 1960-2000, Turning 65 in 2025-2065)**:

| Worker Type | Avg % Diff | Min %   | Max %   |
|-------------|-----------|---------|---------|
| very_low    | -0.78%    | -1.05%  | -0.71%  |
| low         | -0.96%    | -1.25%  | -0.88%  |
| medium      | -1.22%    | -1.60%  | -1.14%  |
| high        | -0.78%    | -1.02%  | -0.73%  |
| max         | -0.78%    | -1.08%  | -0.70%  |

**Overall: 0.90% average |difference| (improved from 1.36%)**

All differences are negative (ssmbar slightly below V.C7). No discontinuity at the 2033/2034 projection boundary. Projected years (birth 1975-2000) show stable accuracy.

**Comparison Across All Validations**:

| Validation | Overall Avg |diff| |
|---|---|
| Original (Jan 2026) | 2.51% |
| After rounding fixes (Jan 29) | 1.36% |
| **After bend point fix (Feb 5)** | **0.90%** |

**Root Cause of Remaining 0.9% Difference — Identified**:

The remaining gap is **not** caused by AIME rounding. Diagnostic testing showed that different intermediate rounding approaches (no rounding, round earnings to dollar, round indexed earnings to cent, both) all produce identical AIME values. The total lifetime earnings difference from rounding is only $0.41 across 44 years.

The actual root cause is the **missing automatic recomputation** (SSA Handbook Section 715). Our `cola()` function applies COLAs to the age-62 PIA and carries it forward, but never accounts for AIME increasing when the worker continues to earn past eligibility:

| Age | AIME | basic_pia | Our cola_pia | With Recomp | Gap |
|-----|------|-----------|-------------|-------------|-----|
| 62 | 4,618 | $2,071.60 | $2,071.60 | $2,071.60 | $0 |
| 63 | 4,654 | $2,083.20 | $2,251.80 | $2,264.40 | $12.60 |
| 64 | 4,683 | $2,092.40 | $2,323.80 | $2,347.10 | $23.30 |
| 65 | 4,709 | $2,100.80 | $2,381.80 | $2,415.40 | $33.60 |

With recomputation, the medium earner benefit becomes $25,120/year vs V.C7's $25,172 (0.21% gap vs current 1.60%). **Fixed in Session 9** — overall accuracy now 0.17%.

---

### Table V.C7 Re-Validation After Automatic Recomputation Fix (February 5, 2026)

After adding automatic recomputation to `cola()`, V.C7 validation re-run for birth years 1960-1970.

**Updated Results**:

| Worker Type | Avg % Diff | Range |
|---|---|---|
| very_low | -0.15% | [-0.28%, +0.16%] |
| low | -0.15% | [-0.22%, +0.07%] |
| medium | -0.16% | [-0.24%, -0.07%] |
| high | -0.15% | [-0.22%, +0.11%] |
| max | -0.14% | [-0.22%, +0.08%] |

**Overall: 0.17% average |difference| (improved from 0.90%)**

**Comparison Across All Validations**:

| Validation | Overall Avg |diff| |
|---|---|
| Original (Jan 2026) | 2.51% |
| After rounding fixes (Jan 29) | 1.36% |
| After bend point fix (Feb 5) | 0.90% |
| **After recomputation fix (Feb 5)** | **0.17%** |

**Remaining 0.17% Difference** (resolved in Session 10): Traced to scaled factor precision. Actuarial Note 2025.3 Table 6 publishes final factors to 3 decimal places; SSA's internal computation uses 6+ decimal places. For medium earner born 1960: our AIME = 4709, V.C7 implies AIME = 4723 (gap = 14). The 3dp rounding loses ~0.4% of earnings at each age, accumulating to ~$5,880 in top-35 indexed earnings. AWI values, indexing factors, COLAs, bend points, and actuarial reduction all match exactly. No intermediate rounding approach closes the gap. This is a data precision limitation, not a computational error.

---

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

648 tests passing (as of February 10, 2026):
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
