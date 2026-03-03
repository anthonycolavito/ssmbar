# ssmbar Technical Reference

## R Package Architecture

### Goals
1. **Benefit Calculation**: Calculate annual Social Security benefits for any hypothetical worker configuration through their lifetime
2. **Distributional Analysis**: Replacement rates and benefit adequacy across worker types and birth cohorts
3. **Policy Reform Modeling**: ~20 reform templates with distributional impact analysis

### Benefit Calculation Pipelines

**Baseline Pipeline** (current law):
```
Earnings → aime() → pia() → cola() → worker_benefit() → spousal_pia()
→ spouse_benefit() → child_pia() → child_benefit() → family_maximum()
→ widow_pia() → widow_benefit() → ret() → final_benefit()
```

**Reform Pipeline** (policy reform modeling):
```
Earnings → aime_reform() → pia_reform() → cola_reform() → worker_benefit()
→ basic_minimum_benefit() → spousal_pia() → spouse_benefit() → child_pia()
→ child_benefit() → family_maximum() → widow_pia() → widow_benefit_reform()
→ ret_reform() → final_benefit()
```

### Key Functions

| Function | Purpose |
|----------|---------|
| `calculate_benefits()` | Full baseline pipeline in one call |
| `calculate_benefits_reform()` | Full reform pipeline: `(birth_yr, sex, type, age_claim, factors, assumptions, ...)` |
| `earnings_generator()` | Creates lifetime earnings from scaled earnings factors |
| `aime()` / `aime_reform()` | Average Indexed Monthly Earnings |
| `pia()` / `pia_reform()` | Primary Insurance Amount (bend point formula) |
| `cola()` / `cola_reform()` | Cost-of-Living Adjustments |
| `worker_benefit()` | Early retirement / delayed credits |
| `ret()` / `ret_reform()` | Retirement Earnings Test |
| `final_benefit()` | Combines worker + max(spousal, survivor) benefits |
| `apply_reform(assumptions, reform)` | Apply single Reform object to assumptions |
| `apply_reforms(assumptions, list, check_exclusivity)` | Apply multiple reforms sequentially |

### Present Value Functions

| Function | Purpose |
|----------|---------|
| `pv_lifetime_benefits()` | Discounted annual benefits from claim_age to death_age |
| `pv_lifetime_taxes()` | Discounted SS taxes from age 21-64 |
| `benefit_tax_ratio()` | Ratio of PV benefits to PV taxes |
| `couple_measures()` | Combined worker + spouse calculations |
| `internal_rate_of_return()` | IRR treating taxes as contributions, benefits as payouts |
| `rep_rates()` | Replacement rates (use `real_all` type for site) |

### Code Organization

| File | Contents |
|------|----------|
| `R/baseline_benefit_calculations.R` | Baseline `aime()`, `pia()`, `cola()` |
| `R/baseline_ret.R` | Baseline `ret()` |
| `R/baseline_survivor.R` | Baseline `widow_benefit()` |
| `R/benefit_calculations.R` | Reform-capable `aime_reform()`, `pia_reform()`, `cola_reform()` + shared functions |
| `R/calculate_benefits.R` | `calculate_benefits()`, `calculate_benefits_reform()` |
| `R/ret.R` | Reform-capable `ret_reform()` |
| `R/survivor.R` | Reform-capable `widow_benefit_reform()` + shared `widow_pia()` |
| `R/reform_templates.R` | All reform factory functions (~20) |
| `R/reform.R` | `apply_reform()`, `apply_reforms()`, Reform class |
| `R/pv_functions.R` | Present value, IRR, replacement rates |
| `data/` | `tr2025.rda`, `sef2025.rda` — Trustees Report assumptions and scaled earnings factors |
| `inst/extdata/` | Raw data files |
| `data-raw/process_data.R` | Processes raw data into package data objects |
| `tests/testthat/` | Test suite |

### Data Objects
- `tr2025` — Trustees Report assumptions (AWI, bend points, COLA factors, NRA, mortality, etc.)
- `sef2025` — Scaled Earnings Factors for worker types

### Worker Types
- `very_low`, `low`, `medium`, `high`, `max` — Preset SSA scaled workers
- `custom` — Specify average real earnings via `custom_avg_earnings` parameter

### Worker ID Format
`{type}-{sex}-{birthyr}-{claimage}` — e.g., `"medium-male-1960-67"`, `"custom50000-female-1970-65"`

### Sex Parameter
`"male"`, `"female"`, `"all"` (unisex — gender-neutral averaged mortality)

### Usage Examples

```r
# Baseline
worker <- calculate_benefits(
  birth_yr = 1960, sex = "all", type = "medium",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Single reform
reform <- reform_chained_cpi(2026)
assum <- apply_reform(tr2025, reform)
worker_reform <- calculate_benefits_reform(
  birth_yr = 1960, sex = "all", type = "medium",
  age_claim = 65, factors = sef2025, assumptions = assum, debugg = TRUE
)

# Multiple reforms combined
reforms <- list(reform_chained_cpi(2026), reform_nra_to_68(2026))
assum <- apply_reforms(tr2025, reforms, check_exclusivity = FALSE)
```

### RET (Retirement Earnings Test) Details

The `ret()` function applies the RET per SSA Handbook Chapter 18:
- **Who**: Workers with earnings > ret1 exempt amount, between claim_age and NRA
- **Reduction**: (earnings - ret1) / 2 annually
- **Spouse handling**: If worker has spouse_spec, includes spouse's dependent benefit in total pot; reduction allocated proportionally
- **DRC payback**: At NRA, actuarial factor is recalculated as if worker claimed later (by months withheld / 12)

### Correct Spousal Benefit Formula
```
spouse_pia = (50% × spouse_PIA - own_PIA)
spouse_ben = floor(spouse_pia × spousal_actuarial_factor)
```
An alternative method was tested and reverted — see PROGRESS.md Session 2026-01-23.

### Statutory Basis

| Calculation | Statutory Authority |
|-------------|---------------------|
| AIME computation | 42 USC 415(b) |
| PIA formula | 42 USC 415(a) |
| COLA | 42 USC 415(i) |
| Old-age benefits | 42 USC 402(a) |
| Spousal benefits | 42 USC 402(b)-(c) |
| Widow(er) benefits | 42 USC 402(e)-(f) |
| Actuarial reduction | 42 USC 402(q) |
| Delayed retirement credits | 42 USC 402(w) |
| Taxable maximum | 42 USC 430 |
| Retirement age | 42 USC 416(l) |

### Validation Status

Package output validated against SSA's Table V.C7 (2025 Trustees Report).
- Overall: 0.17% average difference across all worker types and birth years 1960-1970
- Remaining difference: scaled factor precision (3 vs 6+ decimal places)

---

## Static Site (Benefit Explorer)

**URL**: https://anthonycolavito.github.io/ssmbar/
**Serves from**: `docs/` directory on master branch

### Site Architecture
Pre-computed JSON data rendered client-side with Chart.js. No server needed.

### Key Site Files
- `docs/index.html` — Single-page app (Bootstrap 5, light theme)
- `docs/js/app.js` — Main controller, event wiring, data flow
- `docs/js/data-loader.js` — JSON fetch with LRU cache
- `docs/js/chart-manager.js` — Chart.js chart builders (benefits, NMTR, cohort)
- `docs/js/table-manager.js` — HTML tables + CSV export
- `docs/js/ui-controls.js` — Hero controls, sidebar, reform selector state
- `docs/js/formatters.js` — Number/currency/percent formatters
- `docs/css/style.css` — All site styles
- `docs/data/manifest.json` — Metadata, dimensions, reform labels, file patterns
- `scripts/generate_currentlaw_data.R` — Baseline data generation
- `scripts/generate_reform_data.R` — Reform data generation

### Current Site Dimensions
- **Worker types**: very_low, low, medium, high, max, custom_50k (6)
- **Sex**: unisex only (`sex = "all"`)
- **Claim age**: 65 only
- **Birth years**: 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010 (8)
- **Reform categories** (5, exclusive — pick 0 or 1 from each):
  1. PIA: reduce_fact3, flat_benefit, simpson_bowles_pia
  2. NRA: nra_to_68, index_nra, nra_to_69_index
  3. COLA: chained_cpi, cola_cap, cpi_e
  4. Tax Max: taxmax_90_pct, eliminate_taxmax, eliminate_taxmax_no_credit (locked)
  5. Other: forty_year_avg, mini_pia, basic_minimum, repeal_ret (locked)
- No spouses on static site
- No "None" radio buttons — click-to-deselect instead

### Combo Key Format
Non-"none" reform names joined with `+` in category order (pia, nra, cola, taxmax, other):
- `chained_cpi` (single reform)
- `reduce_fact3+nra_to_68+chained_cpi` (multi-reform)

### Data File Structure
- `docs/data/cohort/{type}.json` — Baseline cohort metrics (keyed by `{sex}_{marital}`)
- `docs/data/individual/{type}_benefits.json` — Baseline benefit series by age
- `docs/data/individual/{type}_nmtr.json` — Net marginal tax rate series
- `docs/data/reform/cohort/{type}.json` — Reform cohort metrics (keyed by combo key)
- `docs/data/reform/individual/{type}.json` — Reform benefit series (keyed by combo key)

---

## Adding Reforms to the Benefit Explorer

### Overview
Reform data lives in `docs/data/reform/`, generated by `scripts/generate_reform_data.R`. Each reform category is exclusive (pick 0 or 1 from each). Combo keys are non-"none" reform names joined with `+` in category order: pia, nra, cola, taxmax, other.

### How to Add a New Reform Category

1. **Define the reform in `scripts/generate_reform_data.R`**:
   - Add an entry to `reform_defs` with `effective_year`, `phase_type`, and `reforms` list
   - `phase_type` is either `"cohort"` (phases in by birth year / eligibility age) or `"calendar"` (phases in by calendar year)
   - Each reform entry is a zero-argument factory function that returns a Reform object

2. **Update `valid_cats`** in the CLI arg validation to include the new category name

3. **Update skip logic in `get_birth_years_for_combo()`**:
   - This is critical for avoiding unnecessary computation
   - See "Skip Logic" below for how to determine which birth years to skip

4. **Unlock the category in the sidebar** (`docs/index.html`):
   - Change the `<div class="reform-category locked">` to an interactive expandable category
   - Add `<button class="reform-option">` elements with `onclick="selectReform('cat', 'name', this)"`

5. **Update `docs/data/manifest.json`**:
   - Add the category to `active_reform_categories`
   - Add entries to `reform_labels` for any new reform keys

6. **Regenerate data**: `Rscript scripts/generate_reform_data.R --categories pia,nra,cola,newcat`
   - Combo count grows multiplicatively: (options+1) per category, minus 1 for all-none baseline
   - Currently: PIA(4) × NRA(4) × COLA(4) - 1 = 63 combos
   - Adding a 4th category with 3 options: 4×4×4×4 - 1 = 255 combos

### Skip Logic (Avoiding Unnecessary Calculations)

**This is the most important optimization.** Each reform has an `effective_year` and a `phase_type` that determines which birth cohorts are affected:

- **`phase_type = "cohort"`** (PIA, NRA reforms): The reform affects workers who reach eligibility age (62) on or after `effective_year`. Workers born before `effective_year - 62` are unaffected. Example: PIA with `effective_year = 2030` → first affected cohort born 1968 → skip birth years 1940, 1950, 1960.

- **`phase_type = "calendar"`** (COLA reforms): The reform takes effect in a calendar year regardless of birth year. Workers who are already dead before `effective_year` are unaffected. Use life expectancy (~83-88 for birth years 1940-2010) to determine. Example: COLA with `effective_year = 2026` → 1940 cohort dies ~age 83 (year 2023) → skip 1940.

- **For combo keys**: Skip a birth year only if NONE of the active reforms in the combo affect that birth year. If even one reform in the combo has an effect, compute it.

### Spot-Checking New Reform Data

After generating data for a new category, always validate:

1. **Exact value match** (7+ spot checks): Pick diverse combos (single reforms, cross-category combos), worker types, and birth years. Compute the values fresh in R and compare against the JSON. All 8 metrics should match: `monthly_benefit`, `pv_benefits`, `pv_taxes`, `ratio`, `irr`, `repl_rate`, `initial_real_benefit`, `death_age`.

2. **Direction check**: For each reform, compare against the unisex baseline (average of male + female from `docs/data/cohort/{type}.json`) and verify the direction makes economic sense:
   - Benefit-cutting reforms (higher NRA, lower COLA) → lower monthly benefit, lower ratio
   - Benefit-increasing reforms (CPI-E) → higher ratio, higher PV
   - Reforms that don't apply to a worker type → zero or near-zero delta
   - Combo reforms → deltas should compound (not just equal the largest single reform)

3. **Baseline identity check**: A reform that doesn't affect a particular worker/cohort should produce values very close to baseline. The reform pipeline uses `sex="all"` while baseline stores male/female separately (JS averages); expect ~0.05% difference in ratio due to this methodological difference, but monthly_benefit should be identical.

### Key Technical Details

- Reform data uses `sex = "all"` (unisex), single only, no spouse support
- Multiple reforms are combined via `apply_reforms(tr2025, list_of_reforms, check_exclusivity = FALSE)` — applies them sequentially to the assumptions object
- `apply_reforms()` is called once per combo (not per birth year), since reforms modify the assumptions globally
- The generation script uses `mclapply` with 6 cores for parallel computation across birth years within each combo

### Existing Reform Templates (in `R/reform_templates.R`)

| Function | Category | What It Does |
|----------|----------|-------------|
| `reform_reduce_fact3(target, eff_yr)` | PIA | Reduces 3rd PIA factor (15% → target) |
| `reform_flat_benefit(flat_amount, eff_yr)` | PIA | Replaces PIA formula with flat amount |
| `reform_simpson_bowles(eff_yr)` | PIA | 4-bracket PIA: 90/30/10/5 |
| `reform_nra_to_68(eff_yr)` | NRA | Raise age from 67 to 68 |
| `reform_index_nra(eff_yr)` | NRA | Index age to life expectancy |
| `reform_nra_to_69_index(eff_yr)` | NRA | Raise age to 69, then index to life expectancy |
| `reform_chained_cpi(eff_yr)` | COLA | COLAs use chained CPI (lower) |
| `reform_cola_cap(eff_yr)` | COLA | Cap COLAs for top half of earners |
| `reform_cpi_e(eff_yr)` | COLA | COLAs use CPI-E elderly index (higher) |
| `reform_taxmax_90_pct(eff_yr)` | Tax Max | Raise taxmax to cover 90% of wages |
| `reform_eliminate_taxmax(eff_yr)` | Tax Max | Remove taxmax with benefit credit |
| `reform_eliminate_taxmax_no_credit(eff_yr)` | Tax Max | Remove taxmax, no benefit credit |
| `reform_40_year_averaging(eff_yr)` | Other | 40-year averaging period (vs 35) |
| `reform_mini_pia(eff_yr)` | Other | Apply PIA formula to annual earnings |
| `reform_basic_minimum(amount, eff_yr)` | Other | Minimum benefit floor |
| `reform_repeal_ret(eff_yr)` | Other | Repeal Retirement Earnings Test |

---

## Refactoring Status

Phases 1-6 completed (testing baseline, parameterization, spouse refactor, RET decompose, reform infrastructure, performance optimization). Phase 7 (documentation/cleanup) partially complete. See PROGRESS.md for details.
