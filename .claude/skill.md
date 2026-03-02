# ssmbar Package - Claude Skill Guide

## Working Rule

Before approaching any problem, search through PROGRESS.md, CLAUDE.md, CLAUDE_GUIDELINES.md, and this skill file to check if there is already a documented method, prior solution, or relevant context. Don't reinvent what's already been solved or repeat mistakes that have been recorded.

## Package Purpose

**ssmbar** (Social Security Microsimulation Benefit Calculator) is an R package that calculates Social Security retirement benefits for hypothetical workers using the exact SSA formulas and parameters.

## Owner

Anthony Colavito (colavito@crfb.org) - Committee for a Responsible Federal Budget

## Goals (Priority Order)

1. **Benefit Calculation**: Calculate annual Social Security benefits for any hypothetical worker configuration through their lifetime
2. **Distributional Analysis**: Answer questions like "What replacement rate would a $50k earner born in 1960 vs 1980 receive?"
3. **Policy Reform Modeling**: Model hypothetical SS reforms and their distributional impact

## Technical Architecture

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
| `calculate_benefits()` | Convenience wrapper - full baseline pipeline in one call |
| `calculate_benefits_reform()` | Convenience wrapper - full reform pipeline in one call |
| `earnings_generator()` | Creates lifetime earnings (vectorized for multiple workers) |
| `aime()` / `aime_reform()` | Average Indexed Monthly Earnings |
| `pia()` / `pia_reform()` | Primary Insurance Amount (bend point formula) |
| `cola()` / `cola_reform()` | Cost-of-Living Adjustments |
| `worker_benefit()` | Applies early retirement/delayed credits |
| `spousal_pia()` | Spousal benefit calculation |
| `spouse_benefit()` | Spousal benefit with actuarial adjustments |
| `widow_pia()` | Survivor PIA from deceased spouse's record |
| `widow_benefit()` / `widow_benefit_reform()` | Survivor benefit with actuarial adjustment |
| `ret()` / `ret_reform()` | Retirement Earnings Test |
| `final_benefit()` | Combines worker + max(spousal, survivor) benefits |

### Worker ID Format
`{type}-{sex}-{birthyr}-{claimage}`
- Examples: `"medium-male-1960-67"`, `"custom50000-female-1970-65"`

### Data Objects
- `tr2025` - Trustees Report assumptions (AWI, bend points, COLA factors, ret1/ret2, etc.)
- `sef2025` - Scaled Earnings Factors for worker types

### Worker Types
- `very_low`, `low`, `medium`, `high`, `max` - Preset SSA scaled workers
- `custom` - Specify average real earnings directly

### Sex Parameter
- `"male"`, `"female"`, `"all"` (gender-neutral / unisex mortality)

## Usage Examples

```r
# Single worker
worker <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium",
  age_claim = 67, factors = sef2025, assumptions = tr2025
)

# With debugg = TRUE for intermediate calculations
worker <- calculate_benefits(
  birth_yr = 1960, sex = "all", type = "medium",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Reform scenario
reform <- reform_chained_cpi(2030)
assum <- apply_reform(tr2025, reform)
worker_reform <- calculate_benefits_reform(
  birth_yr = 1960, sex = "all", type = "medium",
  age_claim = 65, factors = sef2025, assumptions = assum, debugg = TRUE
)

# Multiple reforms combined
reforms <- list(reform_chained_cpi(2030), reform_nra_to_68(2030))
assum <- apply_reforms(tr2025, reforms, check_exclusivity = FALSE)
```

## Static Site (Benefit Explorer)

**URL**: https://anthonycolavito.github.io/ssmbar/
**Serves from**: `docs/` directory on master branch

### Site Architecture
Pre-computed JSON data rendered client-side with Chart.js. No server needed.

### Key Site Files
- `docs/index.html` — Single-page app (Bootstrap 5 dark theme)
- `docs/js/app.js` — Main controller
- `docs/js/data-loader.js` — JSON fetch with LRU cache
- `docs/js/chart-manager.js` — Chart.js chart builders
- `docs/js/table-manager.js` — HTML tables + CSV export
- `docs/js/ui-controls.js` — Dropdowns, sidebar, reform selector
- `docs/data/manifest.json` — Metadata, dimensions, reform labels
- `scripts/generate_combo_data.R` — Data generation script

### Current Site Dimensions
- **Worker types**: very_low, low, medium, high, max, custom_50k (6)
- **Sex**: unisex only (`sex = "all"`)
- **Claim age**: 65 only
- **Birth years**: 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010 (8)
- **Reform categories** (5, exclusive — pick 0 or 1 from each):
  1. PIA: reduce_fact3, flat_benefit, simpson_bowles_pia
  2. NRA: nra_to_68, index_nra, nra_to_69_index
  3. COLA: chained_cpi, cola_cap, cpi_e
  4. Tax Max: taxmax_90_pct, eliminate_taxmax, eliminate_taxmax_no_credit
  5. Other Reforms: forty_year_avg, repeal_ret, mini_pia, basic_minimum
- **1,280 reform combinations** (4 × 4 × 4 × 4 × 5)
- No spouses on static site
- No "None" radio buttons — click-to-deselect instead

### Combo Key Format
Non-"none" reform names joined with `+` in category order (pia, nra, cola, taxmax, other):
- `baseline` (nothing selected)
- `chained_cpi` (single reform)
- `reduce_fact3+nra_to_68+chained_cpi` (multi-reform)

## Development Environment

### Mac (current)
- R 4.0.3 at `/usr/local/bin/Rscript`
- ssmbar is NOT installed as a package — use `devtools::load_all(".", quiet = TRUE)` to load
- 8 CPU cores available; use 6 for parallel work (`mclapply`)
- Kill zombie R processes before data generation: `ps aux | grep "R --no-echo" | grep -v grep`

### Windows (CRFB)
- R 4.5.0 at `C:/Users/AnthonyColavito/AppData/Local/Programs/R/R-4.5.0/bin/Rscript.exe`
- Rtools not installed

### Package File Locations
- R source: `R/`
- Package data: `data/` (tr2025.rda, sef2025.rda)
- Raw data: `inst/extdata/`
- Data processing: `data-raw/process_data.R`
- Tests: `tests/testthat/`
- Test fixtures: `tests/testthat/fixtures/`

## Key Rules

### Commit Workflow (Non-Negotiable)
1. Update PROGRESS.md in the same commit
2. Run `devtools::test()` immediately before committing (not a stale run)
3. Stage specific files only (never `git add -A`)
4. Write descriptive commit message

### Benefit Calculation Changes (Elevated Standard)
Any change to core pipeline functions requires:
1. Run validation scripts (not just unit tests)
2. Verify every intermediate step
3. Manual inspection of code logic
4. Unit tests against expected values
5. Validation against external sources (Table V.C7, Actuarial Notes)

### Correct Spousal Benefit Formula
```
spouse_pia = (50% × spouse_PIA - own_PIA)
spouse_ben = floor(spouse_pia × spousal_actuarial_factor)
```
An alternative method was tested and reverted — see PROGRESS.md Session 2026-01-23.

### Objective Evaluation
Do not be eager to validate the user's point of view. If the user is correct, confirm it. But if they may not be correct, explain how and why. Apply rigorous standards to all ideas and respectfully disagree when necessary.

## RET (Retirement Earnings Test) Details

The `ret()` function applies the Retirement Earnings Test per SSA Handbook Chapter 18:
- **Who**: Workers with earnings > ret1 exempt amount, between claim_age and NRA
- **Reduction**: (earnings - ret1) / 2 annually
- **Spouse handling**: If worker has spouse_spec, includes spouse's dependent benefit in total pot; reduction allocated proportionally
- **DRC payback**: At NRA, actuarial factor is recalculated as if worker claimed later (by months withheld / 12)

## Refactoring Status

Phases 1-6 completed (testing baseline, parameterization, spouse refactor, RET decompose, reform infrastructure, performance optimization). Phase 7 (documentation/cleanup) partially complete. See PROGRESS.md for full details and decisions made.

## Validation Status

Package output validated against SSA's Table V.C7 (2025 Trustees Report).
- Overall: 0.17% average difference across all worker types and birth years 1960-1970
- Remaining difference traced to scaled factor precision (3 vs 6+ decimal places)
