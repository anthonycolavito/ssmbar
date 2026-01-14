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

### File Locations
- R source: `R/`
- Package data: `data/` (tr2025.rda, sef2025.rda)
- Raw data: `inst/extdata/`
- Data processing: `data-raw/process_data.R`

### Known Issues
- `legacy/worker_builder.R` is broken legacy code (moved out of R/ directory)
- Rtools 4.5 not installed on dev machine (4.4 available but incompatible with R 4.5.0)

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

## RET (Retirement Earnings Test) Details

The `ret()` function applies the Retirement Earnings Test per SSA Handbook Chapter 18:

- **Who**: Workers with earnings > ret1 exempt amount, between claim_age and NRA
- **Reduction**: (earnings - ret1) / 2 annually
- **Spouse handling**: If worker has spouse_spec, includes spouse's dependent benefit in total pot; reduction allocated proportionally
- **DRC payback**: At NRA, actuarial factor is recalculated as if worker claimed later (by months withheld / 12)
- **Debug output**: `excess_earnings`, `ret_reduction`, `months_withheld`, `cum_months_withheld`, `ret_adj_factor`, `spouse_dep_ben`
