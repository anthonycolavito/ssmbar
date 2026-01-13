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
Earnings → AIME → PIA → COLA → Actuarial Adjustment → Final Benefit
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
- `tr2025` - Trustees Report assumptions (AWI, bend points, COLA factors, etc.)
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
- `worker_builder.R` is broken legacy code (excluded from build via .Rbuildignore)
- Rtools 4.5 not installed on dev machine (4.4 available but incompatible with R 4.5.0)

### R CMD Check Status
- Passes with 0 errors, 0 warnings, 1 note (expected for dev version)

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
