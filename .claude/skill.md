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
- **R 4.5.2**: `C:/Users/AnthonyColavito/AppData/Local/Programs/R/R-4.5.2/bin/R.exe`
- **Rscript**: `C:/Users/AnthonyColavito/AppData/Local/Programs/R/R-4.5.2/bin/Rscript.exe`
- **Package root**: `C:/Users/AnthonyColavito/Dev/ssmbar`
- **GitHub repo**: `https://github.com/anthonycolavito/ssmbar.git`
- **Rtools**: Not installed on this computer

### Package File Locations
- R source: `R/`
- Package data: `data/` (tr2025.rda, sef2025.rda)
- Raw data: `inst/extdata/`
- Data processing: `data-raw/process_data.R`

### Known Issues
- `legacy/worker_builder.R` is broken legacy code (moved out of R/ directory)
- Rtools not installed on work computer (CRFB)

### Running R Scripts
To run R scripts or test the package:
```bash
cd /c/Users/AnthonyColavito/Dev/ssmbar
"/c/Users/AnthonyColavito/AppData/Local/Programs/R/R-4.5.2/bin/Rscript.exe" -e "library(devtools); load_all(); load('data/tr2025.rda'); load('data/sef2025.rda'); # your code here"
```

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

## Future Work To-Do List

### 1. Add Additional Documentation
- Fill in all `TODO-DOC:` markers throughout the R files
- Add SSA Handbook section references where marked
- Document program rules for each benefit calculation step

### 2. Move Eligibility Ages Out of Hard Code
- Currently age 62 (worker eligibility) and age 60 (spousal eligibility) are hard-coded
- Move these to the assumptions dataframe to allow policy modeling
- Affected functions: `aime()`, `pia()`, `cola()`, `spousal_pia()`, `generate_spouse_dependent_benefit()`

### 3. Create Lifetime Benefit Measures
- **Real lifetime benefits**: Sum of inflation-adjusted annual benefits over lifetime
- **Present value (PV) lifetime benefits**: Discounted sum using interest rate from assumptions
- Consider mortality adjustments (expected benefits accounting for survival probability)

### 4. Create Lifetime Earnings Measures
- **Real lifetime earnings**: Sum of inflation-adjusted earnings over working life
- **Present value (PV) lifetime earnings**: Discounted sum using interest rate
- May want to calculate through different endpoints (age 62, NRA, claim age)

### 5. Create Lifetime Tax Measures
- **Real lifetime SS taxes**: Sum of inflation-adjusted payroll taxes paid
- **Present value (PV) lifetime taxes**: Discounted sum using interest rate
- Need to apply OASDI tax rates (from assumptions) to capped earnings

### 6. Create Replacement Rate Measures
- Ratio of initial benefit to pre-retirement earnings
- Options: benefit at claim age vs. AIME, career-average earnings, final years earnings
- Consider both individual and couple replacement rates

### 7. Create Annual Real Benefit Measures
- Add `real_ben` column: nominal benefit adjusted to constant dollars
- Use GDP price index or CPI-W for inflation adjustment
- Allow user to specify base year for real values

### 8. Add Interest Rate to Assumptions Dataset
- Add `int_rate` (or `discount_rate`) column to tr2025
- Source from Trustees Report intermediate assumptions
- Used for PV calculations in items 3-5 above
