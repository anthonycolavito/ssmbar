# ssmbar - Project Context for Claude

## Package Purpose

**ssmbar** (Social Security Microsimulation Benefit Calculator) is an R package that accurately calculates Social Security retirement benefits for hypothetical workers using the exact formulas, rules, and parameters defined by the Social Security Administration.

## Goals (Priority Order)

1. **Benefit Calculation**: Specify any hypothetical worker configuration (e.g., "medium-earner born in 1960 with a low-earner spouse born in 1963") and calculate their annual Social Security benefits at every year of their life based on their earnings profile.

2. **Distributional Analysis**: Use calculated benefits to answer questions like "What would the replacement rate be for a worker with $50,000 (2025 dollars) in average earnings if they were born in 1960 versus 1980?"

3. **Policy Reform Modeling** (Future): Model hypothetical Social Security reforms and analyze their distributional impact across different worker types.

## Technical Implementation

### The Benefit Calculation Pipeline

```
Earnings → AIME → PIA → COLA Adjustment → Actuarial Adjustment → Final Benefit
```

| Step | Function | What It Does |
|------|----------|--------------|
| 1 | `earnings_generator()` | Creates lifetime earnings using Trustees' scaled earnings factors |
| 2 | `aime()` | Computes Average Indexed Monthly Earnings (indexes to age 60, caps at taxmax, averages highest 35 years) |
| 3 | `pia()` | Computes Primary Insurance Amount using bend point formula (90/32/15) |
| 4 | `spousal_pia()` | Calculates spousal benefit (50% of spouse's PIA minus own PIA) |
| 5 | `cola()` | Applies Cost-of-Living Adjustments using CPI-W |
| 6 | `worker_benefit()` | Applies early retirement reduction factors or delayed retirement credits |
| 7 | `spouse_benefit()` | Applies actuarial adjustments to spousal benefits |
| 8 | `final_benefit()` | Combines worker + spousal benefits |

### Convenience Function

`calculate_benefits()` chains all steps together for easy single-call benefit calculation.

### Data Structure

- `tr{YEAR}` (e.g., `tr2025`): Processed Trustees Report assumptions
- `sef{YEAR}` (e.g., `sef2025`): Scaled earnings factors
- Raw data in `inst/extdata/`, processed via `data-raw/process_data.R`

## Design Principles

1. **Accuracy**: Every function implements specific SSA Handbook rules (citations in code comments)
2. **Transparency**: `debugg` parameter exposes intermediate calculations
3. **Flexibility**: Supports preset worker types (very_low, low, medium, high, max) OR custom average earnings
4. **Future-proofing**: Parameters come from data, not hardcoded values
5. **Updateability**: New Trustees Reports only require data updates

## Working Guidelines

- **Always commit changes** with meaningful, descriptive messages explaining what and why
- **Never delete files** without explicit user permission
- **Ask permission** if uncertain about any action
- All development should support the goals above

## Known Issues

- `worker_builder.R`: Contains incomplete/broken code (references undefined `dataset` variable) - this file needs work or removal

## Owner

Anthony Colavito (colavito@crfb.org) - Committee for a Responsible Federal Budget
