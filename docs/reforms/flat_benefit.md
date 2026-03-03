# Flat Benefit Floor

## Summary

Establishes a flat monthly benefit floor equal to 125% of the federal poverty level ($19,300/year in 2025$), while phasing down the upper PIA replacement factors to flatten the benefit structure.

## Parameters Modified

| Parameter | Current Law | Reform Value | Type |
|-----------|-------------|--------------|------|
| `flat_benefit` | `NA` (disabled) | `$1,608.33/month` (2025$), AWI-indexed | replace |
| `fact2` | `0.32` (32%) | `0.04` (4%) | replace |
| `fact3` | `0.15` (15%) | `0.00` (0%) | replace |

## Effective Year

2030 (as configured in `generate_reform_data.R`)

## Phase-In

- **Duration**: 25 eligibility cohorts
- **Dimension**: Cohort (by year turning 62)
- **Mechanism**: `fact2` and `fact3` linearly interpolate from current-law values toward reform targets. `flat_benefit` activates immediately when `phase_factor > 0` (no blending for NA→value transitions).
- **Example**: First affected cohort turns 62 in 2030 (born 1968). Fully phased in for cohort turning 62 in 2054 (born 1992).

## Indexing

The flat benefit amount is **AWI-indexed with a two-year lag**, identical to how bend points (`bp1`, `bp2`), taxable maximum, and quarter-of-coverage thresholds are indexed.

**Formula**: `flat_benefit(year) = $1,608.33 × AWI(year - 2) / AWI(2023)`

- AWI base year: 2023 (corresponds to 2025$ via the standard two-year lag)
- AWI(2023) = $66,621.80 (from 2025 Trustees Report)

The AWI schedule is captured in a closure at reform creation time when `assumptions` is provided to `reform_flat_benefit()`.

## Formula

Worker's PIA under this reform:

```
formula_pia = floor_dime(
  fact1 × bp1 +
  fact2_phased × (bp2 - bp1) +
  fact3_phased × (AIME - bp2)
)

final_pia = max(formula_pia, flat_benefit_indexed)
```

Where:
- `fact2_phased` interpolates from 0.32 → 0.04 over 25 years
- `fact3_phased` interpolates from 0.15 → 0.00 over 25 years
- `flat_benefit_indexed` = `$1,608.33 × AWI(elig_year - 2) / AWI(2023)`
- PIA = max of formula result and flat benefit (evaluated at eligibility year, age 62)

## Expected Behavior

| Worker Type | Effect |
|-------------|--------|
| Very low | Flat benefit floor is binding → benefit **increases** relative to current law |
| Low | Flat benefit floor likely binding → benefit increases |
| Medium | Reduced formula PIA dominates, flat floor may or may not bind depending on cohort |
| High | Significant benefit reduction from fact2/fact3 cuts; flat floor well below formula PIA |
| Max | Large benefit reduction; flat floor far below formula PIA |

**Key property**: When fully phased in, very low and low earners receive essentially the same monthly benefit (the flat amount), creating a more progressive benefit structure.

## Code References

- **Template**: `R/reform_templates.R` — `reform_flat_benefit()`
- **Consumption**: `R/reform_functions.R:364` — `flat_benefit_elig` extracted at eligibility age
- **PIA logic**: `R/reform_functions.R:402–407` — `max(formula_pia, flat_benefit_elig)`
- **Generation**: `scripts/generate_reform_data.R:81` — factory passes `assumptions = tr2025`
