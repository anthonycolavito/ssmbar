# Reduce Third Replacement Factor (Top 20%)

## Summary

Reduces the third PIA replacement factor from 15% to 5%, cutting benefits for high earners whose AIME exceeds the second bend point.

## Parameters Modified

| Parameter | Current Law | Reform Value | Type |
|-----------|-------------|--------------|------|
| `fact3` | `0.15` (15%) | `0.05` (5%) | replace |

## Effective Year

2030 (as configured in `generate_reform_data.R`)

## Phase-In

- **Duration**: 10 eligibility cohorts
- **Dimension**: Cohort (by year turning 62)
- **Mechanism**: `fact3` linearly interpolates from 0.15 to 0.05. For example, after 5 years: `fact3 = 0.15 + 0.5 × (0.05 - 0.15) = 0.10`.
- **Example**: First cohort turning 62 in 2030 (born 1968) gets `fact3 = 0.14`. Fully phased in for cohort turning 62 in 2039 (born 1977).

## Indexing

No special indexing. The replacement factor is a constant (0.05). Bend points continue to be AWI-indexed under current law.

## Formula

```
PIA = floor_dime(
  0.90 × min(AIME, bp1) +
  0.32 × max(0, min(AIME, bp2) - bp1) +
  fact3_phased × max(0, AIME - bp2)
)
```

Where `fact3_phased = 0.15 + phase_factor × (0.05 - 0.15)`.

## Expected Behavior

| Worker Type | Effect |
|-------------|--------|
| Very low | No effect — AIME below bp2 |
| Low | No effect — AIME below bp2 |
| Medium | Minimal effect — small amount of AIME above bp2 |
| High | Moderate benefit reduction — significant AIME above bp2 |
| Max | Largest benefit reduction — most AIME above bp2 |

## Code References

- **Template**: `R/reform_templates.R` — `reform_reduce_fact3()` (wraps `reform_benefit_formula()`)
- **Formula implementation**: `R/reform.R` — `reform_benefit_formula()`
- **PIA calculation**: `R/reform_functions.R:376–387` — standard 3-bracket formula with phased factors
