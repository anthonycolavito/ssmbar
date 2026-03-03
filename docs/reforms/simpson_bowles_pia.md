# Simpson-Bowles 4-Bracket PIA (Top Half)

## Summary

Adds a fourth PIA bracket at AWI with a 5% replacement factor, and reduces the second and third factors. Creates a 90/30/10/5 bracket structure (vs. current-law 90/32/15).

## Parameters Modified

| Parameter | Current Law | Reform Value | Type |
|-----------|-------------|--------------|------|
| `bp3` | `NA` (no 4th bracket) | AWI (via `taxmax_benefit / 12` fallback) | replace |
| `fact2` | `0.32` (32%) | `0.30` (30%) | replace |
| `fact3` | `0.15` (15%) | `0.10` (10%) | replace |
| `fact4` | `NA` | `0.05` (5%) | replace |

## Effective Year

2030 (as configured in `generate_reform_data.R`)

## Phase-In

- **Duration**: 10 eligibility cohorts
- **Dimension**: Cohort (by year turning 62)
- **Mechanism**: All factors linearly interpolate toward targets. `bp3` is set via function returning `NA_real_` — consumed via `taxmax_benefit / 12` fallback in `reform_functions.R:359-361`.

## Indexing

- `bp3` is effectively the taxable maximum (AWI-indexed under current law), divided by 12 for monthly AIME comparison.
- Replacement factors are constants (no indexing needed).

## Formula

```
PIA = floor_dime(
  0.90 × min(AIME, bp1) +
  fact2_phased × (bp2 - bp1) +
  fact3_phased × (bp3 - bp2) +
  fact4_phased × max(0, AIME - bp3)
)
```

Where:
- `bp3 = taxmax_benefit_at_elig / 12` (fallback when bp3 column is NA)
- `fact2_phased` interpolates 0.32 → 0.30
- `fact3_phased` interpolates 0.15 → 0.10
- `fact4_phased` interpolates 0.00 → 0.05

## Expected Behavior

| Worker Type | Effect |
|-------------|--------|
| Very low | No effect — AIME well below bp2 |
| Low | No or minimal effect — AIME near or below bp2 |
| Medium | Small reduction from fact2/fact3 decreases |
| High | Moderate reduction — hit by both factor cuts and 4th bracket |
| Max | Largest reduction — most AIME in higher brackets |

**Note**: The 4th bracket with a 5% factor means earnings above taxmax get a small replacement rate (5% vs. zero under current law), but this only applies to max earners whose AIME already exceeds bp3.

## Code References

- **Template**: `R/reform_templates.R` — `reform_simpson_bowles()`
- **bp3 fallback**: `R/reform_functions.R:359-361` — when `bp3` is NA, uses `taxmax_benefit / 12`
- **4-bracket PIA**: `R/reform_functions.R:379-381` — `!is.na(bp3_elig) & !is.na(fact4_elig)` branch
