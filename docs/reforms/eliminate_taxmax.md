# Eliminate Taxmax with 15% Credit (Reform #13)

## Summary

Removes the taxable maximum entirely by setting taxmax to $10,000,000. Adds a fourth PIA bracket with a 15% replacement rate for AIME above the old (current-law) taxmax. Workers pay Social Security taxes on all earnings and earn benefit credit at 15% for earnings above the old cap.

## Parameters Modified

| Parameter | Current Law | Reform Value | Type |
|-----------|-------------|--------------|------|
| `bp3` | `NA` (no 4th bracket) | Old taxmax / 12 (from `assumptions$taxmax`) | replace |
| `fact4` | `NA` | `0.15` (15%) | replace |
| `taxmax` | ~$184,500 (2026) | `10,000,000` (effectively unlimited) | replace |

## Effective Year

2026 (default; configurable via `effective_year` parameter)

## Phase-In

None (immediate).

## Indexing

- `bp3` = old (current-law) taxmax / 12, AWI-indexed via `assumptions$taxmax` schedule
- When `assumptions` is not provided, bp3 returns `NA_real_` and `pia_reform()` uses the fallback: `taxmax_benefit / 12`
- `taxmax` = $10M (constant scalar, no indexing needed — effectively unlimited)

## Formula

```
PIA = floor_dime(
  0.90 × min(AIME, bp1) +
  0.32 × (bp2 - bp1) +
  0.15 × (bp3 - bp2) +
  0.15 × max(0, AIME - bp3)
)
```

Where:
- `bp3 = current-law taxmax at eligibility / 12`
- AIME is computed using all earnings (taxmax = $10M)

## Expected Behavior

| Worker Type | Effect |
|-------------|--------|
| Very low through max | No effect — all current worker types earn below current-law taxmax |
| Custom high earner (e.g., $250k) | Would pay more tax, earn benefit credit at 15% above old cap |

**Note**: None of the existing worker types are affected. The max earner peaks at $173,747, below the 2025 taxmax of $176,100.

## Code References

- **Template**: `R/reform_templates.R` — `reform_eliminate_taxmax()`
- **bp3 fallback**: `R/reform_functions.R:359-361` — when bp3 is NA, uses `taxmax_benefit / 12`
- **4-bracket PIA**: `R/reform_functions.R:379-381`
