# Raise Taxmax to 90% Coverage (Reform #12)

## Summary

Raises the taxable maximum to cover 90% of all earnings. New taxmax = $330,500 (2026$, AWI-indexed). Adds a fourth PIA bracket with a 5% replacement rate for AIME above the old (current-law) taxmax. Both taxes and benefits use the higher cap.

## Parameters Modified

| Parameter | Current Law | Reform Value | Type |
|-----------|-------------|--------------|------|
| `bp3` | `NA` (no 4th bracket) | Old taxmax / 12 (from `assumptions$taxmax`) | replace |
| `fact4` | `NA` | `0.05` (5%) | replace |
| `taxmax` | ~$184,500 (2026) | ~$330,500 (2026), AWI-indexed | replace |
| `taxmax_benefit` | `NA` (uses taxmax) | Same as new taxmax | replace |

## Effective Year

2026 (default; configurable via `effective_year` parameter)

## Phase-In

None (immediate).

## Indexing

- `bp3` = old (current-law) taxmax / 12, AWI-indexed via `assumptions$taxmax` schedule
- New `taxmax` = old taxmax × 1.7913 (ratio of $330,500 / $184,500), effectively AWI-indexed
- When `assumptions` is not provided, falls back to hardcoded values (2024-2040) with 3.8%/year extrapolation

## Formula

```
PIA = floor_dime(
  0.90 × min(AIME, bp1) +
  0.32 × (bp2 - bp1) +
  0.15 × (bp3 - bp2) +
  0.05 × max(0, AIME - bp3)
)
```

Where:
- `bp3 = current-law taxmax at eligibility / 12`
- AIME is computed using earnings capped at the new higher taxmax ($330,500 in 2026$)

## Expected Behavior

| Worker Type | Effect |
|-------------|--------|
| Very low through max | No effect — all current worker types earn below current-law taxmax |
| Custom high earner (e.g., $250k) | Would pay more tax, earn benefit credit at 5% above old cap |

**Note**: None of the existing worker types (max earner peaks at $173,747) are affected by this reform because the current-law taxmax ($176,100 in 2025) is already above their peak earnings. This reform becomes relevant only with high-earner custom worker types.

## Code References

- **Template**: `R/reform_templates.R` — `reform_taxmax_90_pct()`
- **4-bracket PIA**: `R/reform_functions.R:379-381` — `!is.na(bp3_elig) & !is.na(fact4_elig)` branch
- **Benefit cap**: `R/reform_functions.R:99-101` — `benefit_cap = if_else(is.na(taxmax_benefit), taxmax, taxmax_benefit)`
