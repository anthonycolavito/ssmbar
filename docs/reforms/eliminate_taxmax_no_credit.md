# Eliminate Taxmax without Benefit Credit (Reform #14)

## Summary

Removes the taxable maximum for tax purposes only. Workers pay the full 12.4% Social Security tax on all earnings, but benefits are still calculated using earnings capped at the current-law taxmax. This is a pure revenue measure — no marginal benefit return above the old cap.

## Parameters Modified

| Parameter | Current Law | Reform Value | Type |
|-----------|-------------|--------------|------|
| `taxmax_tax` | Same as `taxmax` | `10,000,000` (effectively unlimited) | replace |
| `taxmax_benefit` | `NA` (uses taxmax) | `NA_real_` (keeps current-law taxmax for AIME) | replace |

## Effective Year

2026 (default; configurable via `effective_year` parameter)

## Phase-In

None (immediate).

## Indexing

- `taxmax_tax` = $10M (constant scalar — effectively unlimited, no indexing needed)
- `taxmax_benefit` = `NA_real_` → `aime_reform()` falls back to `taxmax`, which remains at current law (AWI-indexed)

## How It Works

The reform uses the split taxmax architecture:
- **`taxmax_tax`**: Used by `calculate_taxes()` to cap earnings for payroll tax calculation. Set to $10M = effectively no cap.
- **`taxmax_benefit`**: Used by `aime_reform()` to cap earnings for AIME calculation. Set to `NA` = falls back to current-law `taxmax`.

This means:
1. `calculate_taxes()` applies the 12.4% rate (6.2% OASI + 1.8% DI, employee share) to all earnings up to $10M
2. `aime_reform()` still caps indexed earnings at the current-law taxmax when computing AIME
3. No fourth PIA bracket is added (no `bp3` or `fact4`)

## Expected Behavior

| Worker Type | Effect |
|-------------|--------|
| Very low through max | No effect — all current worker types earn below current-law taxmax |
| Custom high earner (e.g., $250k) | Would pay more tax but receive identical benefits to current law |

**Note**: None of the existing worker types are affected. The max earner peaks at $173,747, below the 2025 taxmax of $176,100.

## Code References

- **Template**: `R/reform_templates.R` — `reform_eliminate_taxmax_no_credit()`
- **taxmax_tax in taxes**: `R/analytic_functions.R` — `calculate_taxes()` uses `taxmax_tax` when present
- **taxmax_benefit in AIME**: `R/reform_functions.R:99-101` — `benefit_cap = if_else(is.na(taxmax_benefit), taxmax, taxmax_benefit)`
- **taxmax_tax initialization**: `R/assumptions_prep.R:469-473` — default initialization of split taxmax columns
