# Raise NRA from 67 to 68

## Summary

Gradually raises the Normal Retirement Age from 67 to 68, increasing the actuarial reduction for early claimers and reducing benefits at any given claim age.

## Parameters Modified

| Parameter | Current Law | Reform Value | Type |
|-----------|-------------|--------------|------|
| `nra` | `67` | Function: `67 + floor(cohorts_since / 2) / 12`, capped at `68` | replace |

## Effective Year

2026 (as configured in `generate_reform_data.R`)

## Phase-In

- **Duration**: 24 eligibility cohorts (from 67 to 68 = 12 months × 2 cohorts per month)
- **Dimension**: Cohort (by year of eligibility, i.e., year turning 62)
- **Mechanism**: NRA increases by 1 month for every 2 eligibility cohorts. The function handles its own phase-in (`phase_in_years = 0`).
- **Schedule**:
  - Cohort turning 62 in 2026 (born 1964): NRA = 67 + 0/12 = 67.00
  - Cohort turning 62 in 2028 (born 1966): NRA = 67 + 1/12 ≈ 67.08
  - Cohort turning 62 in 2038 (born 1976): NRA = 67 + 6/12 = 67.50
  - Cohort turning 62 in 2049 (born 1987): NRA = 67 + 11/12 ≈ 67.92
  - Cohort turning 62 in 2050+ (born 1988+): NRA = 68.00 (capped)

## Indexing

No indexing. NRA is computed as a function of the eligibility year relative to the effective year.

## Formula

```
cohorts_since = max(0, year - effective_year)
nra = min(67 + floor(cohorts_since / 2) / 12, 68)
```

## Expected Behavior

| Worker Type | Effect |
|-------------|--------|
| All types | Equally affected — NRA change applies the same actuarial reduction regardless of earnings level |

**Mechanism**: Higher NRA means more months of actuarial reduction for workers claiming before NRA. At claim age 65:
- Current law (NRA 67): 24 months early → ~13.3% reduction
- Reform (NRA 68): 36 months early → ~20% reduction

Benefits at NRA are unchanged (full PIA), but fewer workers reach NRA by typical claim ages.

## Code References

- **Template**: `R/reform_templates.R` — `reform_nra_to_68()`
- **NRA consumption**: `R/benefit_calculations.R` — actuarial adjustment factors use `nra` from assumptions
