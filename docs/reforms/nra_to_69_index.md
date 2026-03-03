# Raise NRA to 69, Then Index to Life Expectancy

## Summary

A two-phase reform: first rapidly raises NRA from 67 to 69 (+2 months per eligibility cohort), then indexes to longevity (+1 month per 2 cohorts). The most aggressive NRA reform.

## Parameters Modified

| Parameter | Current Law | Reform Value | Type |
|-----------|-------------|--------------|------|
| `nra` | `67` | Two-phase function (see below) | replace |

## Effective Year

2026 (as configured in `generate_reform_data.R`)

## Phase-In

- **Phase 1**: 12 cohorts to reach NRA 69 (+2 months/year)
- **Phase 2**: Indefinite indexing after NRA 69 (+1 month per 2 cohorts)
- **Dimension**: Cohort (by year turning 62)
- **Schedule**:
  - Cohort turning 62 in 2026 (born 1964): NRA = 67.00
  - Cohort turning 62 in 2032 (born 1970): NRA = 68.00
  - Cohort turning 62 in 2038 (born 1976): NRA = 69.00 (Phase 1 complete)
  - Cohort turning 62 in 2050 (born 1988): NRA = 69 + 6/12 = 69.50
  - Cohort turning 62 in 2072 (born 2010): NRA = 69 + 17/12 ≈ 70.42

## Indexing

No price/wage indexing. NRA itself increases as a function of cohort.

## Formula

```
cohorts_since = max(0, year - effective_year)
if (cohorts_since < 12) {
  # Phase 1: +2 months per cohort year
  nra = 67 + cohorts_since * 2 / 12
} else {
  # Phase 2: +1 month per 2 cohort years after reaching 69
  nra = 69 + floor((cohorts_since - 12) / 2) / 12
}
```

## Expected Behavior

| Worker Type | Effect |
|-------------|--------|
| All types | Largest benefit reduction among NRA reforms; equally affects all earner levels |

**Comparison across NRA reforms** (cohort born 2010, claim age 65):
- NRA to 68: NRA = 68, 36 months early → ~20% reduction
- Index NRA: NRA ≈ 68.92, ~47 months early → ~26% reduction
- NRA to 69 + index: NRA ≈ 70.42, ~65 months early → ~36% reduction

## Code References

- **Template**: `R/reform_templates.R` — `reform_nra_to_69_index()`
- **NRA consumption**: `R/benefit_calculations.R` — actuarial adjustment factors use `nra` from assumptions
