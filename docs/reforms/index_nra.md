# Index NRA to Life Expectancy

## Summary

Indexes the Normal Retirement Age to life expectancy improvements by increasing NRA by 1 month for every 2 eligibility cohorts, with no cap. Unlike "Raise NRA to 68," this reform continues increasing NRA indefinitely.

## Parameters Modified

| Parameter | Current Law | Reform Value | Type |
|-----------|-------------|--------------|------|
| `nra` | `67` | Function: `67 + floor(cohorts_since / 2) / 12`, no cap | replace |

## Effective Year

2026 (as configured in `generate_reform_data.R`)

## Phase-In

- **Duration**: Indefinite (no cap)
- **Dimension**: Cohort (by year turning 62)
- **Mechanism**: NRA increases by 1 month for every 2 eligibility cohorts, without cap.
- **Schedule**:
  - Cohort turning 62 in 2026 (born 1964): NRA = 67.00
  - Cohort turning 62 in 2036 (born 1974): NRA = 67 + 5/12 ≈ 67.42
  - Cohort turning 62 in 2050 (born 1988): NRA = 68.00
  - Cohort turning 62 in 2072 (born 2010): NRA = 67 + 23/12 ≈ 68.92

## Indexing

No price/wage indexing. The NRA itself is the indexed parameter, increasing as a proxy for longevity gains.

## Formula

```
cohorts_since = max(0, year - effective_year)
nra = 67 + floor(cohorts_since / 2) / 12
```

## Expected Behavior

| Worker Type | Effect |
|-------------|--------|
| All types | Equally affected — benefit reduction grows with each successive cohort |

**Key difference from NRA to 68**: For cohorts born after 1988, this reform produces NRAs higher than 68. A 2010 birth cohort would face NRA ≈ 68.92, a much larger reduction than the capped version.

## Code References

- **Template**: `R/reform_templates.R` — `reform_index_nra()`
- **NRA consumption**: `R/benefit_calculations.R` — actuarial adjustment factors use `nra` from assumptions
