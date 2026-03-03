# Cap COLAs at Median PIA

## Summary

Caps cost-of-living adjustments for beneficiaries with PIAs above the median. High-PIA beneficiaries receive the same dollar COLA increase as the median beneficiary, rather than the standard percentage increase.

## Parameters Modified

| Parameter | Current Law | Reform Value | Type |
|-----------|-------------|--------------|------|
| `cola_cap_active` | `FALSE` | `TRUE` | replace |

**Note**: This reform also relies on the `cola_cap` column in assumptions, which is pre-populated from `cola_cap_median.csv` containing projected median PIA thresholds by year.

## Effective Year

2026 (as configured in `generate_reform_data.R`)

## Phase-In

- **Duration**: Immediate (no phase-in)
- **Dimension**: Calendar year — applies to all beneficiaries starting in the effective year.

## Indexing

The `cola_cap` threshold (median PIA) is pre-computed and stored in the assumptions data. It reflects projected median PIA values by calendar year.

## Formula

```
if (worker_pia <= cola_cap_threshold) {
  # Below median: standard percentage COLA
  benefit_next_year = benefit × (1 + cola_rate)
} else {
  # Above median: dollar COLA = median_pia × cola_rate
  dollar_cola = cola_cap_threshold × cola_rate
  benefit_next_year = benefit + dollar_cola
}
```

## Expected Behavior

| Worker Type | Effect |
|-------------|--------|
| Very low | No effect — PIA below median |
| Low | No effect — PIA below median |
| Medium | May be affected depending on cohort (PIA near median) |
| High | COLA capped — receives smaller percentage increase |
| Max | Most affected — largest gap between capped and uncapped COLA |

**Key dynamics**:
- Progressive reform: redistributes COLA from high earners to preserve low-earner COLAs
- Like chained CPI, the effect compounds over time
- Unlike chained CPI, very low and low earners are completely unaffected

## Code References

- **Template**: `R/reform_templates.R` — `reform_cola_cap()`
- **COLA capping logic**: `R/reform_functions.R:458` — `cola_reform()` checks `cola_cap_active` flag and applies capping
- **Median data**: `inst/extdata/cola_cap_median.csv` — pre-computed median PIA thresholds
