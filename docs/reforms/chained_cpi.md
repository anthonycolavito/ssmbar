# Index COLAs to Chained CPI

## Summary

Switches the COLA index from CPI-W to Chained CPI-U (C-CPI-U), which grows approximately 0.3 percentage points slower per year. Benefits grow more slowly after initial entitlement.

## Parameters Modified

| Parameter | Current Law | Reform Value | Type |
|-----------|-------------|--------------|------|
| `cola` | Trustees Report projection (varies ~2.4%/yr) | `-0.3` percentage points | add |

## Effective Year

2026 (as configured in `generate_reform_data.R`)

## Phase-In

- **Duration**: Immediate (no phase-in)
- **Dimension**: Calendar year — applies to all beneficiaries receiving COLAs starting in the effective year, regardless of when they became eligible.

## Indexing

The COLA adjustment is a constant offset (-0.3 pp) applied to the Trustees Report COLA projection each year. The underlying COLA projection remains CPI-based.

## Formula

```
cola_reform(year) = cola_current_law(year) - 0.3
```

For example, if current-law COLA is 2.4% in 2030, reform COLA is 2.1%.

## Expected Behavior

| Worker Type | Effect |
|-------------|--------|
| All types | Equally affected in percentage terms |

**Key dynamics**:
- No effect at eligibility year (PIA is not affected by COLA changes)
- Effect compounds over time: each year post-eligibility, benefits are 0.3% lower than they would have been
- Older cohorts (who die sooner) are less affected because the compounding period is shorter
- Later cohorts (born 2000, 2010) see larger cumulative reductions by the end of their benefit period
- Lifetime PV of benefits decreases for all worker types

**Example**: After 20 years of benefit receipt, cumulative benefit is ~6% lower than current law.

## Code References

- **Template**: `R/reform_templates.R` — `reform_chained_cpi()`
- **COLA application**: `R/reform_functions.R:458` — `cola_reform()` uses the modified COLA rate from assumptions
