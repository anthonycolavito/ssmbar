# Index COLAs to CPI-E (Elderly Index)

## Summary

Switches the COLA index from CPI-W to CPI-E (Consumer Price Index for the Elderly), which grows approximately 0.2 percentage points faster per year due to higher health care spending weights for seniors. This **increases** benefits.

## Parameters Modified

| Parameter | Current Law | Reform Value | Type |
|-----------|-------------|--------------|------|
| `cola` | Trustees Report projection (varies ~2.4%/yr) | `+0.2` percentage points | add |

## Effective Year

2026 (as configured in `generate_reform_data.R`)

## Phase-In

- **Duration**: Immediate (no phase-in)
- **Dimension**: Calendar year — applies to all beneficiaries receiving COLAs starting in the effective year.

## Indexing

The COLA adjustment is a constant offset (+0.2 pp) applied to the Trustees Report COLA projection each year.

## Formula

```
cola_reform(year) = cola_current_law(year) + 0.2
```

For example, if current-law COLA is 2.4% in 2030, reform COLA is 2.6%.

## Expected Behavior

| Worker Type | Effect |
|-------------|--------|
| All types | Equally affected in percentage terms — benefits grow faster |

**Key dynamics**:
- Benefit-increasing reform (unlike chained CPI which is benefit-reducing)
- Effect compounds: each year post-eligibility, benefits are 0.2% higher than current law
- After 20 years of benefit receipt, cumulative benefit is ~4% higher than current law
- Increases PV of lifetime benefits and benefit-tax ratios for all worker types
- Higher earners receive larger dollar increases (same percentage, larger base)

## Code References

- **Template**: `R/reform_templates.R` — `reform_cpi_e()`
- **COLA application**: `R/reform_functions.R:458` — `cola_reform()` uses the modified COLA rate from assumptions
