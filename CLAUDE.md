# ssmbar - Social Security Microsimulation Benefit Calculator

An R package that accurately calculates Social Security retirement benefits for hypothetical workers using the exact formulas, rules, and parameters defined by the Social Security Administration.

---

## Required Reading for Claude

**Before starting any work, Claude must read these companion files:**

1. `CLAUDE_GUIDELINES.md` — Behavioral rules (testing, source authority, autonomy, documentation)
2. `PROGRESS.md` — Work history, methodological decisions, validation records, session logging
3. `STATUTORY_AUDIT.md` — Title II comparison findings (if working on the audit)

These files contain binding rules and essential context. Do not proceed without reading them.

---

## Goals

1. **Benefit Calculation**: Specify any hypothetical worker configuration (e.g., "medium-earner born in 1960 with a low-earner spouse born in 1963") and calculate their annual Social Security benefits at every year of their life.

2. **Distributional Analysis**: Analyze replacement rates and benefit adequacy across worker types and birth cohorts.

3. **Policy Reform Modeling** (Future): Model hypothetical Social Security reforms and analyze their distributional impact.

---

## Technical Implementation

### The Benefit Calculation Pipeline

```
Earnings → AIME → PIA → COLA Adjustment → Actuarial Adjustment → Final Benefit
```

| Step | Function | What It Does |
|------|----------|--------------|
| 1 | `earnings_generator()` | Creates lifetime earnings using Trustees' scaled earnings factors |
| 2 | `aime()` | Computes Average Indexed Monthly Earnings (indexes to age 60, caps at taxmax, averages highest 35 years) |
| 3 | `pia()` | Computes Primary Insurance Amount using bend point formula (90/32/15) |
| 4 | `spousal_pia()` | Calculates spousal benefit (50% of spouse's PIA minus own PIA) |
| 5 | `widow_pia()` | Calculates widow(er) benefit based on deceased spouse's PIA |
| 6 | `cola()` | Applies Cost-of-Living Adjustments using CPI-W |
| 7 | `worker_benefit()` | Applies early retirement reduction factors or delayed retirement credits |
| 8 | `spouse_benefit()` | Applies actuarial adjustments to spousal benefits |
| 9 | `widow_benefit()` | Applies actuarial adjustments to widow(er) benefits |
| 10 | `final_benefit()` | Combines worker + spousal/widow(er) benefits |

### Convenience Function

`calculate_benefits()` chains all steps together for easy single-call benefit calculation.

### Present Value Functions

| Function | Purpose |
|----------|---------|
| `pv_lifetime_benefits()` | Sum of discounted annual benefits from claim_age to death_age |
| `pv_lifetime_taxes()` | Sum of discounted SS taxes from age 21-64 |
| `benefit_tax_ratio()` | Ratio of PV benefits to PV taxes |
| `couple_measures()` | Combined worker + spouse calculations with 50/50 split option |
| `real_lifetime_benefits()` | Sum of price-deflated benefits |
| `real_lifetime_earnings()` | Sum of price-deflated earnings |
| `pv_lifetime_earnings()` | Sum of discounted earnings |

### Data Structure

- `tr{YEAR}` (e.g., `tr2025`): Processed Trustees Report assumptions
- `sef{YEAR}` (e.g., `sef2025`): Scaled earnings factors
- Raw data in `inst/extdata/`, processed via `data-raw/process_data.R`

---

## Design Principles

1. **Accuracy**: Every function implements specific SSA Handbook rules (citations in code comments)
2. **Transparency**: `debugg` parameter exposes intermediate calculations
3. **Flexibility**: Supports preset worker types (very_low, low, medium, high, max) OR custom average earnings
4. **Future-proofing**: Parameters come from data, not hardcoded values
5. **Updateability**: New Trustees Reports only require data updates

---

## Statutory Basis

Core benefit calculations are governed by Title II of the Social Security Act (42 USC 401-434):

| Calculation | Primary Statutory Authority |
|-------------|----------------------------|
| AIME computation | 42 USC 415(b) |
| PIA formula | 42 USC 415(a) |
| COLA | 42 USC 415(i) |
| Old-age benefits | 42 USC 402(a) |
| Spousal benefits | 42 USC 402(b)-(c) |
| Widow(er) benefits | 42 USC 402(e)-(f) |
| Actuarial reduction | 42 USC 402(q) |
| Delayed retirement credits | 42 USC 402(w) |
| Taxable maximum | 42 USC 430 |
| Retirement age | 42 USC 416(l) |

---

## Shiny App

The Benefit Explorer app visualizes benefit calculations. Launch via `run_app()`.

See `PROGRESS.md` for file manifest and implementation details.

---

## Validation Status

Package output validated against SSA's Table V.C7 (2025 Trustees Report).
- Overall: 2.51% average difference across all worker types and birth years 1960-2000
- Remaining differences traced to cumulative rounding in AIME calculation

See `PROGRESS.md` for complete validation history and methodology.

---

## Owner

Anthony Colavito (colavito@crfb.org)  
Committee for a Responsible Federal Budget
