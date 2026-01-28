# ssmbar - Project Context for Claude

## Package Purpose

**ssmbar** (Social Security Microsimulation Benefit Calculator) is an R package that accurately calculates Social Security retirement benefits for hypothetical workers using the exact formulas, rules, and parameters defined by the Social Security Administration.

## Goals (Priority Order)

1. **Benefit Calculation**: Specify any hypothetical worker configuration (e.g., "medium-earner born in 1960 with a low-earner spouse born in 1963") and calculate their annual Social Security benefits at every year of their life based on their earnings profile.

2. **Distributional Analysis**: Use calculated benefits to answer questions like "What would the replacement rate be for a worker with $50,000 (2025 dollars) in average earnings if they were born in 1960 versus 1980?"

3. **Policy Reform Modeling** (Future): Model hypothetical Social Security reforms and analyze their distributional impact across different worker types.

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
| 5 | `cola()` | Applies Cost-of-Living Adjustments using CPI-W |
| 6 | `worker_benefit()` | Applies early retirement reduction factors or delayed retirement credits |
| 7 | `spouse_benefit()` | Applies actuarial adjustments to spousal benefits |
| 8 | `final_benefit()` | Combines worker + spousal benefits |

### Convenience Function

`calculate_benefits()` chains all steps together for easy single-call benefit calculation.

### Data Structure

- `tr{YEAR}` (e.g., `tr2025`): Processed Trustees Report assumptions
- `sef{YEAR}` (e.g., `sef2025`): Scaled earnings factors
- Raw data in `inst/extdata/`, processed via `data-raw/process_data.R`

## Design Principles

1. **Accuracy**: Every function implements specific SSA Handbook rules (citations in code comments)
2. **Transparency**: `debugg` parameter exposes intermediate calculations
3. **Flexibility**: Supports preset worker types (very_low, low, medium, high, max) OR custom average earnings
4. **Future-proofing**: Parameters come from data, not hardcoded values
5. **Updateability**: New Trustees Reports only require data updates

## Working Guidelines

- **Always commit changes** with meaningful, descriptive messages explaining what and why
- **Never delete files** without explicit user permission
- **Ask permission** if uncertain about any action
- All development should support the goals above

## Known Issues

- `worker_builder.R`: Contains incomplete/broken code (references undefined `dataset` variable) - this file needs work or removal

---

## Validation Against Trustees Report Table V.C7 (January 2026)

### Overview

Comprehensive validation was performed comparing ssmbar calculations against SSA's Table V.C7 from the 2025 Trustees Report. V.C7 shows "Annual Scheduled Benefit Amounts" for hypothetical workers.

### Key Discovery: V.C7 Uses 2025 CPI-W Dollars

**Critical insight**: V.C7 shows all benefits in **constant 2025 CPI-W adjusted dollars**, not nominal dollars. This means:
- For birth year 1960 (turning 65 in 2025), minimal adjustment needed
- For future birth years, nominal benefits must be deflated: `benefit_2025 = nominal × (CPI_2025 / CPI_benefit_year)`

### V.C7 Table Structure

V.C7 is organized by **year turning 65**, not birth year:
- Column 1: Year turning 65
- Column 2: NRA (e.g., "67:0")
- Column 3: Full PIA at NRA (in 2025 dollars)
- Column 6: Reduced benefit at age 65 (in 2025 dollars)

### Validation Results (With Deflation)

| Worker Type | Avg % Diff | Min % | Max % |
|-------------|-----------|-------|-------|
| medium      | 0.73%     | -0.16%| 1.04% |
| low         | 2.26%     | 0.77% | 2.77% |
| max         | 2.66%     | 1.12% | 3.18% |
| high        | 3.42%     | 1.52% | 4.18% |
| very_low    | 3.47%     | 1.55% | 4.21% |

**Overall: 2.51% average difference across all worker types and birth years 1960-2000**

### Source of Remaining Differences

For the medium earner born 1960 (most detailed analysis):
- ssmbar: $25,368 annual at age 65
- V.C7: $25,172 (difference: $196 or 0.78%)

Root cause traced to AIME calculation:
- ssmbar AIME: 4,618
- V.C7 implied AIME: ~4,557
- Difference: 61/month (1.3%)

This stems from cumulative small rounding differences in `scaled_factor × AWI` calculations across 44 working years. Total nominal earnings differ by only $575 (0.04%), but rounding compounds through the calculation pipeline.

### Other Technical Findings

1. **Actuarial reduction factors are correct**: 5/9 of 1% per month for first 36 months = 0.8667 for 24 months early

2. **Scaled earnings factors match SSA**: Verified against Actuarial Note 2025.3 Table 6

3. **January 1 vs January 2 birth dates**: SSA hypotheticals use January 2 birthdays. Our indexing year (age 60) matches SSA methodology.

4. **COLA timing**: Year-by-year COLA application with flooring at each step is implemented correctly

### Validation Scripts

Located in `scripts/`:
- `validate_all_workers.R` - Basic validation (nominal dollars)
- `validate_all_workers_deflated.R` - Proper validation with CPI-W deflation
- `trace_medium_earner.R` - Detailed calculation trace
- `compare_table4_earnings.R` - Earnings comparison with Actuarial Note

---

---

## Benefit Explorer Shiny App (January 2026)

### Overview

A Shiny visualization app for exploring Social Security benefit calculations, launched via `run_app()`.

### Files Created

| File | Purpose |
|------|---------|
| `R/pv_functions.R` | Present value calculation functions |
| `R/run_app.R` | App launcher function |
| `inst/shiny/benefit_explorer/app.R` | Main app (ui + server) |
| `inst/shiny/benefit_explorer/global.R` | Load packages and data |
| `inst/shiny/benefit_explorer/modules/mod_worker_input.R` | Worker selection sidebar |
| `inst/shiny/benefit_explorer/modules/mod_benefits.R` | Benefits over time charts |
| `inst/shiny/benefit_explorer/modules/mod_replacement.R` | Replacement rate comparisons |
| `inst/shiny/benefit_explorer/modules/mod_lifetime.R` | PV benefits/taxes display |
| `inst/shiny/benefit_explorer/modules/mod_ratios.R` | Benefit-tax ratios |

### PV Functions

1. **`pv_lifetime_benefits()`** - Sum of discounted annual benefits from claim_age to death_age
2. **`pv_lifetime_taxes()`** - Sum of discounted SS taxes from age 21-64
3. **`benefit_tax_ratio()`** - Ratio of PV benefits to PV taxes
4. **`couple_measures()`** - Combined worker + spouse calculations with 50/50 split option
5. **`real_lifetime_benefits()`** - Sum of price-deflated benefits
6. **`real_lifetime_earnings()`** - Sum of price-deflated earnings
7. **`pv_lifetime_earnings()`** - Sum of discounted earnings

### Critical Implementation Rules

1. **Nominal discount factor for nominal cash flows**: PV calculations use `df` (nominal effective discount factor), not `real_df`, because benefits and taxes are in nominal dollars.

2. **Discount factor lookup before filtering**: The `df_norm` lookup must happen BEFORE filtering rows, otherwise the discount year (e.g., age 65) may be excluded:
   ```r
   # CORRECT: Lookup df_norm in mutate before filter
   mutate(df_norm = df[which(year == discount_year)][1]) %>%
   filter(age >= claim_age & age < death_age)

   # WRONG: Filtering first loses access to age 65 row
   filter(age >= claim_age) %>%
   mutate(df_norm = ...)  # Age 65 row already gone!
   ```

3. **Death age exclusion**: Use `age < death_age` (not `<=`) to exclude benefits in the year of death.

4. **Real deflation normalization**: When deflating to real dollars, normalize to a consistent age (default 65) using `gdp_pi[age_65] / gdp_pi[year]`.

5. **rep_rates() uses real_df**: The replacement rate PV annuity calculation uses `real_df` because it's computing a real annuity equivalent.

### Test Results

All 382 tests pass:
- 256 regression tests
- 67 reform tests
- 46 PV function tests
- 13 actuarial tests

---

## Owner

Anthony Colavito (colavito@crfb.org) - Committee for a Responsible Federal Budget
