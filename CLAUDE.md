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

## Commit Workflow (Non-Negotiable)

Every commit must follow this exact sequence:

1. **Update `PROGRESS.md`** with a session log entry describing what changed — this goes in the same commit, not a separate one
2. **Run full test suite** (`devtools::test()`) immediately before committing — a test run from earlier in the session does not count
3. **Stage only files you actually changed** — do not use `git add -A` or `git add .`
4. **Write a commit message** that explains what changed and why

Failure to follow this sequence (e.g., committing without PROGRESS.md, relying on a stale test run, pushing and then updating PROGRESS.md as a second commit) violates project rules.

---

## Goals

1. **Benefit Calculation**: Specify any hypothetical worker configuration (e.g., "medium-earner born in 1960 with a low-earner spouse born in 1963") and calculate their annual Social Security benefits at every year of their life.

2. **Distributional Analysis**: Analyze replacement rates and benefit adequacy across worker types and birth cohorts.

3. **Policy Reform Modeling**: Model hypothetical Social Security reforms (~20 reform templates) and analyze their distributional impact using `calculate_benefits_reform()`.

---

## Technical Implementation

### The Benefit Calculation Pipelines

The package provides two parallel pipelines:

**Baseline Pipeline** (current law, no reform parameters):
```
Earnings → aime() → pia() → cola() → worker_benefit() → spousal_pia()
→ spouse_benefit() → child_pia() → child_benefit() → family_maximum()
→ widow_pia() → widow_benefit() → ret() → final_benefit()
```

**Reform Pipeline** (supports policy reform modeling):
```
Earnings → aime_reform() → pia_reform() → cola_reform() → worker_benefit()
→ basic_minimum_benefit() → spousal_pia() → spouse_benefit() → child_pia()
→ child_benefit() → family_maximum() → widow_pia() → widow_benefit_reform()
→ ret_reform() → final_benefit()
```

| Function | Baseline | Reform-Capable | What It Does |
|----------|----------|----------------|--------------|
| `earnings_generator()` | Shared | Shared | Creates lifetime earnings using Trustees' scaled earnings factors |
| `aime()` / `aime_reform()` | ✓ | ✓ | Computes AIME. Reform version supports taxmax split (#14), child care credit (#29) |
| `pia()` / `pia_reform()` | ✓ | ✓ | Computes PIA using 90/32/15. Reform supports multipliers, flat benefit, 4th bracket |
| `cola()` / `cola_reform()` | ✓ | ✓ | Applies COLAs. Reform version supports COLA cap (#9) |
| `worker_benefit()` | Shared | Shared | Applies actuarial adjustments (no reform logic) |
| `spousal_pia()` | Shared | Shared | Calculates spousal benefit (50% of spouse's PIA minus own PIA) |
| `widow_pia()` | Shared | Shared | Calculates widow(er) benefit based on deceased spouse's PIA |
| `widow_benefit()` / `widow_benefit_reform()` | ✓ | ✓ | Reform version supports 75% combined benefit (#28) |
| `ret()` / `ret_reform()` | ✓ | ✓ | Retirement Earnings Test. Reform version supports RET repeal (#23) |
| `final_benefit()` | Shared | Shared | Combines worker + spousal/widow(er) benefits |

### Convenience Functions

- `calculate_benefits()` — Chains **baseline** functions for current-law benefit calculation
- `calculate_benefits_reform()` — Chains **reform-capable** functions for policy analysis

### Code Organization

| File | Contents |
|------|----------|
| `R/baseline_benefit_calculations.R` | Baseline `aime()`, `pia()`, `cola()` |
| `R/baseline_ret.R` | Baseline `ret()` |
| `R/baseline_survivor.R` | Baseline `widow_benefit()` |
| `R/benefit_calculations.R` | Reform-capable `aime_reform()`, `pia_reform()`, `cola_reform()` + shared functions |
| `R/ret.R` | Reform-capable `ret_reform()` |
| `R/survivor.R` | Reform-capable `widow_benefit_reform()` + shared `widow_pia()` |

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
- Overall: 0.17% average difference across all worker types and birth years 1960-1970
- Remaining differences traced to earnings generation precision (scaled_factor × AWI rounding)

See `PROGRESS.md` for complete validation history and methodology.

---

## Development Environment

**R Installation Path:**
```
C:\Users\AnthonyColavito\AppData\Local\Programs\R\R-4.5.0\bin\Rscript.exe
```

To run tests from command line:
```powershell
& 'C:\Users\AnthonyColavito\AppData\Local\Programs\R\R-4.5.0\bin\Rscript.exe' -e "devtools::test()"
```

---

## Owner

Anthony Colavito (colavito@crfb.org)
Committee for a Responsible Federal Budget
