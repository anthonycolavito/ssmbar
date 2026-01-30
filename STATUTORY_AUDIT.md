# Statutory Audit: ssmbar vs. Title II

This document tracks the comparison of ssmbar's benefit calculation functions against the statutory requirements in Title II of the Social Security Act (42 USC 401-434).

**Audit Scope**: Statute only (Option A). Regulations and POMS are consulted only to resolve ambiguity, and such consultations are documented in PROGRESS.md.

**Audit Rule**: Discrepancies are documented here for discussion. Claude does NOT modify code based on audit findings without explicit user approval.

---

## Audit Status

| Section | Statutory Reference | Package Function(s) | Status |
|---------|--------------------|--------------------|--------|
| AIME Computation | 42 USC 415(b) | `aime()` | **Reviewed** ✓ |
| PIA Formula | 42 USC 415(a) | `pia()` | **Reviewed** ✓ |
| COLA | 42 USC 415(i) | `cola()` | **Reviewed** ✓ |
| Old-Age Benefits | 42 USC 402(a) | `worker_benefit()` | **Reviewed** ✓ |
| Wife's Benefits | 42 USC 402(b) | `spousal_pia()`, `spouse_benefit()` | **Reviewed** ✓ |
| Husband's Benefits | 42 USC 402(c) | `spousal_pia()`, `spouse_benefit()` | **Reviewed** ✓ |
| Child's Benefits | 42 USC 402(d) | **Not yet implemented** | **Documented** ✓ |
| Widow's Benefits | 42 USC 402(e) | `widow_pia()`, `widow_benefit()` | **Reviewed** ✓ |
| Widower's Benefits | 42 USC 402(f) | `widow_pia()`, `widow_benefit()` | **Reviewed** ✓ |
| Family Maximum | 42 USC 403(a) | **Not yet implemented** | **Documented** ✓ |
| Actuarial Reduction | 42 USC 402(q) | `worker_benefit()`, `spouse_benefit()`, `widow_benefit()` | **Reviewed** ✓ |
| Delayed Retirement Credits | 42 USC 402(w) | `worker_benefit()` | **Reviewed** ✓ |
| Contribution/Benefit Base | 42 USC 430 | `tr2025$taxmax` | **Reviewed** ✓ |
| Retirement Age | 42 USC 416(l) | Various | **Reviewed** ✓ |
| AWI Definition | 42 USC 409(k) | `tr2025$awi` | **Reviewed** ✓ |
| Bend Points | 42 USC 415(a)(1)(A) | `tr2025$bp1`, `tr2025$bp2` | **Reviewed** ✓ |
| Insured Status | 42 USC 414 | `qc_comp()`, `aime()` | **Reviewed** ✓ |
| Earnings Record | 42 USC 415(b)(2) | `comp_period()`, `aime()` | **Reviewed** ✓ |

---

## Out of Scope (Intentionally Not Implemented)

The following benefit types are defined in Title II but are **intentionally not implemented** in ssmbar. These should not be flagged as discrepancies during the audit.

| Benefit Type | Statutory Reference | Reason Not Implemented |
|--------------|--------------------|-----------------------|
| Mother's/Father's Benefits | 42 USC 402(g) | Dependent on child benefits; not core to retirement analysis |
| Parent's Benefits | 42 USC 402(h) | Rare benefit type, not core to retirement analysis |
| Lump-Sum Death Payment | 42 USC 402(i) | Not a recurring benefit |

---

## Not Yet Implemented (Future Work)

The following benefit rules are **within ssmbar's scope** but have not yet been implemented. The audit should document statutory requirements for these to inform future development.

| Benefit Type | Statutory Reference | Priority | Notes |
|--------------|--------------------|---------| ------|
| Child's Benefits | 42 USC 402(d) | TBD | Needed for complete family benefit modeling |
| Family Maximum | 42 USC 403(a) | TBD | Caps total family benefits; affects couples with children |

---

## Audit Findings

*Discrepancies, questions, and observations organized by statutory section.*

---

### 42 USC 415(b) — AIME Computation

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 415(b)](https://www.law.cornell.edu/uscode/text/42/415)

**Package Implementation**: `aime()` in `R/benefit_calculations.R`, `comp_period()` and `qc_comp()` in `R/eligibility.R`

**Statutory Text Summary**:

1. **(b)(1) AIME Formula**: "An individual's average indexed monthly earnings shall be equal to the quotient obtained by dividing—(A) the total (after adjustment under paragraph (3)) of his wages paid in and self-employment income credited to his benefit computation years (determined under paragraph (2)), by (B) the number of months in those years."

2. **(b)(2)(A) Dropout Years**:
   - Old-age/death: **5 years**
   - Disability: "one-fifth of such individual's elapsed years (disregarding any resulting fractional part of a year), but not by more than 5 years"
   - Minimum computation period: **2 years**

3. **(b)(2)(B) Elapsed Years**: "the number of calendar years after 1950 (or, if later, the year in which the individual attained age 21) and before the year in which the individual died, or, if it occurred earlier (but after 1960), the year in which he attained age 62"

4. **(b)(3) Indexing**: Wages indexed by ratio of "the national average wage index...for the **second calendar year preceding** the earliest of the year of the individual's death, eligibility for an old-age insurance benefit, or eligibility for a disability insurance benefit...by...the national average wage index...for the computation base year"

5. **(e)(2) Rounding**: "if an individual's average indexed monthly earnings...is not a multiple of $1, it shall be reduced to the next lower multiple of $1."

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| AIME formula | Total indexed earnings ÷ months in computation years | `floor(top_earnings_sum / (comp_period[i] * 12))` | ✓ | Line 208 of benefit_calculations.R |
| Indexing year | 2nd year before eligibility (age 60 for age-62 eligibility) | `index_age = elig_age - index_age_offset` where offset=2 | ✓ | Lines 169-171; `index_age_offset=2` in assumptions |
| Index factor | AWI(index_year) / AWI(base_year); earnings past index year at face value | `index_factor = pmax(awi_index_age / awi, 1)` | ✓ | Line 171; pmax ensures post-index earnings aren't reduced |
| Elapsed years (retirement) | Years after age 21, before year of age 62 = 40 years | `elapsed_years = pmax(elig_age - 1 - 21, 0)` = 40 for elig_age=62 | ✓ | eligibility.R line 71 |
| Dropout years (retirement) | 5 years | `pmin(max_dropout_years, ...)` where max_dropout_years=5 | ✓ | eligibility.R line 72; always yields 5 for retirement |
| Dropout years (disability) | 1/5 of elapsed years, max 5 | Formula uses `floor(elig_age/5)` not `floor(elapsed_years/5)` | ⚠️ | See discrepancy #1 below |
| Min computation period | 2 years | `pmax(min_comp_period, ...)` where min_comp_period=2 | ✓ | eligibility.R line 73 |
| Years averaged | Highest earning years equal to computation period | Partial sort to select top N years | ✓ | Lines 199-207 |
| Earnings cap | Capped at contribution/benefit base (taxmax) | `capped_earn = pmin(earnings, taxmax)` | ✓ | Line 172 |
| AIME rounding | Floor to next lower $1 | `floor()` | ✓ | Line 208 |

**Discrepancies Found**:

~~1. **Disability dropout years formula** (Low severity for current scope)~~ **RESOLVED**
   - Fixed in commit 9d867f5 (January 29, 2026)
   - Formula now correctly uses `floor(elapsed_years / 5)` for disability per statute

**Questions for Discussion**:

*None — all issues resolved*

---

### 42 USC 415(a) — PIA Formula

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 415(a)](https://www.law.cornell.edu/uscode/text/42/415)

**Package Implementation**: `pia()` in `R/benefit_calculations.R`, bend point projection in `R/assumptions_prep.R`

**Statutory Text Summary**:

1. **(a)(1)(A) PIA Formula**: Three-tiered formula:
   - 90% of AIME up to first bend point
   - 32% of AIME between first and second bend points
   - 15% of AIME above second bend point

2. **(a)(1)(B) 1979 Bend Points**: First bend point = $180; Second bend point = $1,085

3. **(a)(1)(B)(ii) Bend Point Indexing**: Multiply base amounts by ratio of AWI for year individual attains age 62 divided by AWI for 1977

4. **(a)(2)(C) PIA Rounding**: "if not a multiple of $0.10, to the next lower multiple of $0.10"

5. **(a)(1)(C)(i) Minimum PIA**: "$11.50 multiplied by the individual's years of coverage in excess of 10" (special minimum PIA)

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| 90/32/15 formula | 90% up to bp1, 32% bp1-bp2, 15% above bp2 | `(fact1 * bp1) + (fact2 * (bp2-bp1)) + (fact3 * (aime-bp2))` | ✓ | Lines 280-282; fact1=0.90, fact2=0.32, fact3=0.15 |
| 1979 base bend points | $180 / $1,085 | Loaded from historical data in assumptions | ✓ | See assumptions_prep.R lines 39-40 |
| Bend point indexing | bp × (AWI_elig / AWI_1977) | `bp1_i <- round(bp1_base * awi_end / awi_1977)` | ✓ | assumptions_prep.R lines 72-73; awi_end = AWI 2 years before |
| Bend points frozen at eligibility | Determined at eligibility age | `bp1_elig = bp1[which(age == first(elig_age))]` | ✓ | Lines 273-274 |
| PIA rounding | Floor to next lower $0.10 | `floor_dime()` rounds to $0.10 | ✓ | Fixed in commit d193627 |
| Minimum PIA (special) | $11.50 × (years of coverage - 10), COLA-adjusted | `special_min_pia = special_min_rate × (yoc - 10)` | ✓ | Implemented January 29, 2026 |
| PIA = max(regular, special min) | Return higher of regular or special minimum | `basic_pia = pmax(regular_pia, special_min_pia)` | ✓ | pia() lines ~330-340 |
| Years of coverage threshold | 25% old-law base (pre-1991), 15% (post-1990) | `yoc_threshold` from yoc.csv + projection | ✓ | assumptions_prep.R |
| Minimum years for special min | 11 years required | `min_yoc_for_special_min = 11` | ✓ | assumptions_prep.R |
| Maximum PIA | None specified (implicit from formula) | No explicit cap | ✓ | Maximum results from taxmax-capped earnings |

**Discrepancies Found**:

~~2. **PIA rounding precision**~~ **RESOLVED**
   - Fixed in commit d193627 (January 29, 2026)
   - Now uses `floor_dime()` to round PIA to next lower $0.10 per statute

~~3. **Special minimum PIA not implemented**~~ **RESOLVED**
   - Implemented January 29, 2026
   - Added `years_of_coverage()` function in eligibility.R to count qualifying years
   - Added `yoc_threshold`, `special_min_rate`, `min_yoc_for_special_min` to assumptions
   - Updated `pia()` to compare regular PIA vs special minimum PIA and return higher
   - Special minimum rate is $11.50 base, COLA-adjusted from 1979 with $0.10 floor rounding
   - Years of coverage threshold: 15% of old-law contribution base (post-1990 rule)
   - 19 new tests added in test-special_min_pia.R

**Questions for Discussion**:

*None — all issues resolved*

---

### 42 USC 415(i) — COLA

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 415(i)](https://www.law.cornell.edu/uscode/text/42/415)

**Package Implementation**: `cola()` in `R/benefit_calculations.R`, COLA data loaded in `R/assumptions_prep.R`

**Statutory Text Summary**:

1. **(i)(1)(D) Index Used**: "Consumer Price Index" (CPI-W - for Urban Wage Earners and Clerical Workers)

2. **(i)(1) Base Quarters**: "the calendar quarter ending on September 30 in each year after 1982" — Q3 CPI comparison

3. **(i)(2)(B) Effective Date**: COLA applies "for months after November of the calendar year" — December effective date, first full check in January

4. **(i)(2)(A)(ii) Rounding**: Adjusted amounts "not a multiple of $0.10 shall be decreased to the next lower multiple of $0.10"

5. **(i)(1)(C) Negative COLA**: Increases occur only when "applicable increase percentage" exceeds zero — no negative COLAs

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| Index for historical COLA | CPI-W Q3 comparison | Loaded from cola.csv (actual SSA COLAs) | ✓ | Historical COLAs match SSA published values |
| Index for projected COLA | CPI-W Q3 comparison | Calculated from CPI-W ratio in assumptions | ✓ | assumptions_prep.R lines 204-211 |
| Application timing | December effective (January check) | Applied starting year after eligibility | ✓ | Uses `lag(cola)` for correct timing |
| Year-by-year application | Each year's result rounded before next | Sequential loop with `floor()` at each step | ✓ | Lines 374-384 |
| Negative COLA protection | No negative COLAs | `pmax(lag(cola, default = 0), 0)` | ✓ | Line 358 |
| Rounding | Floor to $0.10 | `floor_dime()` rounds to $0.10 | ✓ | Fixed in commit d193627 |

**Discrepancies Found**:

~~4. **COLA-adjusted PIA rounding precision**~~ **RESOLVED**
   - Fixed in commit d193627 (January 29, 2026)
   - Now uses `floor_dime()` to round COLA-adjusted PIA to next lower $0.10 per statute

**Questions for Discussion**:

*None — all issues resolved*

---

### 42 USC 402(a) — Old-Age Benefits

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 402(a)](https://www.law.cornell.edu/uscode/text/42/402)

**Package Implementation**: `worker_benefit()` in `R/benefit_calculations.R`, `rf_and_drc()` helper

**Statutory Text Summary**:

1. **(a)(1) Eligibility**: Individual must be (A) fully insured, (B) age 62+, (C) file application

2. **(a)(1) Benefit Amount**: "such individual's old-age insurance benefit for any month shall be equal to his primary insurance amount...for such month" subject to reduction under (q) or increase under (w)

3. **(a)(3) Termination**: Benefits end "with the month preceding the month in which he dies"

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| Minimum age | Age 62+ | `elig_age_retired = 62` in assumptions | ✓ | Line 137 of assumptions_prep.R |
| Fully insured requirement | 40 quarters of coverage | `qc_required = 40`; checked in `aime()` | ✓ | Line 133 of assumptions_prep.R |
| Benefit = PIA | Benefit equals PIA (before adjustments) | `wrk_ben = floor(cola_basic_pia * act_factor)` | ✓ | Line 465 of benefit_calculations.R |
| Early reduction | Per subsection (q) | Applies `rf_and_drc()` | ✓ | See 402(q) audit below |
| Delayed credits | Per subsection (w) | Applies `rf_and_drc()` | ✓ | See 402(w) audit below |
| Benefits end at death | Month before death | `age < death_age` filter in PV functions | ✓ | Benefit stops in death year |
| Rounding | (Not specified here) | `floor()` rounds benefit to $1 | ✓ | Matches general practice |

**Discrepancies Found**:

*None — implementation matches statutory requirements*

**Questions for Discussion**:

*None*

---

### 42 USC 402(d) — Child's Benefits (NOT YET IMPLEMENTED)

**Statutory Requirements**:
- [x] Documented for future implementation (January 29, 2026)

**Source**: [42 USC 402(d)](https://www.law.cornell.edu/uscode/text/42/402)

**Package Implementation**: Not yet implemented

**Statutory Summary** (to inform future implementation):

| Requirement | Statute Says | Notes |
|-------------|--------------|-------|
| Eligible children | Child per 416(e): under 18, student under 19, or disabled before 22 | Includes biological, adopted, stepchildren |
| Benefit % (parent living) | 50% of parent's PIA | Same as spousal benefit percentage |
| Benefit % (parent deceased) | 75% of parent's PIA | Higher than living parent rate |
| Age limit (non-disabled) | Under 18, or under 19 if full-time student | Elementary or secondary school only |
| Student benefits | Full-time elementary/secondary student | Ends at 19 or graduation |
| Disabled child rules | Disability before age 22 | No age limit if disability continues |
| Dependency requirement | Must have been dependent on insured | At time of entitlement or death |
| Termination | Death, marriage, age out, disability ceases | Marriage to another beneficiary may not terminate |
| Subject to family max | Yes | See 403(a) family maximum |

**Implementation Notes for Future Development**:

1. **Data requirements**: Would need child birth year, disability status, student status
2. **Interaction with family maximum**: Child benefits must be reduced when total exceeds 403(a) cap
3. **Multiple children**: Each eligible child receives benefit (all subject to family max)
4. **Mother/father benefits (402(g))**: Parent caring for child under 16 can receive benefits (separate section)
5. **Priority**: Medium — needed for complete family benefit modeling

---

### 42 USC 403(a) — Family Maximum (NOT YET IMPLEMENTED)

**Statutory Requirements**:
- [x] Documented for future implementation (January 29, 2026)

**Source**: [42 USC 403(a)](https://www.law.cornell.edu/uscode/text/42/403)

**Package Implementation**: Not yet implemented

**Statutory Summary** (to inform future implementation):

| Requirement | Statute Says | Notes |
|-------------|--------------|-------|
| Family max formula | Bend point formula: 150%/272%/134%/175% | Similar structure to PIA formula |
| 1979 bend points | $230, $332, $433 | AWI-indexed annually like PIA bend points |
| Rounding | Nearest $0.10 (floor) | Same as PIA rounding |
| Who it applies to | All auxiliaries on one worker's record | Spouse, children, divorced spouse |
| Worker benefit protected | Worker's own benefit not reduced | Only auxiliary benefits reduced pro-rata |
| Disability alternative | Min of 85% AIME or 150% PIA | Lower cap for disability cases |

**Family Maximum Formula** (for future reference):
```
max_family_benefit =
  (1.50 × PIA up to fm_bp1) +
  (2.72 × PIA from fm_bp1 to fm_bp2) +
  (1.34 × PIA from fm_bp2 to fm_bp3) +
  (1.75 × PIA above fm_bp3)
```

**Implementation Notes for Future Development**:

1. **Data requirements**: Need family maximum bend points (fm_bp1, fm_bp2, fm_bp3) in assumptions
2. **Calculation order**: Calculate total auxiliary benefits first, then reduce pro-rata if exceeds max
3. **Worker benefit exempt**: Worker's own benefit is always paid in full
4. **Divorced spouse exception**: Divorced spouse benefits may not count against family max
5. **Interaction with child benefits**: Most relevant when children are receiving benefits
6. **Priority**: Medium — currently not needed since child benefits not implemented
7. **Note**: Spousal-only couples cannot exceed family max (spouse benefit ≤ 50% PIA, max ≥ 150% PIA)

---

### 42 USC 402(b) — Wife's Benefits

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 402(b)](https://www.law.cornell.edu/uscode/text/42/402)

**Package Implementation**: `spousal_pia()`, `spouse_benefit()` in `R/spousal.R`

**Statutory Text Summary**:

1. **(b)(1) Eligibility**: Wife or divorced wife of individual entitled to old-age or disability benefits, if she (A) files application, (B) has reached age 62 or has child in care, (C) is not entitled to own benefit equaling or exceeding half spouse's PIA

2. **(b)(2) Benefit Amount**: "such wife's insurance benefit for each month shall be equal to one-half of the primary insurance amount of her husband"

3. **(b)(2) Subject to subsection (q)**: Early claiming reductions apply per 402(q)

4. **Divorced wife**: Can receive benefits if married 10+ years, divorced 2+ years, and ex-spouse is eligible (even if not claiming)

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| Benefit percentage | 50% of spouse's PIA | `s_pia_share = 0.5` in assumptions | ✓ | assumptions_prep.R line 111 |
| Minimum age | 62 (or has child in care) | `age >= elig_age_ret` where elig_age_ret=62 | ✓ | spousal.R line 239 |
| Must wait for spouse to claim | Cannot start before spouse claims | `year >= yr_s_claim` | ✓ | spousal.R line 239 |
| Dual entitlement | Spousal benefit = 50% spouse PIA - own PIA | `(s_pia_share * s_pia) - own_pia` | ✓ | spousal.R line 240 |
| Early reduction (first 36 mo) | 25/36 of 1% per month | `s_rf1 <- 25 / 36 / 100` | ✓ | assumptions_prep.R line 118 |
| Early reduction (beyond 36 mo) | 5/12 of 1% per month | `s_rf2 <- assume$rf2` | ✓ | assumptions_prep.R line 119 |
| No DRCs for spouses | Spouses don't earn delayed credits | `rf_and_drc(..., drc=0)` | ✓ | spousal.R line 325 |
| Divorced spouse rules | 10+ year marriage, 2+ year divorced | Not implemented | — | Out of scope for hypotheticals |
| Child in care exception | Can claim before 62 with child | Not implemented | — | Child benefits not implemented |
| Benefit rounding | (Not specified in 402(b)) | `floor()` rounds to $1 | ✓ | spousal.R line 364 |

**Discrepancies Found**:

*None — implementation matches statutory requirements for implemented features*

**Questions for Discussion**:

*None — divorced spouse and child-in-care rules intentionally out of scope*

---

### 42 USC 402(c) — Husband's Benefits

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 402(c)](https://www.law.cornell.edu/uscode/text/42/402)

**Package Implementation**: `spousal_pia()`, `spouse_benefit()` in `R/spousal.R`

**Statutory Text Summary**:

Husband's benefits are substantively identical to wife's benefits under 402(b). The package treats spouses gender-neutrally, so the same functions handle both.

1. **(c)(1) Eligibility**: Husband or divorced husband with same requirements as wife
2. **(c)(2) Benefit Amount**: "such husband's insurance benefit for each month shall be equal to one-half of the primary insurance amount of his wife"

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| Gender neutrality | Same rules as 402(b) | Same functions used | ✓ | Code is gender-neutral |
| All 402(b) requirements | Identical to wife's benefits | See 402(b) analysis above | ✓ | |

**Discrepancies Found**:

*None — implementation is gender-neutral and matches both 402(b) and 402(c)*

**Questions for Discussion**:

*None*

---

### 42 USC 402(e) — Widow's Benefits

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 402(e)](https://www.law.cornell.edu/uscode/text/42/402)

**Package Implementation**: `widow_pia()`, `widow_benefit()` in `R/survivor.R`

**Statutory Text Summary**:

1. **(e)(1) Eligibility**: Widow or surviving divorced wife of fully insured individual, if she (A) is not married, (B) has reached age 60, or age 50-59 and disabled, (C) files application

2. **(e)(2) Benefit Amount**: The widow's insurance benefit equals the deceased's PIA, subject to RIB-LIM

3. **(e)(2)(D) RIB-LIM (Reduction In Benefits Limitation)**: If deceased claimed reduced benefits, widow benefit is:
   - Limited to the amount deceased would have received if still alive
   - But not less than 82.5% of the deceased's PIA

4. **(e)(3) Termination**: Benefits end upon remarriage (before age 60), death, or entitlement to own benefit >= widow benefit

5. **Disabled widow(er)**: Can claim at age 50-59 if disabled; 7-year rule applies (disability must occur within 7 years of spouse's death)

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| Standard eligibility age | Age 60 | `w_elig_age_ind = elig_age_retired - 2` = 60 | ✓ | survivor.R line 275 |
| Disabled widow age | Age 50-59 | `disabled_widow_elig_age <- 50` | ✓ | survivor.R line 158 |
| 7-year rule | Disability within 7 yrs of death | `yr_disability <= yr_s_death + 7` | ✓ | survivor.R line 152 |
| Benefit = deceased's PIA | 100% of PIA | `surv_s_pia` passed to survivor_pia | ✓ | survivor.R line 181-185 |
| RIB-LIM floor | Not less than 82.5% of PIA | `pmax(s_wrk_ben, 0.825 * s_pia)` | ✓ | survivor.R line 184 |
| RIB-LIM ceiling | Limited to deceased's actual benefit | `if s_wrk_ben > s_pia then s_wrk_ben` | ✓ | survivor.R line 183 |
| If deceased died before claiming | Use full PIA | `if s_death_age < s_claim_age then s_pia` | ✓ | survivor.R line 182 |
| Dual entitlement | Survivor benefit = survivor PIA - own PIA | `prelim_survivor_pia - own_pia` | ✓ | survivor.R line 189 |
| Disabled widow actuarial factor | Calculated as if age 60 | `effective_widow_claim_age = w_elig_age_ind` for disabled | ✓ | survivor.R lines 291-295 |
| Remarriage rules | Benefits end on remarriage before 60 | Not modeled | — | Remarriage not in scope |
| Benefit rounding | (Not specified in 402(e)) | `floor()` rounds to $1 | ✓ | survivor.R line 313 |

**Discrepancies Found**:

*None — implementation matches statutory requirements for implemented features*

**Questions for Discussion**:

- The 28.5% maximum reduction constant (line 278 of survivor.R) needs verification against POMS RS 00615.301. This appears correct for NRA 67 (28.5% = reduction for claiming at 60 when NRA is 67).

---

### 42 USC 402(f) — Widower's Benefits

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 402(f)](https://www.law.cornell.edu/uscode/text/42/402)

**Package Implementation**: `widow_pia()`, `widow_benefit()` in `R/survivor.R`

**Statutory Text Summary**:

Widower's benefits are substantively identical to widow's benefits under 402(e). The package treats survivors gender-neutrally, so the same functions handle both.

1. **(f)(1) Eligibility**: Widower or surviving divorced husband with same requirements as widow
2. **(f)(2) Benefit Amount**: Same as 402(e) — deceased's PIA subject to RIB-LIM

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| Gender neutrality | Same rules as 402(e) | Same functions used | ✓ | Code is gender-neutral |
| All 402(e) requirements | Identical to widow's benefits | See 402(e) analysis above | ✓ | |

**Discrepancies Found**:

*None — implementation is gender-neutral and matches both 402(e) and 402(f)*

**Questions for Discussion**:

*None*

---

### 42 USC 402(q) — Actuarial Reduction

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 402(q)](https://www.law.cornell.edu/uscode/text/42/402)

**Package Implementation**: `rf_and_drc()` in `R/benefit_calculations.R`, reduction factors in `tr2025` assumptions

**Statutory Text Summary** (from SSA Handbook citations in code):

1. **(q)(1) Worker reduction (first 36 months)**: 5/9 of 1% per month = 0.005555556 per month

2. **(q)(1) Worker reduction (beyond 36 months)**: 5/12 of 1% per month = 0.004166667 per month

3. **(q)(3) Spouse reduction (first 36 months)**: 25/36 of 1% per month = 0.006944444 per month

4. **(q)(3) Spouse reduction (beyond 36 months)**: 5/12 of 1% per month = 0.004166667 per month

5. **Maximum worker reduction**: At age 62 with NRA 67 (60 months early): 36×(5/9%) + 24×(5/12%) = 20% + 10% = **30%**

6. **Maximum spouse reduction**: At age 62 with NRA 67: 36×(25/36%) + 24×(5/12%) = 25% + 10% = **35%**

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| Worker rf1 (first 36 mo) | 5/9 of 1% = 0.005556 | `rf1 = 0.005555556` in assumptions | ✓ | Verified in CSV |
| Worker rf2 (beyond 36 mo) | 5/12 of 1% = 0.004167 | `rf2 = 0.004166667` in assumptions | ✓ | Verified in CSV |
| Spouse rf1 (first 36 mo) | 25/36 of 1% = 0.006944 | `s_rf1 <- 25 / 36 / 100` | ✓ | assumptions_prep.R line 118 |
| Spouse rf2 (beyond 36 mo) | 5/12 of 1% | `s_rf2 <- assume$rf2` | ✓ | assumptions_prep.R line 119 |
| Reduction formula | Months × rate | `dist_from_nra * rf1` or `(-36*rf1) + (beyond*rf2)` | ✓ | rf_and_drc() lines 98-100 |
| Reduction = 0 at/after NRA | No reduction at NRA+ | `if_else(dist_from_nra >= 0, 0, ...)` | ✓ | Line 98 |
| Applied to worker benefit | PIA × (1 - reduction) | `cola_basic_pia * act_factor` | ✓ | Line 465 |
| Applied to spouse benefit | Spousal PIA × (1 - reduction) | Uses `spouse_rf_and_drc()` | ✓ | See spousal.R |

**Discrepancies Found**:

*None — implementation matches statutory requirements*

**Questions for Discussion**:

*None*

---

### 42 USC 402(w) — Delayed Retirement Credits

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 402(w)](https://www.law.cornell.edu/uscode/text/42/402), [SSA Delayed Retirement](https://www.ssa.gov/benefits/retirement/planner/delayret.html)

**Package Implementation**: `rf_and_drc()` in `R/benefit_calculations.R`, `drc` values in `tr2025` assumptions

**Statutory Text Summary**:

1. **Credit Rate (current)**: 2/3 of 1% per month = 8% per year for birth year 1943+

2. **Phase-In Schedule** (per 1983 amendments):
   - Born 1937-38: 6.5% per year (0.005417/month)
   - Born 1939-40: 7.0% per year (0.005833/month)
   - Born 1941-42: 7.5% per year (0.00625/month)
   - Born 1943+: 8.0% per year (0.006667/month)

3. **Maximum Age**: DRCs stop accruing at age 70

4. **Application**: Credits applied monthly from NRA to claim age (up to age 70)

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| DRC rate (1943+) | 0.006667/month (8%/yr) | `drc = 0.006666667` for year 2005+ | ✓ | Verified in assumptions CSV |
| Phase-in by birth year | Gradual increase 1937-1943 | DRC values vary by year in assumptions | ✓ | Correct phase-in values |
| Max age 70 | No DRCs after age 70 | `drc_max_months = 36` (NRA 67 + 3 yrs = 70) | ✓ | assumptions_prep.R line 158 |
| DRC formula | Months past NRA × rate | `pmin(drc_max_months * drc, dist_from_nra * drc)` | ✓ | rf_and_drc() lines 103-104 |
| No DRC before NRA | DRC = 0 if claim ≤ NRA | `if_else(dist_from_nra <= 0, 0, ...)` | ✓ | Line 103 |

**Discrepancies Found**:

*None — implementation matches statutory requirements*

**Questions for Discussion**:

*None*

---

### 42 USC 430 — Contribution and Benefit Base

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 430](https://www.law.cornell.edu/uscode/text/42/430)

**Package Implementation**: `tr2025$taxmax`, projection formula in `assumptions_prep.R`

**Statutory Text Summary**:

1. **Base amount**: $60,600 for determination year 1994

2. **Indexing formula**: The contribution and benefit base equals the greater of:
   - Current year's amount, OR
   - $60,600 × (AWI for year preceding determination / AWI for 1992)

3. **Rounding**: Round to nearest $300; if multiple of $150 but not $300, round up to next $300

4. **Non-declining**: Cannot be less than prior year ("greater of" clause ensures this)

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| Base amount (1994) | $60,600 | `taxmax_base <- assume[year == 1994, "taxmax"]` = 60600 | ✓ | Loaded from raw data |
| Base year AWI | 1992 AWI | `awi_1992 <- assume[year == 1992, "awi"]` | ✓ | assumptions_prep.R line 34 |
| Indexing formula | base × (AWI_t-2 / AWI_1992) | `taxmax_base * awi_end / awi_1992` | ✓ | assumptions_prep.R line 63 |
| Rounding | Nearest $300 | `round(..../300)*300` | ✓ | assumptions_prep.R line 63 |
| Non-declining | Max of formula or prior year | `max(..., prev_taxmax)` | ✓ | assumptions_prep.R line 63 |
| Historical values | See SSA publications | Loaded from raw CSV | ✓ | Verified: 2023=$160,200, 2024=$168,600 |

**Discrepancies Found**:

*None — implementation matches statutory requirements*

**Questions for Discussion**:

*None*

---

### 42 USC 416(l) — Retirement Age

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 416(l)](https://www.law.cornell.edu/uscode/text/42/416)

**Package Implementation**: `tr2025$nra` column, referenced by `worker_benefit()`, `spouse_benefit()`, `widow_benefit()`

**Statutory Text Summary**:

1. **(l)(1) Definition**: "Retirement age" means:
   - 65 for those reaching early retirement age before Jan 1, 2000
   - 65 + age increase factor for Jan 1, 2000 - Dec 31, 2004
   - 66 for Jan 1, 2005 - Dec 31, 2016
   - 66 + age increase factor for Jan 1, 2017 - Dec 31, 2021
   - 67 for after Dec 31, 2021

2. **(l)(2) Early retirement age**: Age 62 for old-age, wife's, husband's; Age 60 for widow(er)'s

3. **Phase-in schedule** (by year turning 62):
   - 2000: 65 + 2 months = 65.167
   - 2001: 65 + 4 months = 65.333
   - 2002: 65 + 6 months = 65.5
   - 2003: 65 + 8 months = 65.667
   - 2004: 65 + 10 months = 65.833
   - 2005-2016: 66
   - 2017: 66 + 2 months = 66.167
   - 2018: 66 + 4 months = 66.333
   - 2019: 66 + 6 months = 66.5
   - 2020: 66 + 8 months = 66.667
   - 2021: 66 + 10 months = 66.833
   - 2022+: 67

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| NRA = 65 (pre-2000) | 65 for year 62 < 2000 | `nra = 65` for years 1990-1999 | ✓ | Verified in raw data |
| NRA = 65 + months (2000-2004) | Phase-in 2 months/year | `nra` values: 65.167, 65.333, 65.5, 65.667, 65.833 | ✓ | Verified in raw data |
| NRA = 66 (2005-2016) | 66 for years 2005-2016 | `nra = 66` for years 2005-2016 | ✓ | Verified in raw data |
| NRA = 66 + months (2017-2021) | Phase-in 2 months/year | `nra` values: 66.167, 66.333, 66.5, 66.667, 66.833 | ✓ | Verified in raw data |
| NRA = 67 (2022+) | 67 for year 62 >= 2022 | `nra = 67` for years 2022+ | ✓ | Verified in raw data |
| Early retirement age (OA) | 62 | `elig_age_retired = 62` | ✓ | assumptions_prep.R line 137 |
| Early retirement age (widow) | 60 | `w_elig_age_ind = elig_age_retired - 2` | ✓ | survivor.R line 275 |
| NRA lookup by birth cohort | Based on year turning 62 | `nra_ind = nra[which(year == yr_62)]` | ✓ | benefit_calculations.R line 483 |

**Discrepancies Found**:

*None — implementation matches statutory requirements. NRA values are loaded from raw Trustees Report data and match the statutory phase-in schedule.*

**Questions for Discussion**:

*None*

---

### 42 USC 409(k) — AWI Definition

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 409(k)](https://www.law.cornell.edu/uscode/text/42/409)

**Package Implementation**: `tr2025$awi`, used throughout benefit calculation pipeline

**Statutory Text Summary**:

1. **(k)(1) Definition**: "National average wage index" means the average of total wages for a calendar year

2. **(k)(2) Computation**: Based on amounts reported to Treasury, WITHOUT applying the wage ceiling (includes all earnings)

3. **(k)(3) Deferred compensation**: After 1990, includes deferred compensation amounts

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| AWI values | Published by SSA annually | Loaded from Trustees Report data | ✓ | Historical + projected values |
| Use in indexing | Used for earnings indexing, bend points, taxmax | Used in `aime()`, `pia()`, `assumptions_prep()` | ✓ | Core to benefit calculations |
| Source data | Commissioner determines from Treasury reports | Trustees Report projections | ✓ | TR projections are official |

**Discrepancies Found**:

*None — AWI values are loaded from official SSA Trustees Report data*

**Questions for Discussion**:

*None*

---

### 42 USC 415(a)(1)(A) — Bend Points

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 415(a)(1)(B)](https://www.law.cornell.edu/uscode/text/42/415) — bend point indexing is in (a)(1)(B), formula is in (a)(1)(A)

**Package Implementation**: `tr2025$bp1`, `tr2025$bp2`, projection in `assumptions_prep.R`

**Statutory Text Summary**:

1. **(a)(1)(A) PIA Formula**: Uses bend points to apply 90%/32%/15% replacement factors

2. **(a)(1)(B)(i) 1979 values**: First bend point = $180, Second bend point = $1,085

3. **(a)(1)(B)(ii) Indexing**: For years after 1978: multiply 1979 amounts by ratio of AWI for year individual turns 62 (or becomes disabled/dies) divided by AWI for 1977

4. **Rounding**: Round to nearest dollar

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| 1979 bend points | $180 / $1,085 | Loaded from raw CSV | ✓ | bp1=180, bp2=1085 for 1979 |
| Indexing formula | bp × (AWI_elig / AWI_1977) | `bp1_i <- round(bp1_base * awi_end / awi_1977)` | ✓ | assumptions_prep.R lines 72-73 |
| Base year AWI | 1977 AWI | `awi_1977 <- assume[year == 1977, "awi"]` | ✓ | assumptions_prep.R line 33 |
| Rounding | Nearest dollar | `round()` | ✓ | assumptions_prep.R line 72 |
| Frozen at eligibility | Determined at eligibility age | `bp1_elig = bp1[which(age == first(elig_age))]` | ✓ | benefit_calculations.R line 299 |

**Discrepancies Found**:

*None — implementation matches statutory requirements*

**Questions for Discussion**:

*None*

---

### 42 USC 414 — Insured Status

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026)

**Source**: [42 USC 414](https://www.law.cornell.edu/uscode/text/42/414)

**Package Implementation**: `qc_comp()` in `eligibility.R` calculates quarters of coverage; `aime()` checks for 40 QCs

**Statutory Text Summary**:

1. **(a) Fully insured**: 1 QC per year after 1950 (or age 21) through year before eligibility, minimum 6, OR 40 QCs total

2. **(b) Currently insured**: At least 6 QCs in the 13-quarter period ending with the quarter of death/eligibility

3. **40 QC rule**: Anyone with 40 QCs is fully insured regardless of timing

**Comparison**:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| 40 QC threshold | 40 QCs = fully insured | `qc_required = 40` in assumptions | ✓ | assumptions_prep.R line 133 |
| QCs per year | Max 4 per year | `max_qc_per_year = 4` | ✓ | assumptions_prep.R line 154 |
| QC earnings requirement | AWI-indexed amount | `qc_rec` column in assumptions | ✓ | Projected in assumptions_prep.R |
| Cumulative QCs | Track total through each age | `qc_tot = cumsum(qc_i)` | ✓ | eligibility.R line 27 |
| Eligibility check | Must have qc_required QCs | `qc_eligible <- qc_tot >= qc_required_val` | ✓ | benefit_calculations.R line 213 |
| Currently insured | 6 QCs in 13 quarters | Not implemented | — | Not needed for retirement workers |
| 1-QC-per-year rule | Alternative to 40 QCs | Not implemented | — | 40 QC rule sufficient for most workers |

**Discrepancies Found**:

*None for retirement benefits — the 40-QC fully insured rule is implemented correctly*

**Questions for Discussion**:

- **Currently insured status**: Not implemented, but only relevant for survivor benefits when worker dies young. Hypothetical workers typically have full careers, so this is unlikely to matter.
- **1-QC-per-year rule**: Not implemented as an alternative; the 40-QC threshold covers most cases adequately.

---

### 42 USC 415(b)(2) — Computation Years / Earnings Record

**Statutory Requirements**:
- [x] Reviewed (January 29, 2026) — See 42 USC 415(b) audit above for full details

**Source**: [42 USC 415(b)(2)](https://www.law.cornell.edu/uscode/text/42/415)

**Package Implementation**: `comp_period()` in `eligibility.R`, `aime()` in `benefit_calculations.R`, `earnings_generator()` in `earnings.R`

**Note**: This section was fully covered under the 42 USC 415(b) AIME audit. Key points:

| Requirement | Statute Says | Code Does | Match? | Notes |
|-------------|--------------|-----------|--------|-------|
| Elapsed years | Years from age 22 to eligibility year - 1 | `elapsed_years = pmax(elig_age - 1 - 21, 0)` | ✓ | eligibility.R line 79 |
| Dropout years (retirement) | 5 years | `pmin(max_dropout_years, 5)` | ✓ | eligibility.R line 85 |
| Dropout years (disability) | floor(elapsed/5), max 5 | `pmin(max_dropout_years, floor(elapsed_years / 5))` | ✓ | eligibility.R line 86 |
| Computation period | elapsed - dropout, min 2 | `pmax(min_comp_period, elapsed_years - dropout_years)` | ✓ | eligibility.R line 88 |
| Highest years used | Top N years in comp period | Partial sort to get top earnings | ✓ | benefit_calculations.R lines 227-233 |

**Discrepancies Found**:

*None — see 42 USC 415(b) audit for the resolved disability dropout years discrepancy*

**Questions for Discussion**:

*None*

---

## Parameters Verification

*Separate from rule verification — confirms parameter values match SSA publications.*

| Parameter | SSA Source | ssmbar Location | Verified? |
|-----------|------------|-----------------|-----------|
| AWI series | Trustees Report Table V.B1 | `tr2025$awi` | ☐ |
| Bend points | SSA annual announcement | `tr2025$bp1`, `tr2025$bp2` | ☐ |
| Taxable maximum | Trustees Report | `tr2025$taxmax` | ☐ |
| COLA factors | Trustees Report Table V.B1 | `tr2025$cola` | ☐ |
| Normal retirement ages | Statute + regulations | Various | ☐ |
| Scaled earnings factors | Actuarial Note 2025.3 | `sef2025` | ☐ (verified in validation) |
| DRC rates by birth year | Statute | Various | ☐ |

---

## Summary of Discrepancies

*Consolidated list for easy reference.*

| # | Location | Statutory Requirement | Package Behavior | Severity | Resolution |
|---|----------|----------------------|------------------|----------|------------|
| 1 | `comp_period()` in eligibility.R | 415(b)(2)(A): Disability dropout = floor(elapsed/5), max 5 | ~~Uses `floor(elig_age/5)` formula~~ | ~~Low~~ | **RESOLVED** - Fixed in commit 9d867f5 |
| 2 | `pia()` in benefit_calculations.R | 415(a)(2)(C): PIA rounds to $0.10 | ~~Uses `floor()` rounding to $1~~ | ~~Low~~ | **RESOLVED** - Fixed in commit d193627 using `floor_dime()` |
| 3 | `pia()` | 415(a)(1)(C)(i): Special minimum PIA | ~~Not implemented~~ | ~~Low~~ | **RESOLVED** - Implemented January 29, 2026 with `years_of_coverage()`, `special_min_rate` |
| 4 | `cola()` in benefit_calculations.R | 415(i)(2)(A)(ii): COLA-adjusted amounts round to $0.10 | ~~Uses `floor()` rounding to $1~~ | ~~Low~~ | **RESOLVED** - Fixed in commit d193627 using `floor_dime()` |

---

## Audit Notes

*Observations that don't fit elsewhere.*

-
