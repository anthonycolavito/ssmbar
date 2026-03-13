# Plan: Implement "Repeal Spousal Benefits" on the Benefit Explorer

## Key Architectural Challenge

The reform data generation (`scripts/generate_reform_data.R`) currently produces **single-worker data only**. "Repeal Spousal Benefits" sets `s_pia_share = 0`, which has **zero effect on single workers** — spousal benefits don't exist for singles. The reform is only meaningful for married couples.

The baseline data generation (`scripts/generate_currentlaw_data.R`) already handles married workers (6 types × 6 spouse types × 2 sexes × 8 birth years = 576 married configs). But the reform generation has no married support.

**This means we must extend the reform generation to produce married data, at least for combos that include spousal-affecting reforms.**

## Step 1 — Define Reform Parameters

- **Column modified**: `s_pia_share` (spousal PIA share, current law = 0.50)
- **Reform value**: 0 (complete repeal)
- **Type**: `"replace"`
- **Effective year**: 2026
- **Phase-in**: 10 years, cohort-based (workers reaching eligibility age 62 in 2026+ are affected)
  - Question for SS expert: Should this be immediate repeal or phased? The existing `reform_phase_out_spousal()` uses 10-year phase-in.
- **Indexing**: None (it's a ratio, not a dollar amount)

## Step 2 — Reform Template

Already exists: `reform_phase_out_spousal()` in `R/reform_templates.R` (line 461). Sets `s_pia_share = 0` with configurable `effective_year` and `phase_in_years`.

For the app, we'll use: `reform_phase_out_spousal(effective_year = 2026, phase_in_years = 10)`

## Step 3 — Reform Documentation

Create `docs/reforms/phase_out_spousal.md`.

## Step 4 — Add to Generation Script

### 4a. Add "other" category to `reform_defs`:
```r
other = list(
  effective_year = 2026L,
  phase_type = "cohort",
  reforms = list(
    phase_out_spousal = function() reform_phase_out_spousal(2026, phase_in_years = 10)
  )
)
```

### 4b. Update `valid_cats` to include "other"

### 4c. Skip logic for "other" category:
- Cohort-based, effective_year = 2026
- First affected: born 1964 (turns 62 in 2026)
- Our decadal birth years: skip 1940, 1950, 1960

### 4d. Combo count impact:
- Current: PIA(4) × NRA(4) × COLA(4) × TaxMax(4) - 1 = 255 combos
- Adding Other(2): 4×4×4×4×2 - 1 = 511 combos
- BUT: for single workers, phase_out_spousal has zero effect. So combos with phase_out_spousal produce identical single-worker data as the same combo without it.
- **Optimization**: For single-worker data, skip phase_out_spousal entirely — don't duplicate computation. Only generate married data for combos that include phase_out_spousal.

### 4e. Add married data generation:
For each combo that includes `phase_out_spousal`:
1. Apply reforms to assumptions (including `s_pia_share = 0`)
2. For each worker type × spouse type × birth year:
   - Call `calculate_benefits_reform()` with spouse params
   - Extract couple metrics (monthly_benefit, annual_couple, pv_benefits, etc.)
3. Store married reform data in the JSON output

**Married data volume**:
- Combos with phase_out_spousal: 4×4×4×4×1 = 256 combos
- Per combo: 6 spouse types × 5 birth years (skip 1940/1950/1960) = 30 configs
- Per worker type: 256 × 30 = 7,680 configs
- Total across 6 worker types: 46,080 configs

This is too many for overnight. **Practical scope reduction**: Generate married data only for standalone `phase_out_spousal` (1 combo) and limit cross-category combos to the most popular (e.g., phase_out_spousal + each single PIA/NRA/COLA reform = ~9 combos). Or: generate all 256 combos but only for cohort metrics (skip NMTR for married).

**Recommended approach**: Generate married cohort + individual data for ALL combos with phase_out_spousal (no married NMTR). This is just the combo phase — no parallel NMTR needed — so it should be fast (~3 sec per config × 7,680 configs / 6 workers = ~3,840 sec = ~64 min per worker = ~384 min total).

Actually that's still ~6.4 hours. Need further optimization: since phase_out_spousal only changes `s_pia_share`, for combos like "chained_cpi+phase_out_spousal", the married data only differs from "chained_cpi" married baseline in the spousal benefit component. But we still need to run the full pipeline.

**Revised recommended approach**:
1. Generate married data for standalone `phase_out_spousal` only (no cross-category combos for married)
2. For cross-category combos involving phase_out_spousal, generate single-worker data only (identical to combo without spousal reform — skip by reusing existing keys)
3. This gives: 1 combo × 6 spouse types × 5 birth years × 6 workers = 180 configs = ~9 min total

## Step 5 — Tests

Add to `test-reform-templates.R`:
- `reform_phase_out_spousal()` parameter count = 1
- Parameter name is `s_pia_share`
- Value type is scalar (0)
- Reform class and attributes

## Step 6 — Composability Smoke Test

Test that phase_out_spousal composes with other reforms without errors:
```r
reform_spousal <- reform_phase_out_spousal(2026, 10)
reform_pia <- reform_flat_benefit(effective_year = 2030, assumptions = tr2025)
reform_cola <- reform_chained_cpi(2026)
combined <- apply_reforms(tr2025, list(reform_spousal, reform_pia, reform_cola),
                          check_exclusivity = FALSE)
# Must not error for both single and married
```

## Step 7 — Generate Data

```bash
ps aux | grep "R --no-echo" | grep -v grep
/usr/local/bin/Rscript scripts/generate_reform_data.R --categories pia,nra,cola,other --cores 6
```

## Step 8 — Verify

- Married phase_out_spousal: spouse benefit should be $0 for fully-phased cohorts
- Married phase_out_spousal: worker's own benefit should be unchanged from baseline
- Phase-in check: birth year 1970 (elig 2032, 7/10 phased) should show partial reduction
- Single data: identical to baseline (no effect)

## Step 9 — UI Changes

### index.html:
Change the locked "Other" category to an interactive one with one button:
```html
<div class="reform-category" data-category="other">
  <div class="category-header" onclick="toggleReformCategory('other')">
    <i class="bi bi-gear category-icon"></i>
    <span class="category-label">Other Reforms</span>
    <i class="bi bi-chevron-down category-chevron" id="other-chevron"></i>
  </div>
  <div class="category-body" id="other-body">
    <button class="reform-option" data-category="other" data-reform="phase_out_spousal"
            onclick="selectReform('other', 'phase_out_spousal', this)">
      Repeal Spousal Benefits
    </button>
  </div>
</div>
```

### manifest.json:
- Add "other" to `active_reform_categories`
- Add `"phase_out_spousal": "Repeal Spousal Benefits"` to `reform_labels`
- Add to `reform_categories.other.reforms`

### JS changes:
- When combo includes "phase_out_spousal" and user is viewing single: strip "phase_out_spousal" from lookup key (data is identical to baseline or to the other reforms in the combo)
- When combo is exactly "phase_out_spousal" and user is viewing married: load married reform data
- For other married+reform combos: show "reform married note" as before

## Step 10 — Run Tests, Update PROGRESS.md, Commit

## Open Questions for Expert Review

1. **Phase-in or immediate?** "Repeal Spousal Benefits" sounds immediate, but `reform_phase_out_spousal` uses 10-year phase-in. Should we use `phase_in_years = 0` for immediate repeal?
2. **Survivor benefits**: Does setting `s_pia_share = 0` affect widow/survivor benefits? The spousal benefit (`s_pia_share`) and survivor benefit use different code paths. Need to verify `reform_phase_out_spousal` doesn't inadvertently affect survivors.
3. **Married data format**: How should married reform data be keyed in the JSON? Should it use the existing married dimension keys (e.g., `male_married_low`) or a new format?
4. **Generation scope**: Is generating married data for standalone phase_out_spousal only (no cross-category combos) acceptable for MVP?
