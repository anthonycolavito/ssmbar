# Reform Data Generation — Complete Process Guide

This skill file documents the end-to-end process for adding new Social Security reforms to the ssmbar Benefit Explorer and generating the JSON data that powers it. Follow these instructions exactly.

---

## Part 1: Adding a New Reform

### Step 1 — Define Reform Parameters

Before writing any code, document these properties:
- Which SSA assumption columns the reform modifies (e.g., `cola`, `fact2`, `nra`, `flat_benefit`)
- Type of modification: `"replace"` (overwrite) or `"add"` (offset)
- Effective year (when reform takes effect)
- Phase-in: duration (years), dimension (`"cohort"` = by eligibility year, `"calendar"` = by calendar year)
- Indexing: constant, AWI-indexed (two-year lag), CPI-indexed, or a function of year
- For AWI-indexed values: base amount in 2025$, formula `value(year) = base × AWI(year-2) / AWI(2023)`

### Step 2 — Create Reform Template Function

Add `reform_xxx()` to `R/reform_templates.R`:

```r
reform_xxx <- function(target_value, effective_year = 2026, phase_in_years = 10,
                        assumptions = NULL) {
  # For AWI-indexed parameters, create a closure capturing the AWI schedule:
  if (!is.null(assumptions)) {
    awi_schedule <- setNames(assumptions$awi, assumptions$year)
    awi_base <- awi_schedule[["2023"]]
    value_fn <- function(year) {
      awi_ref <- awi_schedule[as.character(year - 2)]
      if (length(awi_ref) == 0 || is.na(awi_ref)) return(target_value)
      unname(target_value * awi_ref / awi_base)
    }
  } else {
    value_fn <- target_value  # constant fallback for tests
  }

  create_reform(
    name = "Reform Name",
    description = "...",
    parameters = list(
      list(param = "column_name", value = value_fn, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = phase_in_years
  )
}
```

Key rules:
- If the value varies by year, pass a `function(year)` — `apply_single_parameter()` already supports this
- If AWI-indexed, pass the `assumptions` argument and build a closure (see `reform_flat_benefit()` as reference)
- For NRA reforms that handle their own phase-in logic, set `phase_in_years = 0`

### Step 3 — Write Reform Documentation

Create `docs/reforms/xxx.md` with these sections:
1. **Summary** — one sentence
2. **Parameters Modified** — table: param name, current law value, reform value, type
3. **Effective Year**
4. **Phase-In** — duration, dimension, mechanism, example cohort calculations
5. **Indexing** — how values change over time
6. **Formula** — exact calculation with pseudocode
7. **Expected Behavior** — table: effect per worker type (very_low through max)
8. **Code References** — template function, consumption points in reform_functions.R

### Step 4 — Add to Generation Script

In `scripts/generate_reform_data.R`:

1. Add the reform factory to the `reform_defs` list (line ~75):
   ```r
   category_name = list(
     effective_year = 2026L,
     phase_type = "cohort",  # or "calendar"
     reforms = list(
       reform_key = function() reform_xxx(effective_year = 2026, assumptions = tr2025)
     )
   )
   ```

2. **Update `valid_cats`** if adding a new category

3. **Update `get_birth_years_for_combo()` skip logic** (line ~164):
   - Add a `has_xxx` flag for the new category
   - Define which birth years to skip (see "Skip Logic" in `.claude/skill.md`)
   - This is critical — wrong skip logic wastes computation or produces missing data

4. **Be aware of combo count growth**: adding a 4th category with 3 reforms: (3+1)^4 - 1 = 255 combos (vs 63 now). The pattern is (reforms+1) per category (the +1 is the "none" option), all multiplied, minus 1 for the all-none baseline. This quadruples generation time.

### Step 5 — Write Tests

Add to `tests/testthat/test-reform-templates.R`:
- Parameter count and names
- Value types (scalar vs function)
- For function-valued params: verify output varies correctly by year
- For AWI-indexed params: verify growth proportional to AWI schedule

### Step 6 — Composability Smoke Test (BEFORE full generation)

Run a quick R script (2 minutes) that:
```r
devtools::load_all(".", quiet = TRUE)
reform_new <- reform_xxx(effective_year = 2026)
# Combine with one reform from each other category
reform_pia <- reform_flat_benefit(effective_year = 2030, assumptions = tr2025)
reform_nra <- reform_nra_to_68(2026)
reform_cola <- reform_chained_cpi(2026)
combined <- apply_reforms(tr2025, list(reform_new, reform_pia, reform_nra, reform_cola),
                           check_exclusivity = FALSE)
# Run pipeline for 2-3 configs
for (by in c(1970, 2000)) {
  w <- calculate_benefits_reform(birth_yr = by, sex = "all", type = "medium",
                                  age_claim = 65, factors = sef2025,
                                  assumptions = combined, debugg = TRUE)
  cat(sprintf("by=%d: PIA=%.2f, benefit=%.2f\n", by,
      w$basic_pia[w$age == 65], w$ben[w$age == 65]))
}
```
This catches composability bugs before committing to a 1-2 hour generation run.

### Step 7 — Generate Data

```bash
# Kill zombie R processes first
ps aux | grep "R --no-echo" | grep -v grep

# Run generation
/usr/local/bin/Rscript scripts/generate_reform_data.R --categories pia,nra,cola --cores 6
```

### Step 8 — Verify (see Part 3 below)

### Step 9 — Add to UI

1. Add button in `docs/index.html` under the correct category
2. Update `docs/data/manifest.json`: `active_reform_categories` and `reform_labels`
3. Test in browser: cohort chart, individual chart, NMTR chart all show reform overlay

### Step 10 — Run Tests, Update PROGRESS.md, Commit

Follow the commit workflow in CLAUDE.md exactly: update PROGRESS.md, run `devtools::test()` immediately before committing, stage specific files only.

---

## Part 2: Efficient Data Generation

### Architecture

The script has two phases per worker type:
1. **Combo phase** (~3 min): 63 combos × 5-7 birth years, sequential. Computes cohort metrics and individual benefit series.
2. **NMTR phase** (~10-21 min depending on optimizations): ~411 configs via `mclapply(mc.cores = 6)`. Computes net marginal tax rates.

Worker types are processed sequentially: very_low, low, medium, high, max, custom_50k.

### Critical Rules

#### Rule 1: No Nested Parallelism

**NEVER use `mclapply` for the outer worker-type loop.** The inner NMTR phase already uses `mclapply(mc.cores = 6)`. Nesting creates `n_cores^2` processes on 8 CPU cores, causing:
- Severe CPU oversubscription (each process gets ~12% of a core)
- 3+ GB swap usage (machine has 8 GB RAM)
- 4+ hours with no completion (vs ~24 min per worker sequentially)

```r
# CORRECT: sequential outer, parallel inner
results <- lapply(worker_configs, process_worker_type)

# WRONG: parallel outer + parallel inner = process explosion
results <- parallel::mclapply(worker_configs, process_worker_type, mc.cores = n_cores)
```

#### Rule 2: Monitor Process Count

During the NMTR phase, verify: `ps aux | grep "R --no-echo" | wc -l` = **7** (1 parent + 6 children). During the combo phase (sequential), the count should be **1**. If you ever see 13+, nested parallelism is occurring — kill immediately with `pkill -f "R --no-echo"`.

Check swap: `sysctl vm.swapusage`. If swap used > 1 GB, something is wrong.

#### Rule 3: Keep R Processes <= 8

On the 8-core Mac with 8 GB RAM, use `mc.cores = 6` (leaves 2 cores for OS + parent). Total active R processes must stay at 7 or fewer.

### Optimization Checklist

These optimizations should be implemented in `scripts/generate_reform_data.R` and `R/analytic_functions.R`. Check whether each is already implemented before regenerating data.

#### Optimization 1 — Cache worker_t in `marginal_benefit_analysis()` [SAVES ~63 MIN]

**File**: `R/analytic_functions.R`, function `marginal_benefit_analysis()`

**Problem**: The loop at lines 624-643 calls `compute_benefits_for_years()` twice per iteration (for `t` and `t-1` years of work). Each call runs the full benefit pipeline (aime->pia->cola->worker_benefit). But the pipeline output for `t-1` years was already computed in the previous iteration — only the PV discounting changes.

**Fix**: Cache each `worker_t` object after the benefit pipeline runs. Reuse it in the next iteration and only recompute `compute_pv_to_year()` with the new `discount_to_year`.

```r
# Before the loop, create cache:
# NOTE: Remove the existing dead-code `pv_cache` variable (line 558) when implementing this.
worker_cache <- vector("list", n_working + 1)  # indices 1 to n_working+1
# Index convention: worker_cache[[t_years + 1]] = worker_t data.frame for t_years of work

for (t in seq_len(n_working)) {
  idx <- working_idx[t]
  working_year <- w$year[idx]

  # --- Pipeline for t years (compute if not cached) ---
  if (is.null(worker_cache[[t + 1]])) {
    result_t <- compute_benefits_for_years(t, working_year)  # existing function, runs full pipeline
    worker_cache[[t + 1]] <- result_t$worker  # cache the data.frame, NOT the PV
    aime_t <- result_t$aime    # already eligibility-gated by compute_benefits_for_years
    pia_t  <- result_t$pia
  } else {
    # Reuse cached pipeline output — extract aime/pia with eligibility check
    wt <- worker_cache[[t + 1]]
    ci <- which(wt$year == birth_yr + ceiling(claim_age))
    aime_t <- if (length(ci) > 0) wt$aime[ci[1]] else 0
    pia_t  <- if (length(ci) > 0) wt$cola_basic_pia[ci[1]] else 0
    qc_val <- if (length(ci) > 0) wt$qc_tot[ci[1]] else 0
    if (is.na(qc_val) || qc_val < 40) { aime_t <- 0; pia_t <- 0 }  # eligibility gate
  }
  # Always recompute PV with the current iteration's discount_to_year
  pv_t <- compute_pv_to_year(worker_cache[[t + 1]], ..., discount_to_year = working_year)

  # --- Pipeline for t-1 years (should be cached from previous iteration) ---
  if (is.null(worker_cache[[t]])) {
    result_tm1 <- compute_benefits_for_years(t - 1, working_year)
    worker_cache[[t]] <- result_tm1$worker
  }
  pv_tm1 <- compute_pv_to_year(worker_cache[[t]], ..., discount_to_year = working_year)

  w$cumulative_aime[idx] <- aime_t
  w$cumulative_pia[idx]  <- pia_t
  w$cumulative_pv[idx]   <- pv_t
  w$delta_pv_benefits[idx] <- pv_t - pv_tm1
}
```

**Why it's correct**: The benefit pipeline (aime, pia, cola, worker_benefit) for `t_years` of work depends ONLY on `t_years` (the earnings truncation point). The `discount_to_year` parameter only affects `compute_pv_to_year()`, which takes the pipeline output as input. These are fully separable. The cache stores the `worker_t` data.frame (after aime->pia->cola->worker_benefit), and `compute_pv_to_year()` is re-called each iteration with the current `discount_to_year`. The eligibility gate (`qc_tot < 40`) is applied when extracting aime/pia from cached workers.

**Savings**: Cuts pipeline calls from 88 to 45 per config (49% reduction). ~63 min off the ~126 min NMTR total.

#### Optimization 2 — Reuse Worker Objects from Combo Phase [SAVES ~18 MIN]

**File**: `scripts/generate_reform_data.R`

**Problem**: The combo phase (lines 333-405) calls `compute_reform_config()` for each (combo, birth_year) pair. The NMTR phase (lines 416-444) then rebuilds `reformed_assumptions` and re-calls `compute_reform_config()` for the same pairs. This is completely redundant — 411 unnecessary pipeline calls per worker type.

**Fix**: During the combo phase, save each `worker` object and its `reformed_assumptions`:

```r
# Before combo loop:
saved_configs <- list()

# Inside combo loop, after computing worker at line 364:
saved_configs[[paste(combo_key, by)]] <- list(
  worker = worker,
  reformed_assumptions = reformed_assumptions
)

# In NMTR config builder (lines 416-431), pull from saved_configs:
cfg <- saved_configs[[paste(combo_key, by)]]
nmtr_configs[[length(nmtr_configs) + 1]] <- list(
  combo_key = combo_key,
  birth_year = by,
  worker = cfg$worker,
  reformed_assumptions = cfg$reformed_assumptions
)

# In mclapply function (line 436-444), use cfg$worker directly:
nmtr_results <- parallel::mclapply(nmtr_configs, function(cfg) {
  if (is.null(cfg$worker)) return(NULL)
  compute_nmtr_reform(cfg$worker, cfg$reformed_assumptions)
  ...
}, mc.cores = n_cores)
```

**Memory impact**: ~16-25 MB (411 worker data.frames × ~40-60 KB each with debugg=TRUE columns). Negligible on 8 GB machine. With mclapply's fork-based parallelism, child processes inherit these read-only via copy-on-write with zero copy cost.

**Savings**: Eliminates 411 redundant `compute_reform_config()` + `build_reforms()` + `apply_reforms()` calls per worker type. ~18 min total.

#### Optimization 3 — Pre-join Assumptions on Base Worker [SAVES ~2 MIN]

**File**: `R/analytic_functions.R`, function `marginal_benefit_analysis()`

**Problem**: Inside `compute_benefits_for_years()`, each call to `aime_reform()` checks which assumption columns are missing from the worker and does a `left_join` to add them. Since `base_worker` is a column subset, every one of the ~36K pipeline calls per worker type redundantly joins ~30 columns.

**Fix**: Before the per-worker loop, pre-join once:
```r
base_worker <- join_all_assumptions(base_worker, assumptions)
```
Then downstream functions skip the join (they check `cols_needed %in% names(worker)`).

#### Optimization 4 — Short-circuit t=0 Case [SAVES ~1 MIN]

**File**: `R/analytic_functions.R`, function `compute_benefits_for_years()`

The case `t_years = 0` zeros all earnings, producing AIME=0, PIA=0, benefits=0. Running the full pipeline is wasteful. Add early return:
```r
if (t_years == 0) return(list(aime = 0, pia = 0, pv = 0, worker = NULL))
```

### Performance Expectations

| Configuration | Per Worker (combo + NMTR) | Total (6 workers) |
|---|---|---|
| Current (no optimizations) | ~24 min (~3 combo + ~21 NMTR) | ~144 min |
| + Opt 1 (cache worker_t) | ~14 min (~3 + ~11) | ~84 min |
| + Opt 2 (reuse from combo) | ~11 min (~3 + ~8) | ~66 min |
| + Opt 3-4 (pre-join + t=0) | ~10.5 min (~3 + ~7.5) | ~63 min |
| **Fully optimized** | **~10.5 min** | **~63 min (~1 hour)** |

### Running the Script

```bash
# Kill zombie R processes
ps aux | grep "R --no-echo" | grep -v grep
# If any exist:
pkill -f "R --no-echo"

# Full generation (all categories, all worker types)
/usr/local/bin/Rscript scripts/generate_reform_data.R --categories pia,nra,cola --cores 6

# Debug mode: one worker type, one category (fast iteration, ~3 min)
/usr/local/bin/Rscript scripts/generate_reform_data.R --categories pia --type medium --cores 6
```

### Monitoring

- Worker types process sequentially: very_low -> low -> medium -> high -> max -> custom_50k
- Process count: `ps aux | grep "R --no-echo" | wc -l` — must be 7 (1 parent + 6 cores), NOT 13+
- Swap check: `sysctl vm.swapusage` — if used > 1 GB, kill and investigate
- File output: `find docs/data/reform -name "*.json" -newermt "today" | wc -l`
- Each worker writes 3 files: `cohort/{type}.json`, `individual/{type}.json`, `individual/{type}_nmtr.json`

---

## Part 3: Verification Procedures

### Mandatory Checks (do ALL of these after every generation)

#### Check 1 — Exact Numerical Spot-Checks (7+ configs)

Pick diverse configs covering:
- At least 2 single reforms (one PIA, one COLA)
- At least 1 cross-category combo (e.g., flat_benefit+chained_cpi)
- At least 2 worker types (one low, one high)
- At least 2 birth years (one early-phase, one fully-phased)

For each, compute fresh in R:
```r
devtools::load_all(".", quiet = TRUE)
reform_list <- list(reform_flat_benefit(effective_year = 2030, assumptions = tr2025),
                     reform_chained_cpi(2026))
reformed <- apply_reforms(tr2025, reform_list, check_exclusivity = FALSE)
w <- calculate_benefits_reform(birth_yr = 2000, sex = "all", type = "medium",
                                age_claim = 65, factors = sef2025,
                                assumptions = reformed, debugg = TRUE)
# Extract and compare all 8 metrics against JSON values
# IMPORTANT: initial_real_benefit uses GDP PI deflator (not CPI):
#   gdp_pi_2025 <- reformed$gdp_pi[reformed$year == 2025]
#   gdp_pi_claim <- reformed$gdp_pi[reformed$year == claim_year]
#   initial_real_benefit <- monthly_benefit * gdp_pi_2025 / gdp_pi_claim
```

All 8 metrics must match: `monthly_benefit`, `pv_benefits`, `pv_taxes`, `ratio`, `irr`, `repl_rate`, `initial_real_benefit`, `death_age`.

#### Check 2 — Direction Checks

For each reform category:
- Benefit-cutting reforms (higher NRA, chained CPI, reduce_fact3) -> lower benefits
- Benefit-increasing reforms (CPI-E, flat_benefit for low earners) -> higher benefits
- Combos compound: `flat_benefit+chained_cpi` gives lower benefits than `flat_benefit` alone

#### Check 3 — AWI Indexing Check

For reforms with AWI-indexed parameters (currently: flat_benefit):
- Nominal values must increase across birth years
- The ratio between cohorts should approximate AWI growth

#### Check 4 — Phase-In Interpolation Check

For phased reforms (reduce_fact3: 10-year, flat_benefit: 25-year):
- Early cohorts should show partial reform effect (between current law and full reform)
- Verify intermediate values, not just endpoints:
  ```
  reduce_fact3, birth 1970 (elig 2032, 3/10 phased): fact3 ≈ 0.12
  reduce_fact3, birth 1975 (elig 2037, 8/10 phased): fact3 ≈ 0.07
  reduce_fact3, birth 1980 (elig 2042, fully phased): fact3 = 0.05
  ```

#### Check 5 — NMTR Sanity Checks

1. **Eligibility transition** (working year 10, i.e., age 30): NMTR should be strongly negative for all worker types (large benefit gain when QCs cross 40 = 10 years × 4 QCs/year)
2. **Years 36+**: For workers whose additional year doesn't enter top 35, NMTR ≈ 12.4% (pure tax, no marginal benefit)
3. **Benefit-cutting reforms**: NMTR should be higher than baseline (less benefit return per tax dollar)
4. **Benefit-increasing reforms**: NMTR should be lower than baseline

#### Check 6 — File Sizes

Expected sizes (±20%):
- Cohort: ~37 KB per worker type
- Individual: ~155 KB per worker type
- NMTR: ~490 KB per worker type

### Reform-Specific Verification Table

| Reform | Worker | Birth Year | Check |
|--------|--------|------------|-------|
| reduce_fact3 | very_low | any | Unchanged (PIA below bp2, fact3 irrelevant) |
| reduce_fact3 | max | 2010 | ~30% lower than current law (fully phased) |
| flat_benefit | very_low | 2010 | = flat_benefit amount (floor binds) |
| flat_benefit | max | 2010 | > flat_benefit (formula PIA still higher) |
| flat_benefit | any | 1980 vs 2010 | Increases proportionally to AWI |
| simpson_bowles | max | 2010 | 4th bracket (fact4=5%) active above bp3 |
| simpson_bowles | very_low | any | Minimal change (only 1st bracket matters) |
| nra_to_68 | any | 2010 | ~6-7% benefit cut vs NRA=67 (claiming 1 year early at NRA=68 ≈ 6.67% reduction) |
| index_nra | any | 2010 | Large reduction (NRA>69, claiming 4+ years early) |
| chained_cpi | any | 1970 vs 2010 | Larger % reduction for 2010 (more years of lower COLA) |
| cola_cap | very_low | any | Unchanged (PIA below median cap threshold) |
| cola_cap | max | any | Lower benefits (capped dollar COLA) |
| cpi_e | any | any | Higher benefits than baseline (more generous COLA) |

---

## Part 4: Known Issues and Gotchas

### `calculate_taxes()` Does Not Use `taxmax_tax`

The current `calculate_taxes()` function (analytic_functions.R:57-100) uses `taxmax` for the earnings cap in tax calculations. However, `reform_eliminate_taxmax_no_credit` sets a separate `taxmax_tax` parameter for unlimited taxation without benefit credit. **Before unlocking the taxmax reform category**, `calculate_taxes()` must be updated to use `taxmax_tax` when available, otherwise NMTRs will undercount taxes for earnings above the old taxmax.

### `basic_minimum_benefit()` Missing from NMTR Pipeline (Two-Layer Gap)

`compute_benefits_for_years()` in `marginal_benefit_analysis()` (analytic_functions.R:574-583) runs `aime_reform -> pia_reform -> cola_reform -> worker_benefit` but does NOT call `basic_minimum_benefit()`. The main pipeline in `calculate_benefits_reform()` does call it. Additionally, `compute_pv_to_year()` (analytic_functions.R:324-332) reconstructs the monthly benefit as `cola_basic_pia * act_factor` — it does NOT read `wrk_ben` (where the BMB supplement is added). So even if `basic_minimum_benefit()` were added to the NMTR pipeline, the PV computation would still miss the supplement.

**Before unlocking Reform #27 (basic minimum benefit) for data generation**, both layers must be fixed:
1. Add `basic_minimum_benefit()` call to `compute_benefits_for_years()`
2. Update `compute_pv_to_year()` to use `wrk_ben` instead of `cola_basic_pia * act_factor`

The BMB supplement for a very_low earner could be $200-400/month, which over 20+ years of benefits is a significant PV amount. Without this fix, NMTRs would be overstated for low earners.

### COLA Reforms DO Affect NMTRs

COLA adjustments affect the PV of lifetime benefits through the cumulative COLA factor in the benefit stream. Do NOT skip NMTR computation for COLA-only combos — they produce different NMTRs than non-COLA combos. The effect magnitude depends on years between working age and benefit receipt (larger for younger workers).

### Top-35-Year Replacement in NMTR

For workers with 35+ years of work, an additional year only increases AIME if its indexed earnings exceed the lowest of the current top 35. For years that don't enter the top 35, delta_pv = 0 and NMTR = 12.4% (or 6.2% employee-only). This is correct behavior, not a bug. Very_low and low workers are most likely to see this pattern at ages 55+.

### mclapply on macOS Uses Fork

`parallel::mclapply` on macOS uses `fork()`, meaning child processes share the parent's memory via copy-on-write. This is why storing 411 worker objects in the parent before forking is efficient — children read them without copying. Do NOT modify shared data in children (it triggers page faults and memory copies).

### Not Worth Doing

- **data.table migration**: Per-call overhead of dplyr vs data.table is ~0.1ms on 100-row data.frames. The bottleneck is call count, not per-call speed. The caching optimization (cutting calls by 49%) dwarfs any framework switch.
- **Incremental generation**: The combo structure means adding or changing any reform regenerates all combos. Use `--type` and `--categories` flags for scoped debug runs instead.
- **Different parallelism framework** (future/furrr): mclapply with mc.preschedule=TRUE and 6 cores provides adequate load balancing for 411 configs. No need to change.
