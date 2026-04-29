# ssmbar Dashboard Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a static GitHub Pages dashboard inside the new `ssmbar` package that displays current-law Social Security benefit data, recreating the visual identity of the old (`ssmbar_OLD`) dashboard while adapting to the new package's data dimensions.

**Architecture:** Static HTML/CSS/JS at `site/`. A single nested JSON file is generated at deploy time from `output/*.csv` via a small R build script. The `site/` contents are published by a GitHub Action to a `gh-pages` branch served by GitHub Pages.

**Tech Stack:** Bootstrap 5.3.3 + Bootstrap Icons + Inter font + Chart.js (all via CDN), vanilla JS (no framework). Build: R + `jsonlite`. Deploy: `r-lib/actions/setup-r@v2` + `peaceiris/actions-gh-pages@v3`.

**Spec:** `docs/superpowers/specs/2026-04-28-ssmbar-site-design.md`

**Reference (do not modify):** `/Users/anthony/ssmbar_OLD` — the old package containing `docs/index.html`, `docs/css/style.css`, `docs/js/*.js`, `docs/data/manifest.json`. Used as the source for ported CSS and as a visual baseline.

---

## File structure

**To create:**

| Path | Purpose | Approx LOC |
|---|---|---|
| `.Rbuildignore` | Exclude site/build/.github/docs from R package check | ~10 |
| `site/index.html` | Single-page dashboard | ~300 |
| `site/css/style.css` | Visual identity (ported byte-for-byte from `ssmbar_OLD`) | 860 |
| `site/js/app.js` | Entry point, tab/toggle wiring, top-level state | ~250 |
| `site/js/chart-manager.js` | Chart.js helpers, themed for old visual style | ~280 |
| `site/js/data-loader.js` | Fetches `data/site_data.json`, exposes `getConfig()` | ~80 |
| `site/js/ui-controls.js` | Worker/spouse/cohort/real-toggle controls | ~150 |
| `site/js/formatters.js` | Currency/percent formatters (copied as-is) | 64 |
| `site/js/table-manager.js` | Table render + CSV export (copied with minor edits) | ~140 |
| `build/build_site_data.R` | Reshapes `output/*.csv` to `site/data/site_data.json` | ~120 |
| `.github/workflows/deploy-site.yml` | Builds and publishes to `gh-pages` | ~40 |

**Not modified:** any existing R sources (`R/`), data (`data/`, `data-raw/`), or outputs (`output/`).

---

## Phase 1: Foundations & build script

### Task 1: Add .Rbuildignore and reserve `site/data/` path

**Files:**
- Create: `.Rbuildignore`
- Create: `site/data/.gitkeep` (empty placeholder so the directory exists in git)

- [ ] **Step 1: Create .Rbuildignore**

Write this content to `/Users/anthony/ssmbar/.Rbuildignore`:

```
^site$
^build$
^docs$
^\.github$
^\.Rbuildignore$
^\.gitignore$
^ssmbar\.Rproj$
^.*\.Rproj$
^\.Rproj\.user$
```

- [ ] **Step 2: Create site/data placeholder so directory tracks**

```bash
mkdir -p /Users/anthony/ssmbar/site/data
touch /Users/anthony/ssmbar/site/data/.gitkeep
```

- [ ] **Step 3: Update root .gitignore to exclude generated JSON**

Append to `/Users/anthony/ssmbar/.gitignore` (read first to confirm content; do not duplicate existing rules):

```
# Generated dashboard data (built by GitHub Action; never committed)
site/data/site_data.json
```

- [ ] **Step 4: Commit**

```bash
cd /Users/anthony/ssmbar
git add .Rbuildignore .gitignore site/data/.gitkeep
git commit -m "Reserve site/ structure and ignore generated dashboard data"
```

Expected: one commit; `git status` clean.

---

### Task 2: Write `build/build_site_data.R` — load CSVs and validate dimensions

**Files:**
- Create: `build/build_site_data.R`

- [ ] **Step 1: Create the build script with header, deps, and CSV loading**

Write `/Users/anthony/ssmbar/build/build_site_data.R`:

```r
# build_site_data.R
#
# Reshapes the four output CSVs into a single nested JSON file consumed by
# the static dashboard at site/. Run by the GitHub Action on every push.
#
# Inputs:  output/benefits_by_worker_age.csv
#          output/net_tax_on_earnings.csv
#          output/pv_lifetime_taxes_benefits.csv
#          output/initial_replacement_rates.csv
# Output:  site/data/site_data.json

suppressPackageStartupMessages({
  library(jsonlite)
})

WORKER_TYPES <- c("very_low", "low", "medium", "high", "max")
SPOUSE_TYPES <- c("none", "very_low", "low", "medium", "high", "max")
BIRTH_YEARS  <- seq(1940, 2010, by = 5)
CLAIM_AGE    <- 65L

WORKER_LABELS <- c(
  very_low = "Very Low Earner",
  low      = "Low Earner",
  medium   = "Medium Earner",
  high     = "High Earner",
  max      = "Maximum Earner"
)
SPOUSE_LABELS <- c(
  none     = "None (Single)",
  very_low = "Very Low Earner",
  low      = "Low Earner",
  medium   = "Medium Earner",
  high     = "High Earner",
  max      = "Maximum Earner"
)

# ---- Load --------------------------------------------------------------------
read_csv_strict <- function(path) {
  stopifnot(file.exists(path))
  read.csv(path, stringsAsFactors = FALSE)
}

ben_age   <- read_csv_strict("output/benefits_by_worker_age.csv")
nmtr      <- read_csv_strict("output/net_tax_on_earnings.csv")
pv        <- read_csv_strict("output/pv_lifetime_taxes_benefits.csv")
rep_rates <- read_csv_strict("output/initial_replacement_rates.csv")

# ---- Validate dimensions -----------------------------------------------------
expected_combos <- expand.grid(
  worker_type = WORKER_TYPES,
  spouse_type = SPOUSE_TYPES,
  birth_yr    = BIRTH_YEARS,
  stringsAsFactors = FALSE
)

assert_all_combos_present <- function(df, name) {
  uniq <- unique(df[c("worker_type", "spouse_type", "birth_yr")])
  uniq$claim_age <- NULL
  merged <- merge(expected_combos, uniq,
                  by = c("worker_type", "spouse_type", "birth_yr"),
                  all.x = TRUE)
  missing <- merged[!paste(merged$worker_type, merged$spouse_type, merged$birth_yr) %in%
                      paste(uniq$worker_type, uniq$spouse_type, uniq$birth_yr), ]
  if (nrow(missing) > 0) {
    stop(sprintf("[%s] missing %d configurations; first: %s|%s|%d",
                 name, nrow(missing),
                 missing$worker_type[1], missing$spouse_type[1], missing$birth_yr[1]),
         call. = FALSE)
  }
  invisible(NULL)
}

for (item in list(
  list(name = "benefits_by_worker_age", df = ben_age),
  list(name = "net_tax_on_earnings",     df = nmtr),
  list(name = "pv_lifetime_taxes_benefits", df = pv),
  list(name = "initial_replacement_rates", df = rep_rates)
)) {
  assert_all_combos_present(item$df, item$name)
}

# ---- Build configs map -------------------------------------------------------
configs <- list()

for (w in WORKER_TYPES) {
  for (s in SPOUSE_TYPES) {
    for (b in BIRTH_YEARS) {
      key <- paste(w, s, b, sep = "|")

      ba <- ben_age[ben_age$worker_type == w &
                    ben_age$spouse_type == s &
                    ben_age$birth_yr    == b &
                    ben_age$claim_age   == CLAIM_AGE, ]
      ba <- ba[order(ba$age), ]

      nm <- nmtr[nmtr$worker_type == w &
                 nmtr$spouse_type == s &
                 nmtr$birth_yr    == b &
                 nmtr$claim_age   == CLAIM_AGE, ]
      nm <- nm[order(nm$age), ]

      pvr <- pv[pv$worker_type == w &
                pv$spouse_type == s &
                pv$birth_yr    == b &
                pv$claim_age   == CLAIM_AGE, ]
      stopifnot(nrow(pvr) == 1)

      rr  <- rep_rates[rep_rates$worker_type == w &
                       rep_rates$spouse_type == s &
                       rep_rates$birth_yr    == b &
                       rep_rates$claim_age   == CLAIM_AGE, ]
      stopifnot(nrow(rr) == 1)

      ben_at_65 <- ba$real_ben[ba$age == 65]
      stopifnot(length(ben_at_65) == 1)

      configs[[key]] <- list(
        annual = list(
          ages     = ba$age,
          years    = ba$year,
          nominal  = round(ba$nominal_ben, 2),
          real     = round(ba$real_ben,    2),
          earnings = round(ba$earnings,    2)
        ),
        nmtr = list(
          ages   = nm$age,
          values = round(nm$net_tax, 6)
        ),
        summary = list(
          monthly_real_at_65 = round(ben_at_65 / 12, 2),
          pv_benefits        = round(pvr$pv_benefits, 2),
          pv_taxes           = round(pvr$pv_taxes,    2),
          ben_tax_ratio      = round(pvr$ben_tax_ratio, 4),
          rep_rate_career    = round(rr$rep_rate_career, 6),
          rep_rate_awi       = round(rr$rep_rate_awi,    6)
        )
      )
    }
  }
}

# ---- Write -------------------------------------------------------------------
out <- list(
  meta = list(
    data_mode  = "current_law_only",
    claim_age  = CLAIM_AGE,
    generated  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ),
  dimensions = list(
    worker_types = lapply(WORKER_TYPES, function(k) list(key = k, label = WORKER_LABELS[[k]])),
    spouse_types = lapply(SPOUSE_TYPES, function(k) list(key = k, label = SPOUSE_LABELS[[k]])),
    birth_years  = BIRTH_YEARS
  ),
  configs = configs
)

dir.create("site/data", recursive = TRUE, showWarnings = FALSE)
write_json(out, "site/data/site_data.json", auto_unbox = TRUE, digits = NA, na = "null")

cat(sprintf("Wrote site/data/site_data.json with %d configs\n", length(configs)))
```

- [ ] **Step 2: Run the build script and verify output**

```bash
cd /Users/anthony/ssmbar
Rscript build/build_site_data.R
```

Expected output: `Wrote site/data/site_data.json with 450 configs`

- [ ] **Step 3: Inspect the JSON shape**

```bash
python3 -c "import json; d=json.load(open('site/data/site_data.json')); print('configs:', len(d['configs'])); print('keys[0]:', list(d['configs'].keys())[0]); print('sample summary:', d['configs']['medium|none|1960']['summary']); print('annual ages length:', len(d['configs']['medium|none|1960']['annual']['ages'])); print('nmtr ages length:', len(d['configs']['medium|none|1960']['nmtr']['ages']))"
```

Expected:
- `configs: 450`
- `keys[0]: very_low|none|1940` (or similar — first cartesian-ordered key)
- `sample summary` shows numeric values for all 6 keys
- `annual ages length` and `nmtr ages length` both `> 0`

- [ ] **Step 4: Commit the build script**

```bash
git add build/build_site_data.R
git commit -m "Add build script reshaping output CSVs to nested JSON"
```

---

### Task 3: Port CSS from ssmbar_OLD byte-for-byte

**Files:**
- Create: `site/css/style.css`

- [ ] **Step 1: Copy CSS file**

```bash
cp /Users/anthony/ssmbar_OLD/docs/css/style.css /Users/anthony/ssmbar/site/css/style.css
```

- [ ] **Step 2: Verify line count matches expectations**

```bash
wc -l /Users/anthony/ssmbar/site/css/style.css
```

Expected: `860 /Users/anthony/ssmbar/site/css/style.css` (or close — exact byte-for-byte port).

- [ ] **Step 3: Commit**

```bash
git add site/css/style.css
git commit -m "Port style.css from ssmbar_OLD byte-for-byte"
```

---

### Task 4: Port `formatters.js` and `table-manager.js` (copy with minor edits)

**Files:**
- Create: `site/js/formatters.js` (copy as-is)
- Create: `site/js/table-manager.js` (copy with minor edits)

- [ ] **Step 1: Copy formatters.js as-is**

```bash
cp /Users/anthony/ssmbar_OLD/docs/js/formatters.js /Users/anthony/ssmbar/site/js/formatters.js
```

- [ ] **Step 2: Copy table-manager.js**

```bash
cp /Users/anthony/ssmbar_OLD/docs/js/table-manager.js /Users/anthony/ssmbar/site/js/table-manager.js
```

- [ ] **Step 3: Inspect table-manager.js for any references to dropped concepts**

```bash
grep -n "sex\|marital\|spouseType\|claim\|reform\|nmtr\|NMTR" /Users/anthony/ssmbar/site/js/table-manager.js
```

Read each match. If a match references rendering reform/NMTR comparison columns, add it to the next step's edit list. If matches are just inside generic helpers (e.g., `comboKey` building), they can stay if harmless.

- [ ] **Step 4: Apply minimal edits**

Use Edit tool to remove or simplify any code paths the inspection found that depend on dropped concepts (sex, claim age, reform comparison columns). For columns the new app doesn't surface (e.g., reform deltas), delete those branches; do not leave dead code.

- [ ] **Step 5: Commit**

```bash
git add site/js/formatters.js site/js/table-manager.js
git commit -m "Port formatters.js and table-manager.js from ssmbar_OLD"
```

---

## Phase 2: HTML scaffold

### Task 5: Create `site/index.html` skeleton

**Files:**
- Create: `site/index.html`

- [ ] **Step 1: Write index.html scaffold**

Write `/Users/anthony/ssmbar/site/index.html`:

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Social Security Benefit Explorer</title>
  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css" rel="stylesheet">
  <link href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css" rel="stylesheet">
  <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">
  <link href="css/style.css" rel="stylesheet">
</head>
<body>

<!-- Navbar -->
<nav class="navbar navbar-dark">
  <div class="container-fluid d-flex justify-content-between align-items-center">
    <a class="navbar-brand" href="#">Social Security Benefit Explorer</a>
    <span class="navbar-attr">ssmbar | 2025 Trustees Report</span>
  </div>
</nav>

<div class="app-container container-fluid">

  <!-- Tabs -->
  <ul class="nav nav-tabs" role="tablist">
    <li class="nav-item">
      <button class="nav-link active" id="tab-individual" data-tab="individual" type="button">Individual Worker</button>
    </li>
    <li class="nav-item">
      <button class="nav-link" id="tab-cohort" data-tab="cohort" type="button">Cohort Comparison</button>
    </li>
  </ul>

  <!-- Worker config row (always visible above both tabs) -->
  <section class="config-row">
    <div class="config-control">
      <label class="config-label">Worker Type</label>
      <div id="workerTypeControl" class="btn-group btn-group-sm" role="group" aria-label="Worker type"></div>
    </div>
    <div class="config-control">
      <label for="spouseControl" class="config-label">Spouse</label>
      <select id="spouseControl" class="form-select form-select-sm"></select>
    </div>
    <div class="config-control">
      <label for="cohortControl" class="config-label">Birth Cohort</label>
      <select id="cohortControl" class="form-select form-select-sm"></select>
    </div>
    <div class="config-control">
      <label class="config-label">Dollars</label>
      <div id="realToggleControl" class="btn-group btn-group-sm" role="group" aria-label="Real or nominal">
        <button type="button" class="btn btn-outline-primary active" data-mode="real">Real</button>
        <button type="button" class="btn btn-outline-primary" data-mode="nominal">Nominal</button>
      </div>
    </div>
  </section>

  <!-- Individual tab -->
  <section id="panel-individual" class="tab-panel active">
    <!-- Summary cards -->
    <div class="summary-grid" id="summaryCards"></div>

    <!-- Charts -->
    <div class="chart-section">
      <h2 class="chart-title">Annual Benefits by Age</h2>
      <div class="chart-canvas-wrap"><canvas id="annualBenefitsChart"></canvas></div>
    </div>

    <div class="chart-section">
      <h2 class="chart-title">Net Tax Rate by Age</h2>
      <div class="chart-canvas-wrap"><canvas id="netTaxRateChart"></canvas></div>
    </div>

    <!-- Data table -->
    <div class="table-section">
      <div class="table-header">
        <h2 class="chart-title">Year-by-Year Detail</h2>
        <button id="downloadCsvBtn" class="btn btn-sm btn-outline-secondary">
          <i class="bi bi-download"></i> Download CSV
        </button>
      </div>
      <div class="table-wrap">
        <table id="detailTable" class="table table-sm table-hover"></table>
      </div>
    </div>
  </section>

  <!-- Cohort tab -->
  <section id="panel-cohort" class="tab-panel" hidden>
    <div class="cohort-grid">
      <div class="chart-section">
        <h2 class="chart-title">Monthly Benefit at 65 (Real $)</h2>
        <div class="chart-canvas-wrap"><canvas id="cohortMonthlyChart"></canvas></div>
      </div>
      <div class="chart-section">
        <h2 class="chart-title">Replacement Rate (Career)</h2>
        <div class="chart-subtitle">Initial benefit as a share of the worker's average real career earnings.</div>
        <div class="chart-canvas-wrap"><canvas id="cohortRrCareerChart"></canvas></div>
      </div>
      <div class="chart-section">
        <h2 class="chart-title">Replacement Rate (AWI)</h2>
        <div class="chart-subtitle">Initial benefit as a share of that year's average wage index.</div>
        <div class="chart-canvas-wrap"><canvas id="cohortRrAwiChart"></canvas></div>
      </div>
      <div class="chart-section">
        <h2 class="chart-title">PV Lifetime Benefits</h2>
        <div class="chart-canvas-wrap"><canvas id="cohortPvBenChart"></canvas></div>
      </div>
      <div class="chart-section">
        <h2 class="chart-title">Benefit / Tax Ratio</h2>
        <div class="chart-canvas-wrap"><canvas id="cohortRatioChart"></canvas></div>
      </div>
    </div>
  </section>

  <!-- Reform placeholder -->
  <aside class="reform-placeholder">
    <i class="bi bi-info-circle"></i>
    Reforms not yet supported in this version. The new ssmbar package computes
    current-law benefits only; reform comparisons may be added in a future release.
  </aside>

</div>

<!-- Libraries -->
<script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js"></script>

<!-- App scripts -->
<script src="js/formatters.js"></script>
<script src="js/data-loader.js"></script>
<script src="js/chart-manager.js"></script>
<script src="js/table-manager.js"></script>
<script src="js/ui-controls.js"></script>
<script src="js/app.js"></script>

</body>
</html>
```

- [ ] **Step 2: Commit**

```bash
git add site/index.html
git commit -m "Add index.html scaffold with controls, tabs, charts, placeholder"
```

---

### Task 6: Add CSS rules for new layout pieces not in old style.css

**Files:**
- Modify: `site/css/style.css` (append at end)

The old CSS targets a sidebar+main layout and is designed for old element IDs. Some of the new IDs (e.g., `summaryCards`, `cohortMonthlyChart`, `reform-placeholder`) need styles. Add a small additive section at the bottom that does not contradict existing rules.

- [ ] **Step 1: Inspect what selectors already exist in style.css for the elements we use**

```bash
grep -n "summary-grid\|summary-card\|chart-section\|cohort-grid\|reform-placeholder\|config-row\|config-control\|tab-panel\|chart-subtitle" /Users/anthony/ssmbar/site/css/style.css | head -40
```

Note which selectors are NOT yet present. They will be added in the next step.

- [ ] **Step 2: Append additive styles**

Append to end of `/Users/anthony/ssmbar/site/css/style.css` the rules needed for any selectors that step 1 reported as missing. Match the existing palette (light theme, navy `#0f1741`, blue `#2563EB`, muted greys, `Inter` font). Example additions (only those missing):

```css
/* === New layout additions (current-law dashboard) ============================ */

.app-container { padding: 24px; max-width: 1200px; margin: 0 auto; }

.config-row {
  display: flex; gap: 24px; flex-wrap: wrap;
  padding: 16px 0; border-bottom: 1px solid rgba(0,0,0,0.06);
  margin-bottom: 24px;
}
.config-control { display: flex; flex-direction: column; gap: 6px; min-width: 160px; }
.config-label { font-size: 12px; color: #6b7280; text-transform: uppercase; letter-spacing: 0.04em; }

.tab-panel[hidden] { display: none; }
.tab-panel.active { display: block; }
.nav-tabs { margin-bottom: 0; }

.summary-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
  gap: 16px;
  margin-bottom: 24px;
}
.summary-card {
  background: #fff; border: 1px solid rgba(0,0,0,0.06); border-radius: 10px;
  padding: 14px 16px; box-shadow: 0 1px 2px rgba(0,0,0,0.03);
}
.summary-card .summary-label { font-size: 12px; color: #6b7280; text-transform: uppercase; letter-spacing: 0.04em; }
.summary-card .summary-value { font-size: 22px; font-weight: 600; color: #0f1741; margin-top: 4px; }
.summary-card .summary-subtitle {
  font-size: 12px; color: #6b7280; font-style: italic; margin-top: 6px; line-height: 1.35;
}

.chart-section { margin-bottom: 32px; }
.chart-title { font-size: 16px; font-weight: 600; color: #0f1741; margin-bottom: 8px; }
.chart-subtitle { font-size: 12px; color: #6b7280; font-style: italic; margin-bottom: 8px; }
.chart-canvas-wrap { position: relative; height: 320px; }

.cohort-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(360px, 1fr));
  gap: 24px;
}
.cohort-grid .chart-canvas-wrap { height: 240px; }

.table-section { margin-bottom: 32px; }
.table-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px; }
.table-wrap { max-height: 360px; overflow-y: auto; border: 1px solid rgba(0,0,0,0.06); border-radius: 8px; }

.reform-placeholder {
  display: flex; gap: 10px; align-items: flex-start;
  padding: 14px 16px; margin: 32px 0 8px;
  background: #f3f4f6; border-radius: 8px; color: #4b5563;
  font-size: 13px;
}
.reform-placeholder .bi { font-size: 18px; color: #6b7280; flex-shrink: 0; }
```

Only add the rules whose selectors were missing in step 1. Skip any that would duplicate existing rules.

- [ ] **Step 3: Commit**

```bash
git add site/css/style.css
git commit -m "Add CSS for new layout sections (summary cards, cohort grid, placeholder)"
```

---

## Phase 3: JS infrastructure

### Task 7: Write `data-loader.js` — fetch JSON, expose `getConfig()`

**Files:**
- Create: `site/js/data-loader.js`

- [ ] **Step 1: Write data-loader.js**

```javascript
// =============================================================================
// DataLoader — Fetches site_data.json and exposes config lookups.
// =============================================================================

const dataLoader = (() => {
  let payload = null;

  async function init() {
    const resp = await fetch('data/site_data.json', { cache: 'no-store' });
    if (!resp.ok) throw new Error(`Failed to load site_data.json: ${resp.status}`);
    payload = await resp.json();
  }

  function ready() { return payload != null; }

  function meta() { return payload.meta; }

  function dimensions() { return payload.dimensions; }

  function configKey(workerType, spouseType, birthYear) {
    return `${workerType}|${spouseType}|${birthYear}`;
  }

  function getConfig(workerType, spouseType, birthYear) {
    const key = configKey(workerType, spouseType, birthYear);
    const cfg = payload.configs[key];
    if (!cfg) throw new Error(`No data for config: ${key}`);
    return cfg;
  }

  function getCohortSeries(workerType, spouseType, summaryField) {
    const years = payload.dimensions.birth_years;
    const values = years.map(y => {
      const cfg = payload.configs[configKey(workerType, spouseType, y)];
      return cfg ? cfg.summary[summaryField] : null;
    });
    return { years, values };
  }

  return { init, ready, meta, dimensions, getConfig, getCohortSeries };
})();
```

- [ ] **Step 2: Commit**

```bash
git add site/js/data-loader.js
git commit -m "Add data-loader.js with config and cohort accessors"
```

---

### Task 8: Write `ui-controls.js` — populate and react to config controls

**Files:**
- Create: `site/js/ui-controls.js`

- [ ] **Step 1: Write ui-controls.js**

```javascript
// =============================================================================
// UIControls — Populates worker/spouse/cohort/real controls and emits state.
// =============================================================================

const uiControls = (() => {
  const state = {
    workerType: 'medium',
    spouseType: 'none',
    birthYear:  1960,
    real:       true
  };

  const listeners = new Set();
  function onChange(fn) { listeners.add(fn); }
  function emit() { listeners.forEach(fn => fn({ ...state })); }

  function buildWorkerTypeControl(types) {
    const host = document.getElementById('workerTypeControl');
    host.innerHTML = '';
    types.forEach(t => {
      const btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'btn btn-outline-primary';
      btn.dataset.value = t.key;
      btn.textContent = t.label.replace(' Earner', '');
      if (t.key === state.workerType) btn.classList.add('active');
      btn.addEventListener('click', () => {
        state.workerType = t.key;
        host.querySelectorAll('button').forEach(b => b.classList.toggle('active', b.dataset.value === t.key));
        emit();
      });
      host.appendChild(btn);
    });
  }

  function buildSpouseControl(spouses) {
    const sel = document.getElementById('spouseControl');
    sel.innerHTML = '';
    spouses.forEach(s => {
      const opt = document.createElement('option');
      opt.value = s.key;
      opt.textContent = s.label;
      if (s.key === state.spouseType) opt.selected = true;
      sel.appendChild(opt);
    });
    sel.addEventListener('change', () => {
      state.spouseType = sel.value;
      emit();
    });
  }

  function buildCohortControl(years) {
    const sel = document.getElementById('cohortControl');
    sel.innerHTML = '';
    years.forEach(y => {
      const opt = document.createElement('option');
      opt.value = String(y);
      opt.textContent = String(y);
      if (y === state.birthYear) opt.selected = true;
      sel.appendChild(opt);
    });
    sel.addEventListener('change', () => {
      state.birthYear = parseInt(sel.value, 10);
      emit();
    });
  }

  function buildRealToggle() {
    const host = document.getElementById('realToggleControl');
    host.querySelectorAll('button').forEach(btn => {
      btn.addEventListener('click', () => {
        const isReal = btn.dataset.mode === 'real';
        if (isReal === state.real) return;
        state.real = isReal;
        host.querySelectorAll('button').forEach(b => b.classList.toggle('active', b.dataset.mode === btn.dataset.mode));
        emit();
      });
    });
  }

  function buildTabs(onTab) {
    document.querySelectorAll('.nav-tabs .nav-link').forEach(btn => {
      btn.addEventListener('click', () => {
        const tab = btn.dataset.tab;
        document.querySelectorAll('.nav-tabs .nav-link').forEach(b => b.classList.toggle('active', b === btn));
        document.querySelectorAll('.tab-panel').forEach(p => {
          const isTarget = p.id === `panel-${tab}`;
          p.hidden = !isTarget;
          p.classList.toggle('active', isTarget);
        });
        onTab(tab);
      });
    });
  }

  function init({ dimensions, onTab }) {
    buildWorkerTypeControl(dimensions.worker_types);
    buildSpouseControl(dimensions.spouse_types);
    buildCohortControl(dimensions.birth_years);
    buildRealToggle();
    buildTabs(onTab);
  }

  function getState() { return { ...state }; }

  return { init, onChange, getState };
})();
```

- [ ] **Step 2: Commit**

```bash
git add site/js/ui-controls.js
git commit -m "Add ui-controls.js for worker/spouse/cohort/real-toggle and tabs"
```

---

### Task 9: Port `chart-manager.js` — strip reform/sex paths, rename NMTR

**Files:**
- Create: `site/js/chart-manager.js` (copy from ssmbar_OLD then trim)

- [ ] **Step 1: Copy chart-manager.js as starting point**

```bash
cp /Users/anthony/ssmbar_OLD/docs/js/chart-manager.js /Users/anthony/ssmbar/site/js/chart-manager.js
```

- [ ] **Step 2: Identify dropped paths to remove**

```bash
grep -n "reform\|Reform\|nmtrAccrual\|nmtrTax\|sex\|marital" /Users/anthony/ssmbar/site/js/chart-manager.js
```

Read the matches. Code paths whose only purpose is reform overlay or accrual/tax stacking can be deleted.

- [ ] **Step 3: Rewrite chart-manager.js to a focused current-law version**

Replace the file's contents with:

```javascript
// =============================================================================
// ChartManager — Chart.js helpers (light theme, current-law only)
// =============================================================================

const CHART_COLORS = {
  line:   '#2563EB',
  fill:   'rgba(37, 99, 235, 0.08)',
  muted:  '#9ca3af',
  grid:   'rgba(0, 0, 0, 0.06)',
  axis:   '#6b7280'
};

const CHART_DEFAULTS = {
  responsive: true,
  maintainAspectRatio: false,
  animation: { duration: 350, easing: 'easeOutQuart' },
  plugins: {
    legend: { display: false },
    tooltip: {
      backgroundColor: '#0f1741',
      titleColor: '#e8e8e8',
      bodyColor:  '#e8e8e8',
      borderColor: '#2a3f5f',
      borderWidth: 1,
      cornerRadius: 6,
      padding: 10,
      bodyFont:  { family: 'Inter', size: 12 },
      titleFont: { family: 'Inter', size: 12, weight: 'bold' }
    }
  },
  scales: {
    x: {
      ticks: { color: CHART_COLORS.axis, font: { family: 'Inter', size: 11 } },
      grid:  { color: CHART_COLORS.grid }
    },
    y: {
      ticks: { color: CHART_COLORS.axis, font: { family: 'Inter', size: 11 } },
      grid:  { color: CHART_COLORS.grid }
    }
  }
};

const chartManager = (() => {
  const charts = {};

  function lineChart(canvasId, { labels, data, yFormat = 'currency', yMin = null, yMax = null }) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    if (charts[canvasId]) charts[canvasId].destroy();

    const opts = JSON.parse(JSON.stringify(CHART_DEFAULTS));
    if (yMin != null) opts.scales.y.min = yMin;
    if (yMax != null) opts.scales.y.max = yMax;
    opts.scales.y.ticks.callback = (v) => formatYTick(v, yFormat);
    opts.plugins.tooltip.callbacks = {
      label: (item) => formatYTick(item.parsed.y, yFormat)
    };

    charts[canvasId] = new Chart(ctx, {
      type: 'line',
      data: {
        labels,
        datasets: [{
          data,
          borderColor: CHART_COLORS.line,
          backgroundColor: CHART_COLORS.fill,
          fill: true,
          tension: 0.25,
          pointRadius: 0,
          pointHoverRadius: 4,
          borderWidth: 2
        }]
      },
      options: opts
    });
  }

  function barChart(canvasId, { labels, data, yFormat = 'percent' }) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    if (charts[canvasId]) charts[canvasId].destroy();

    const opts = JSON.parse(JSON.stringify(CHART_DEFAULTS));
    opts.scales.y.ticks.callback = (v) => formatYTick(v, yFormat);
    opts.plugins.tooltip.callbacks = {
      label: (item) => formatYTick(item.parsed.y, yFormat)
    };

    charts[canvasId] = new Chart(ctx, {
      type: 'bar',
      data: {
        labels,
        datasets: [{
          data,
          backgroundColor: CHART_COLORS.line,
          borderRadius: 2
        }]
      },
      options: opts
    });
  }

  function formatYTick(v, fmt) {
    if (v == null) return '';
    if (fmt === 'currency') return formatters.currency(v);
    if (fmt === 'percent')  return formatters.percent(v);
    return String(v);
  }

  function destroyAll() {
    Object.values(charts).forEach(c => c.destroy());
    Object.keys(charts).forEach(k => delete charts[k]);
  }

  return { lineChart, barChart, destroyAll };
})();
```

- [ ] **Step 4: Verify formatters.js exports `currency` and `percent` with the right signatures**

```bash
grep -n "currency\|percent" /Users/anthony/ssmbar/site/js/formatters.js
```

If the global is `formatters.currency(value)` and `formatters.percent(value)` — good. Otherwise adjust the `formatYTick` calls to match what formatters.js exposes.

- [ ] **Step 5: Commit**

```bash
git add site/js/chart-manager.js
git commit -m "Port chart-manager.js stripped to current-law line/bar helpers"
```

---

## Phase 4: Individual tab — wire data and charts

### Task 10: Write `app.js` — init, summary cards, table, individual charts

**Files:**
- Create: `site/js/app.js`

- [ ] **Step 1: Write app.js**

```javascript
// =============================================================================
// App — Entry point. Wires data, controls, charts, summary cards, table.
// =============================================================================

document.addEventListener('DOMContentLoaded', async () => {
  try {
    await dataLoader.init();
  } catch (err) {
    console.error('Failed to load data:', err);
    showLoadError(err);
    return;
  }

  uiControls.init({
    dimensions: dataLoader.dimensions(),
    onTab: handleTabChange
  });
  uiControls.onChange(state => render(state));
  render(uiControls.getState());
});

// -----------------------------------------------------------------------------
// Render dispatch
// -----------------------------------------------------------------------------

function render(state) {
  const cfg = dataLoader.getConfig(state.workerType, state.spouseType, state.birthYear);
  renderSummaryCards(cfg, state.real);
  renderAnnualBenefitsChart(cfg, state.real);
  renderNetTaxRateChart(cfg);
  tableManager.render(cfg, state.real);

  if (!document.getElementById('panel-cohort').hidden) {
    renderCohortCharts(state);
  }
}

function handleTabChange(tab) {
  const state = uiControls.getState();
  if (tab === 'cohort') renderCohortCharts(state);
  // individual already rendered; no-op
}

// -----------------------------------------------------------------------------
// Summary cards
// -----------------------------------------------------------------------------

function renderSummaryCards(cfg, real) {
  const monthly = cfg.summary.monthly_real_at_65;
  const cards = [
    { label: 'Monthly Benefit at 65',  value: formatters.currency(monthly), subtitle: '(real $, age 65)' },
    { label: 'PV Lifetime Benefits',   value: formatters.currency(cfg.summary.pv_benefits) },
    { label: 'PV Lifetime Taxes',      value: formatters.currency(cfg.summary.pv_taxes) },
    { label: 'Benefit / Tax Ratio',    value: cfg.summary.ben_tax_ratio.toFixed(2) },
    { label: 'Replacement Rate (Career)',
      value: formatters.percent(cfg.summary.rep_rate_career),
      subtitle: "Initial benefit as a share of the worker's average real career earnings." },
    { label: 'Replacement Rate (AWI)',
      value: formatters.percent(cfg.summary.rep_rate_awi),
      subtitle: "Initial benefit as a share of that year's average wage index." }
  ];

  const host = document.getElementById('summaryCards');
  host.innerHTML = cards.map(c => `
    <div class="summary-card">
      <div class="summary-label">${c.label}</div>
      <div class="summary-value">${c.value}</div>
      ${c.subtitle ? `<div class="summary-subtitle">${c.subtitle}</div>` : ''}
    </div>
  `).join('');
}

// -----------------------------------------------------------------------------
// Individual charts
// -----------------------------------------------------------------------------

function renderAnnualBenefitsChart(cfg, real) {
  const data = real ? cfg.annual.real : cfg.annual.nominal;
  chartManager.lineChart('annualBenefitsChart', {
    labels: cfg.annual.ages,
    data,
    yFormat: 'currency'
  });
}

function renderNetTaxRateChart(cfg) {
  chartManager.lineChart('netTaxRateChart', {
    labels: cfg.nmtr.ages,
    data:   cfg.nmtr.values,
    yFormat: 'percent',
    yMin: -0.10,
    yMax: 0.25
  });
}

// -----------------------------------------------------------------------------
// Cohort charts
// -----------------------------------------------------------------------------

function renderCohortCharts(state) {
  const w = state.workerType, s = state.spouseType;

  const monthly = dataLoader.getCohortSeries(w, s, 'monthly_real_at_65');
  chartManager.barChart('cohortMonthlyChart', {
    labels: monthly.years, data: monthly.values, yFormat: 'currency'
  });

  const rrCareer = dataLoader.getCohortSeries(w, s, 'rep_rate_career');
  chartManager.barChart('cohortRrCareerChart', {
    labels: rrCareer.years, data: rrCareer.values, yFormat: 'percent'
  });

  const rrAwi = dataLoader.getCohortSeries(w, s, 'rep_rate_awi');
  chartManager.barChart('cohortRrAwiChart', {
    labels: rrAwi.years, data: rrAwi.values, yFormat: 'percent'
  });

  const pvBen = dataLoader.getCohortSeries(w, s, 'pv_benefits');
  chartManager.barChart('cohortPvBenChart', {
    labels: pvBen.years, data: pvBen.values, yFormat: 'currency'
  });

  const ratio = dataLoader.getCohortSeries(w, s, 'ben_tax_ratio');
  chartManager.barChart('cohortRatioChart', {
    labels: ratio.years, data: ratio.values
  });
}

// -----------------------------------------------------------------------------
// Errors
// -----------------------------------------------------------------------------

function showLoadError(err) {
  const host = document.querySelector('.app-container');
  host.innerHTML = `<div class="alert alert-danger m-4">Failed to load dashboard data: ${err.message}</div>`;
}
```

- [ ] **Step 2: Verify formatters.js exposes the API used here**

```bash
grep -nE "(currency|percent)\s*[:=(]" /Users/anthony/ssmbar/site/js/formatters.js
```

Expected: a global object `formatters` with `currency(v)` and `percent(v)` methods (or equivalent function exports). If the API differs, adjust the calls in app.js or formatters.js accordingly. Document the shape used.

- [ ] **Step 3: Commit**

```bash
git add site/js/app.js
git commit -m "Add app.js wiring controls, summary cards, charts, table"
```

---

### Task 11: Adjust `table-manager.js` to consume the new config shape

**Files:**
- Modify: `site/js/table-manager.js`

The old table-manager.js consumed a flat per-row data structure. The new one receives a single `cfg` object plus a `real` flag. Replace the `render` and CSV-export logic accordingly while keeping the styling/DOM hooks the same.

- [ ] **Step 1: Read the current file to see what its `render` signature looks like**

```bash
head -80 /Users/anthony/ssmbar/site/js/table-manager.js
```

- [ ] **Step 2: Replace the file with a clean, minimal version**

Write `/Users/anthony/ssmbar/site/js/table-manager.js`:

```javascript
// =============================================================================
// TableManager — Renders the year-by-year detail table and CSV export.
// =============================================================================

const tableManager = (() => {
  let lastRows = [];
  let lastFilenameHint = '';

  function render(cfg, real) {
    const rows = cfg.annual.ages.map((age, i) => ({
      age,
      year:        cfg.annual.years[i],
      earnings:    cfg.annual.earnings[i],
      nominal_ben: cfg.annual.nominal[i],
      real_ben:    cfg.annual.real[i]
    }));
    lastRows = rows;

    const tbl = document.getElementById('detailTable');
    tbl.innerHTML = `
      <thead>
        <tr>
          <th>Age</th>
          <th>Year</th>
          <th>Earnings</th>
          <th>Nominal Benefit</th>
          <th>Real Benefit</th>
        </tr>
      </thead>
      <tbody>
        ${rows.map(r => `
          <tr>
            <td>${r.age}</td>
            <td>${r.year}</td>
            <td class="text-end">${formatters.currency(r.earnings)}</td>
            <td class="text-end">${formatters.currency(r.nominal_ben)}</td>
            <td class="text-end">${formatters.currency(r.real_ben)}</td>
          </tr>
        `).join('')}
      </tbody>
    `;

    document.getElementById('downloadCsvBtn').onclick = () => downloadCsv(rows);
  }

  function downloadCsv(rows) {
    const header = ['age', 'year', 'earnings', 'nominal_ben', 'real_ben'];
    const lines = [header.join(',')].concat(
      rows.map(r => [r.age, r.year, r.earnings, r.nominal_ben, r.real_ben].join(','))
    );
    const blob = new Blob([lines.join('\n')], { type: 'text/csv;charset=utf-8' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'ssmbar_annual_detail.csv';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  }

  return { render };
})();
```

- [ ] **Step 3: Commit**

```bash
git add site/js/table-manager.js
git commit -m "Rewrite table-manager.js for the new per-config data shape"
```

---

### Task 12: First local smoke test

- [ ] **Step 1: Ensure JSON is built**

```bash
cd /Users/anthony/ssmbar
Rscript build/build_site_data.R
```

Expected: `Wrote site/data/site_data.json with 450 configs`

- [ ] **Step 2: Serve the site locally**

```bash
cd /Users/anthony/ssmbar/site
python3 -m http.server 8080
```

(Run in a background shell so the next steps can continue.)

- [ ] **Step 3: Open the site in a browser and walk through this checklist**

Open `http://localhost:8080/`. Observe and verify:

- [ ] Page loads with no console errors (DevTools → Console).
- [ ] Default state visible: Worker = Medium (highlighted), Spouse = "None (Single)", Cohort = 1960, Dollars = Real.
- [ ] All 6 summary cards show numeric values, none are `NaN` or `undefined`.
- [ ] Annual Benefits by Age chart renders a line.
- [ ] Net Tax Rate by Age chart renders a line.
- [ ] Year-by-year table is populated.
- [ ] Click each Worker Type — chart and cards update each time.
- [ ] Change Spouse dropdown — cards/chart update.
- [ ] Change Birth Cohort — cards/chart update.
- [ ] Toggle Nominal — Annual Benefits chart redraws.
- [ ] Click "Cohort Comparison" tab — 5 small charts render with x-axis 1940→2010.
- [ ] Click "Download CSV" — file downloads, contents match table.
- [ ] Reform placeholder card visible at bottom of page.

- [ ] **Step 4: Stop the local server**

(Kill the background process started in Step 2.)

- [ ] **Step 5: Fix any issues found**

If anything fails, fix the relevant file (most likely `app.js`, `chart-manager.js`, or `index.html`) and re-test. Repeat until all checklist items pass.

- [ ] **Step 6: Commit any fixes from Step 5**

(Only if files changed during Step 5.)

```bash
git add -u
git commit -m "Fix issues found during first smoke test"
```

---

## Phase 5: Visual parity check

### Task 13: Side-by-side visual check against ssmbar_OLD

- [ ] **Step 1: Serve ssmbar_OLD's docs alongside in another tab**

```bash
cd /Users/anthony/ssmbar_OLD/docs
python3 -m http.server 8081
```

- [ ] **Step 2: Open both sites side-by-side in the browser**

`http://localhost:8080/` (new) and `http://localhost:8081/` (old). Compare:

- [ ] Navbar styling (background, text, "ssmbar | 2025 Trustees Report" attribution)
- [ ] Tab styling (active/inactive states)
- [ ] Card styling (border, shadow, label/value typography)
- [ ] Chart theming (line color, grid color, tooltip dark theme, axis label font)
- [ ] Overall typography (Inter, weights, sizes)
- [ ] Spacing/padding around major sections

- [ ] **Step 3: Note any visual deltas**

For each delta, decide whether it is acceptable (the new layout has different content, so some difference is expected) or a regression (e.g., wrong color, missing typography). Fix regressions in `site/css/style.css` (additive, do not modify the byte-for-byte ported core rules) or in `site/index.html`.

- [ ] **Step 4: Stop both servers**

- [ ] **Step 5: Commit any visual fixes**

```bash
git add -u
git commit -m "Visual parity tweaks against ssmbar_OLD"
```

---

## Phase 6: Deploy

### Task 14: Write GitHub Action workflow

**Files:**
- Create: `.github/workflows/deploy-site.yml`

- [ ] **Step 1: Create the workflow file**

Write `/Users/anthony/ssmbar/.github/workflows/deploy-site.yml`:

```yaml
name: Deploy site to GitHub Pages

on:
  push:
    branches: [main]
    paths:
      - 'R/**'
      - 'output/**'
      - 'site/**'
      - 'build/**'
      - '.github/workflows/deploy-site.yml'
  workflow_dispatch:

permissions:
  contents: write

jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Set up R
        uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'

      - name: Install jsonlite
        run: Rscript -e 'install.packages("jsonlite", repos = "https://cloud.r-project.org")'

      - name: Build site data
        run: Rscript build/build_site_data.R

      - name: Verify JSON exists
        run: |
          test -f site/data/site_data.json
          echo "site_data.json size: $(wc -c < site/data/site_data.json) bytes"

      - name: Publish to gh-pages
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./site
          publish_branch: gh-pages
          force_orphan: true
          user_name: 'github-actions[bot]'
          user_email: 'github-actions[bot]@users.noreply.github.com'
          commit_message: 'Deploy site from ${{ github.sha }}'
```

- [ ] **Step 2: Commit**

```bash
git add .github/workflows/deploy-site.yml
git commit -m "Add deploy workflow building JSON and publishing to gh-pages"
```

---

### Task 15: Configure GitHub Pages and Action permissions via gh CLI

- [ ] **Step 1: Confirm Actions write permission on the repo**

```bash
gh api repos/anthonycolavito/ssmbar/actions/permissions/workflow
```

If `default_workflow_permissions` is `read`, change it:

```bash
gh api -X PUT repos/anthonycolavito/ssmbar/actions/permissions/workflow \
  -f default_workflow_permissions=write \
  -F can_approve_pull_request_reviews=true
```

- [ ] **Step 2: Push the workflow to trigger the first run**

```bash
git push origin main
```

- [ ] **Step 3: Watch the Action run**

```bash
gh run list --workflow=deploy-site.yml --limit 1
gh run watch
```

Expected: green run that creates a new `gh-pages` branch.

- [ ] **Step 4: Configure Pages source**

```bash
gh api -X POST repos/anthonycolavito/ssmbar/pages \
  -f source[branch]=gh-pages \
  -f source[path]=/ || \
gh api -X PUT repos/anthonycolavito/ssmbar/pages \
  -f source[branch]=gh-pages \
  -f source[path]=/
```

(POST creates Pages config the first time; PUT updates an existing config. Run whichever applies.)

- [ ] **Step 5: Verify Pages config**

```bash
gh api repos/anthonycolavito/ssmbar/pages
```

Expected: `source.branch == "gh-pages"`, `status` becomes `built` after a few seconds.

---

### Task 16: Live verification

- [ ] **Step 1: Wait until Pages reports built**

```bash
until [ "$(gh api repos/anthonycolavito/ssmbar/pages --jq .status)" = "built" ]; do sleep 5; done
echo "Pages built."
```

- [ ] **Step 2: Open the live URL**

```bash
open https://anthonycolavito.github.io/ssmbar/
```

- [ ] **Step 3: Run the same smoke checklist from Task 12 but against the live URL**

(Same items: default state, control changes, chart updates, tab switching, CSV download, no console errors.)

- [ ] **Step 4: Cross-config spot check**

Pick three configs at random (e.g., `very_low|none|1940`, `medium|high|1980`, `max|max|2010`). For each, compare the summary card values against the source CSV rows:

```bash
grep "^very_low,none,1940,65," /Users/anthony/ssmbar/output/initial_replacement_rates.csv
```

Verify the displayed `Replacement Rate (Career)` matches `rep_rate_career` from the CSV (formatted as a percent). Repeat for the other two configs. Any mismatch is a bug.

- [ ] **Step 5: If any issue is found**

Diagnose, fix, commit, push, wait for the new Action run, re-verify. Repeat until clean.

- [ ] **Step 6: Update README pointer (optional but useful)**

If a README exists at the repo root, add a small "Dashboard" section with a link to the live URL. If none exists, skip.

---

## Out-of-scope safety net

The following are NOT in this plan and should not be added unless the spec is updated:

- IRR computation
- Sex dimension
- Multiple claim ages
- `$50k` worker type
- Reform comparisons / overlays
- Two-step Single/Married + spouse-type control
- Sticky URL state
- Print/PDF stylesheet

If during implementation a step seems to require any of these, stop and surface the discrepancy rather than expanding scope.

---

## Self-review notes

- The build script asserts all 450 expected configs are present in each input CSV before writing the JSON, so a missing row fails the build loudly rather than producing a silent zero.
- All user-facing references to "Net Marginal Tax Rate" / "NMTR" are replaced with "Net Tax Rate by Age" in HTML and chart titles. Internal variable/canvas IDs use `netTaxRate` (e.g., `netTaxRateChart`) to keep code coherent.
- The two replacement-rate cards always show their description text (not behind tooltips) per the spec.
- The Cohort Comparison tab has 5 charts, mirroring the old layout minus IRR.
- No reform UI or sex/claim-age controls are wired anywhere.
