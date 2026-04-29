# ssmbar Dashboard — Design Spec

**Date**: 2026-04-28
**Status**: Approved (design phase). Implementation plan to follow.
**Repo**: `anthonycolavito/ssmbar` (`main` branch)

## Goal

Recreate the GitHub Pages dashboard that previously lived in the old ssmbar package (now archived as `ssmbar_OLD`) inside the new, simpler `ssmbar` package. Maintain the visual identity of the old dashboard while adapting to the new package's data dimensions and current-law-only scope.

## Context

The new `ssmbar` package was rebuilt from scratch as a slimmer R package. It computes Social Security benefits and taxes for a fixed set of worker configurations under current law, with no reform machinery and no Shiny app. The package emits four CSV/RDS files into `output/` describing the panel of worker configurations.

The old static dashboard lived at `anthonycolavito.github.io/ssmbar/` and consumed per-worker JSON files. The new dashboard will live in the same repo at a custom path served by GitHub Pages from a `gh-pages` branch.

## Data dimensions (from the new package)

- **Worker types** (5): `very_low`, `low`, `medium`, `high`, `max`
- **Spouse types** (6): `none` (single), `very_low`, `low`, `medium`, `high`, `max`
- **Birth cohorts** (15): 1940 through 2010 in 5-year increments
- **Claim age**: 65 only
- **Total configurations**: 5 × 6 × 15 = 450

Dropped vs. the old dashboard: sex dimension, `$50k` worker, multiple claim ages, all reform comparisons, Internal Rate of Return summary card.

## Source data files (already in `output/`)

| File | Rows | Purpose |
|---|---|---|
| `benefits_by_worker_age.csv` | ~24,750 | Annual nominal/real benefit and earnings per (worker, spouse, cohort, age) |
| `net_tax_on_earnings.csv` | ~19,800 | Net tax rate per (worker, spouse, cohort, age) |
| `pv_lifetime_taxes_benefits.csv` | ~300 | PV taxes, PV benefits, and benefit/tax ratio per config |
| `initial_replacement_rates.csv` | ~450 | Initial monthly benefit and two replacement-rate measures per config |

Total raw size ~4.1 MB; gzipped ~1 MB. Feeding the dashboard directly from these would require client-side parsing of repeated key columns, so they will be reshaped into a single nested JSON file at deploy time.

## Architecture

### Repo layout

```
ssmbar/
├── R/                                   # unchanged — package code
├── data-raw/                            # unchanged
├── data/                                # unchanged
├── output/                              # unchanged — source of truth for site data
├── site/                                # NEW — static dashboard source
│   ├── index.html
│   ├── css/style.css                    # ported byte-for-byte from ssmbar_OLD
│   └── js/
│       ├── app.js                       # kept, trimmed
│       ├── chart-manager.js             # kept ~as-is
│       ├── data-loader.js               # rewritten
│       ├── formatters.js                # kept as-is
│       ├── table-manager.js             # kept ~as-is
│       └── ui-controls.js               # rewritten
├── build/
│   └── build_site_data.R                # NEW — CSVs → site_data.json
├── .github/workflows/
│   └── deploy-site.yml                  # NEW — Actions workflow
├── .Rbuildignore                        # updated to exclude site/, build/, .github/, docs/
└── docs/superpowers/specs/              # NEW — design docs (this file)
```

`site/`, `build/`, `.github/`, and `docs/` are added to `.Rbuildignore` so the R package check is unaffected.

### Data flow

```
output/*.csv  →  build_site_data.R  →  site/data/site_data.json
                                                    ↓
                                             fetch().json()
                                                    ↓
                                          ui-controls.js / app.js
                                                    ↓
                                       Chart.js + DOM rendering
```

The build script is run by the GitHub Action only — `site/data/site_data.json` is never committed to `main`. The CSVs in `output/` are the single source of truth.

### Generated JSON shape

```jsonc
{
  "meta": {
    "data_mode": "current_law_only",
    "claim_age": 65,
    "generated": "2026-04-28T...Z",
    "package_version": "<from DESCRIPTION>"
  },
  "dimensions": {
    "worker_types": [
      { "key": "very_low", "label": "Very Low Earner" },
      { "key": "low",      "label": "Low Earner" },
      { "key": "medium",   "label": "Medium Earner" },
      { "key": "high",     "label": "High Earner" },
      { "key": "max",      "label": "Maximum Earner" }
    ],
    "spouse_types": [
      { "key": "none",     "label": "None (Single)" },
      { "key": "very_low", "label": "Very Low Earner" },
      { "key": "low",      "label": "Low Earner" },
      { "key": "medium",   "label": "Medium Earner" },
      { "key": "high",     "label": "High Earner" },
      { "key": "max",      "label": "Maximum Earner" }
    ],
    "birth_years": [1940, 1945, 1950, 1955, 1960, 1965, 1970, 1975,
                    1980, 1985, 1990, 1995, 2000, 2005, 2010]
  },
  "configs": {
    "medium|none|1960": {
      "annual": {
        "ages":     [65, 66, ...],
        "nominal":  [...],
        "real":     [...],
        "earnings": [...]
      },
      "nmtr": {
        "ages":   [21, 22, ...],
        "values": [...]
      },
      "summary": {
        "monthly_real_at_65": 1234.56,
        "pv_benefits":        345678.9,
        "pv_taxes":           123456.7,
        "ben_tax_ratio":      2.80,
        "rep_rate_career":    0.41,
        "rep_rate_awi":       0.30
      }
    }
    // 449 more entries
  }
}
```

Estimated size: ~900 KB raw, ~250 KB gzipped — about 4× smaller than the four CSVs.

### Deployment

GitHub Action `deploy-site.yml`, triggered by push to `main` on changes under `R/**`, `output/**`, `site/**`, or `build/**`:

1. `actions/checkout@v4`
2. `r-lib/actions/setup-r@v2`
3. `Rscript -e 'install.packages("jsonlite")'`
4. `Rscript build/build_site_data.R` — writes `site/data/site_data.json`
5. `peaceiris/actions-gh-pages@v3` — publishes `site/` to `gh-pages` branch

GitHub Pages is configured to serve `gh-pages` at root. Live URL: `anthonycolavito.github.io/ssmbar/`.

The implementing agent will configure Pages and Action permissions via `gh` CLI / API; the user has authorized handling these setup steps autonomously.

## UI structure

### Overall layout

```
┌──────────────────────────────────────────────────────────────┐
│  Navbar: "Social Security Benefit Explorer  |  2025 TR"     │
├──────────────────────────────────────────────────────────────┤
│  [ Individual Worker ]  [ Cohort Comparison ]    ← tabs      │
├──────────────────────────────────────────────────────────────┤
│  Worker config row (4 controls)                              │
├──────────────────────────────────────────────────────────────┤
│  Summary cards (6)                                           │
├──────────────────────────────────────────────────────────────┤
│  Annual Benefits by Age   (real/nominal toggle)              │
├──────────────────────────────────────────────────────────────┤
│  Net Tax Rate by Age                                         │
├──────────────────────────────────────────────────────────────┤
│  Data table  +  CSV export                                   │
├──────────────────────────────────────────────────────────────┤
│  Reform placeholder card (small, footer-area)                │
└──────────────────────────────────────────────────────────────┘
```

The old left reform sidebar is removed. Main content takes full width.

### Worker configuration controls

| Control | Type | Options |
|---|---|---|
| Worker Type | radio / segmented | Very Low · Low · Medium · High · Maximum |
| Spouse | dropdown | None (Single) · Very Low · Low · Medium · High · Maximum |
| Birth Cohort | dropdown | 1940 · 1945 · … · 2010 |
| Real / Nominal | toggle | Real (default) · Nominal |

A single Spouse dropdown is used in v1 (encodes both single/married and spouse type in one control). The two-step `Single/Married` + spouse-type pattern from the old app is deferred — may be revisited after the user sees the v1 layout.

### Individual Worker tab

**Summary cards** (6, in this order):

1. Monthly Benefit at 65 (real $) — derived from `benefits_by_worker_age` row at `age == 65` for the active config, divided by 12 to convert the annual figure to monthly. (`real_ben_at_65` in `initial_replacement_rates.csv` is the same annual value; either source is acceptable as long as the build script picks one and documents it.)
2. PV Lifetime Benefits — `pv_lifetime_taxes_benefits.pv_benefits`
3. PV Lifetime Taxes — `pv_lifetime_taxes_benefits.pv_taxes`
4. Benefit / Tax Ratio — `pv_lifetime_taxes_benefits.ben_tax_ratio`
5. Replacement Rate (Career) — `initial_replacement_rates.rep_rate_career`. Subtitle text: *"Initial benefit as a share of the worker's average real career earnings."*
6. Replacement Rate (AWI) — `initial_replacement_rates.rep_rate_awi`. Subtitle text: *"Initial benefit as a share of that year's average wage index."*

The two replacement-rate cards display their description text directly under the headline number in small muted italic, always visible (not hidden behind a tooltip).

**Charts:**

- **Annual Benefits by Age** — line chart, x = age (range from claim age to death age), y = benefit dollars. Respects the global Real/Nominal toggle. Chart title carries the same toggle indicator. Source: `benefits_by_worker_age` filtered to active config.
- **Net Tax Rate by Age** — line chart, x = age (range across working years), y = net tax rate (%). Source: `net_tax_on_earnings.net_tax`. Y-axis bounds applied to keep the chart readable across configs (initial bounds: `[-10%, 25%]`; tune based on observed data).

Both chart titles, axis labels, legends, and code comments use **"Net Tax Rate by Age"** and never **"Net Marginal Tax Rate"** or **"NMTR"**.

**Data table:**

Scrollable table of the year-by-year data feeding the Annual Benefits chart (columns: age, year, earnings, nominal benefit, real benefit). "Download CSV" button exports the current view.

### Cohort Comparison tab

Iterates over all 15 birth cohorts for the active Worker Type and Spouse. Five small charts (matches the old layout minus IRR):

1. Monthly Benefit at 65 (real $) — same derivation as the summary card (annual `real_ben_at_65` divided by 12)
2. Replacement Rate (Career) — with description as in the summary card
3. Replacement Rate (AWI) — with description as in the summary card
4. PV Lifetime Benefits
5. Benefit / Tax Ratio

Each plotted with x = birth cohort year, in 5-year steps from 1940 to 2010.

### Reform placeholder

Small card, placed at the bottom of the page beneath the data table:

> ℹ️ Reforms not yet supported in this version. The new ssmbar package computes current-law benefits only; reform comparisons may be added in a future release.

No greyed-out reform buttons; just this single card.

### Visual identity

- `style.css` from `ssmbar_OLD/docs/css/style.css` ports byte-for-byte (860 lines).
- HTML structure mirrors the old layout where it overlaps; only the worker-config row, the reform-card replacement, and the summary-card content are modified.
- Same font stack (Inter via Google Fonts), same Bootstrap 5.3.3, same Bootstrap Icons.
- Chart.js for charts, with the old's light-theme styling preserved.

### Default state on load

- Worker Type: **Medium**
- Spouse: **None (Single)**
- Birth Cohort: **1960**
- Real / Nominal: **Real**
- Active tab: **Individual Worker**

## Build script (`build/build_site_data.R`)

~40 lines. Pseudocode:

```r
# Read CSVs
ben_age   <- read.csv("output/benefits_by_worker_age.csv")
nmtr      <- read.csv("output/net_tax_on_earnings.csv")
pv        <- read.csv("output/pv_lifetime_taxes_benefits.csv")
rep_rates <- read.csv("output/initial_replacement_rates.csv")

# Build configs map keyed by "<worker>|<spouse>|<birth_yr>"
configs <- list()
for each (w, s, b) in cartesian(worker_types, spouse_types, birth_years):
  key <- paste(w, s, b, sep = "|")
  configs[[key]] <- list(
    annual  = list(
      ages     = subset(ben_age, ...)$age,
      nominal  = subset(ben_age, ...)$nominal_ben,
      real     = subset(ben_age, ...)$real_ben,
      earnings = subset(ben_age, ...)$earnings
    ),
    nmtr    = list(
      ages   = subset(nmtr, ...)$age,
      values = subset(nmtr, ...)$net_tax
    ),
    summary = list(
      monthly_real_at_65 = ...,
      pv_benefits        = subset(pv, ...)$pv_benefits,
      pv_taxes           = subset(pv, ...)$pv_taxes,
      ben_tax_ratio      = subset(pv, ...)$ben_tax_ratio,
      rep_rate_career    = subset(rep_rates, ...)$rep_rate_career,
      rep_rate_awi       = subset(rep_rates, ...)$rep_rate_awi
    )
  )

# Write
out <- list(
  meta       = list(data_mode = "current_law_only", claim_age = 65, generated = Sys.time()),
  dimensions = list(...),
  configs    = configs
)
jsonlite::write_json(out, "site/data/site_data.json", auto_unbox = TRUE, digits = 6)
```

The script must:

- Verify input row counts match the expected 450 configs × N rows.
- Drop rows where `claim_age != 65` defensively (data should already be filtered, but be explicit).
- Use `auto_unbox = TRUE` so single-element values aren't wrapped in arrays.
- Round values to a sensible precision (6 significant digits is fine; smaller transfers).
- Fail loudly if any expected (worker, spouse, birth_yr) tuple is missing from any of the four input files.

## JS modules — what changes from `ssmbar_OLD/docs/js/`

| File | Action | Rationale |
|---|---|---|
| `formatters.js` | Keep as-is | Currency/percent formatters work unchanged. |
| `table-manager.js` | Keep with minor edits | Column set is the same shape; CSV export logic unchanged. |
| `chart-manager.js` | Keep with edits | Strip reform-overlay code paths; rename "Net Marginal Tax Rate" → "Net Tax Rate by Age" in titles/labels; keep all theming. |
| `data-loader.js` | Rewrite | Fetches `data/site_data.json` once via `fetch().json()` and exposes a `getConfig(worker, spouse, birth_yr)` accessor over the `configs` map. |
| `ui-controls.js` | Rewrite | New control set (Worker Type, Spouse, Birth Cohort, Real/Nominal). No sex, no claim age, no reform buttons. |
| `app.js` | Trim significantly | Remove reform selection logic, sex/marital-pair lookups, claim-age handling. Keep tab switching, Real/Nominal toggle, summary-card update, chart update wiring. |

## Out of scope (deferred)

- **IRR (Internal Rate of Return)** — would require a new R output (cashflow-based IRR over taxes/benefits). Defer to a future enhancement.
- **Sex dimension** — not in new package data. Defer.
- **Multiple claim ages** — only age 65 in new data. Defer.
- **`$50k` worker type** — not in new package. Defer.
- **Reform comparisons** — no reform code in new package. Placeholder card only; full integration deferred.
- **Two-step Single/Married + spouse-type control** — single dropdown in v1; revisit after user sees layout.
- **Sticky URL state** (e.g., `?worker=medium&birth=1960`) — nice-to-have.
- **Dedicated mobile layout polish** — inherit from ported CSS; do not redesign.
- **Print/PDF stylesheet**.

## Verification

Before declaring v1 complete:

1. **Build script runs locally**: `Rscript build/build_site_data.R` produces a well-formed JSON with all 450 configs and no missing fields.
2. **Spot-check three random configs**: numerical values in `summary` and the first/last entries of each array match the source CSVs.
3. **Functional test in Chrome and Safari**:
   - Each control change updates charts and cards correctly
   - Real/Nominal toggle redraws the Annual Benefits chart and the corresponding card
   - Tab switching populates the cohort charts over all 15 cohorts for the current worker/spouse
   - "Download CSV" produces a file matching the visible table
4. **Visual check**: open `ssmbar_OLD`'s live old dashboard side-by-side; confirm fonts, colors, card styling, navbar, and chart theming match within obvious tolerance.
5. **Console clean**: no JS errors on load or on any user interaction.
6. **Performance**: time-to-interactive under ~2s on a normal connection (gzipped JSON ~250 KB).
7. **Live deploy**: trigger the Action; confirm the published site at `anthonycolavito.github.io/ssmbar/` works end-to-end.

## Risks and mitigations

| Risk | Mitigation |
|---|---|
| Old `chart-manager.js` references reform overlays / sex toggles that don't exist in the new data | Trim those code paths during the JS port, before wiring data. |
| Default y-axis bounds for the Net Tax Rate chart squish or distort the line for some configs | Start with the old's clamping convention; verify across at least 6 configs (low/medium/high × single/married). |
| GitHub Pages caching delays a deploy from being visible | Document the URL `?v=<timestamp>` cache-bust in the README. |
| First-time `peaceiris/actions-gh-pages` permissions issues | Confirm `Settings → Actions → Workflow permissions` is set to "Read and write" via API/CLI as part of setup. |
| `ben_at_65` / `real_ben_at_65` units (annual vs monthly) | Verify against `benefits_by_worker_age` for an overlapping config before wiring the summary card; document the unit explicitly in the build script. |

## Open questions to resolve during implementation

1. What is the base year for the "real" dollar series? (Likely 2025 based on file naming, but verify against the package source and surface the year in the chart axis label, e.g., "Real $ (2025)".)
2. What is the appropriate Y-axis range for "Net Tax Rate by Age" given the new package's data? Empirically determine after the first build by inspecting the spread across worker types.
3. Confirm `real_ben_at_65` is annual (matches the per-row `real_ben` value at `age == 65` in `benefits_by_worker_age`). Initial inspection of one row strongly suggests yes; the build script should assert this rather than assume.
