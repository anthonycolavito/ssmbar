# Social Security Benefit Explorer -- Redesign Implementation Spec

**Date:** 2026-03-02
**Authors:** SS Policy Expert + UI/Design Expert (collaborative design session)
**Status:** APPROVED -- ready for implementation

---

## Table of Contents

1. [Design Philosophy](#1-design-philosophy)
2. [Color System](#2-color-system)
3. [Typography](#3-typography)
4. [Page Layout & Structure](#4-page-layout--structure)
5. [Hero Section](#5-hero-section)
6. [Individual View -- Narrative Sections](#6-individual-view----narrative-sections)
7. [Cohort View -- "The Generational Story"](#7-cohort-view----the-generational-story)
8. [Reform Sidebar](#8-reform-sidebar)
9. [Chart.js Configuration](#9-chartjs-configuration)
10. [Data Layer Changes](#10-data-layer-changes)
11. [Responsive / Mobile](#11-responsive--mobile)
12. [Notes & Methodology Panels](#12-notes--methodology-panels)
13. [File Manifest](#13-file-manifest)
14. [Phase 2 Ideas](#14-phase-2-ideas)

---

## 1. Design Philosophy

The current site is a technically solid dashboard with no storytelling. The redesign transforms it from a "data dashboard" into a "narrative data tool" -- answering user questions before asking for configuration input.

**Core principles:**

- **Story first, controls second.** Lead with a compelling number and context, not dropdowns.
- **Plain English everywhere.** No jargon in user-facing labels. "NMTR" becomes "How Your Taxes Work." "IRR" becomes "Rate of Return." "PV Benefits" becomes "Lifetime Benefits."
- **Progressive disclosure.** Hero metrics visible immediately; detailed data available on demand.
- **One story at a time.** Cohort tab shows one chart with a selector, not five charts simultaneously.

**Target audience:** General public researching Social Security -- not economists or policy analysts (though the data supports their needs too via CSV export and detailed tables).

---

## 2. Color System

Switch from dark theme to light theme. Keep navbar and sidebar dark for brand identity.

### CSS Variables (replace existing `:root` block)

```css
:root {
  /* Brand */
  --crfb-navy:       #0f1741;
  --crfb-blue:       #003477;
  --crfb-orange:     #F36107;

  /* Page */
  --bg-page:         #FAFBFC;
  --bg-card:         #FFFFFF;
  --bg-card-alt:     #F3F4F6;   /* alternate card bg for visual rhythm */
  --shadow-card:     0 1px 3px rgba(0, 0, 0, 0.08);

  /* Text */
  --text-primary:    #1a1a2e;
  --text-secondary:  #6b7280;
  --text-muted:      #9ca3af;

  /* Interactive */
  --accent-blue:     #2563EB;   /* chart data lines, links, active states */
  --accent-orange:   #F36107;   /* reform comparison, CTA buttons */
  --border:          #E5E7EB;
  --border-focus:    #2563EB;

  /* Directional (for reform comparisons only -- NOT value judgments) */
  --color-positive:  #059669;
  --color-negative:  #DC2626;

  /* NMTR chart bars */
  --nmtr-accrual:    #0D9488;   /* negative bars: taxes offset by future benefits */
  --nmtr-tax:        #D97706;   /* positive bars: net tax burden */

  /* Dark surfaces (navbar, sidebar) */
  --bg-dark:         #0f1741;
  --bg-dark-card:    #1a2150;
  --text-on-dark:    #e8e8e8;
  --text-muted-dark: #a0a0b0;
  --border-dark:     #2a3f5f;

  /* Chart */
  --chart-grid:      rgba(0, 0, 0, 0.06);
  --chart-line:      #2563EB;
  --chart-fill:      rgba(37, 99, 235, 0.08);
  --chart-reform:    #F36107;

  /* Font */
  --font-family:     'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
}
```

### Font Loading (replace Open Sans)

```html
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">
```

---

## 3. Typography

### Size Scale

| Role | Size | Weight | Usage |
|------|------|--------|-------|
| Hero number | 2.75rem | 700 | Monthly benefit in hero section |
| Hero unit | 1.5rem | 400 | "/month" suffix |
| Section title | 1.25rem | 700 | "Your Benefit", "Your Return on Taxes", etc. |
| Section subtitle | 0.95rem | 400 | Explanatory text below section titles |
| Body / metrics | 0.9rem | 400 | General text, secondary metric values |
| Metric value (Tier 2) | 1.5rem | 700 | Ratio, PV Benefits values |
| Form labels | 0.78rem | 600 | Uppercase, letter-spacing: 0.04em |
| Fine print / notes | 0.75rem | 400 | Methodology notes, disclaimers |
| Chart axis labels | 11px | 400 | Chart.js tick labels |
| Chart tooltip | 12px | 400/700 | Tooltip body/title |

### Principles

- Uppercase reserved for form labels and fine-print headers ONLY. Section titles use sentence case.
- Tabular figures (`font-variant-numeric: tabular-nums`) on all number displays for alignment.
- Hero number is the largest text on the page (2.75rem). Nothing else competes.

---

## 4. Page Layout & Structure

### Overall Structure

```
+--------------------------------------------------+
|  NAVBAR (dark: --bg-dark)                         |
|  [Logo/Title]               [Individual | Cohort] |
+------+-------------------------------------------+
| SIDE |  HERO SECTION (always visible)             |
| BAR  |  - Sentence with inline dropdowns          |
| 48px |  - Big number + context                    |
| icon |  - Secondary controls (sex, marital)       |
| strip|  - Two CTAs                                |
|      |-------------------------------------------+
|      |  TAB CONTENT                               |
|      |  (Individual narrative OR Cohort story)     |
|      |                                            |
+------+-------------------------------------------+
```

### Key Decisions

- **Tabs stay** (not a single long-scroll page). Tabs are embedded in the navbar, not below the hero.
- **Hero section is always visible** regardless of active tab. It always shows the individual worker's monthly benefit. Switching to the Cohort tab changes the content below the hero but the hero remains as context.
- **Reform sidebar is a 48px collapsed icon strip** on the left edge. It does NOT expand until reforms are enabled.
- **Content area gets full remaining width** (~calc(100% - 48px) on desktop).

### Navbar

```html
<nav class="navbar navbar-dark">
  <div class="container-fluid">
    <a class="navbar-brand" href="#">Social Security Benefit Explorer</a>
    <div class="d-flex align-items-center">
      <ul class="nav nav-pills" id="mainTabs">
        <li><button class="nav-link active" data-tab="individual">Your Benefits</button></li>
        <li><button class="nav-link" data-tab="cohort">Across Generations</button></li>
      </ul>
      <span class="text-muted-dark ms-3" style="font-size: 0.72rem;">
        ssmbar | 2025 Trustees Report
      </span>
    </div>
  </div>
</nav>
```

Tab labels: "Your Benefits" and "Across Generations" (user-facing language, not data-structure language).

---

## 5. Hero Section

### HTML Structure

```html
<section class="hero-section">
  <p class="hero-sentence">
    A <select id="heroWorkerType" class="inline-select">
      <option value="very_low">very low</option>
      <option value="low">low</option>
      <option value="medium" selected>medium</option>
      <option value="high">high</option>
      <option value="max">maximum</option>
      <option value="custom_50k">$50K</option>
    </select> earner born in
    <select id="heroBirthYear" class="inline-select">
      <option value="1940">1940</option>
      <option value="1950">1950</option>
      <option value="1960" selected>1960</option>
      <!-- ... 1970-2010 -->
    </select> can expect:
  </p>

  <div class="hero-number">
    $<span id="heroNominalBenefit">2,093</span><span class="hero-unit">/month</span>
  </div>
  <div class="hero-real-benefit" id="heroRealBenefit">
    $2,093/month in today's dollars
  </div>
  <div class="hero-replacement" id="heroReplacement">
    That replaces <strong>50.1%</strong> of pre-retirement earnings
  </div>

  <div class="hero-customize">
    <label class="customize-label">Customize:</label>
    <select id="heroSex" class="inline-select-sm">
      <option value="unisex" selected>Unisex</option>
      <option value="male">Male</option>
      <option value="female">Female</option>
    </select>
    <select id="heroMarital" class="inline-select-sm">
      <option value="single" selected>Single</option>
      <option value="married">Married</option>
    </select>
  </div>

  <div class="hero-ctas">
    <button class="btn btn-primary" onclick="switchTab('individual')">Explore Your Benefits</button>
    <button class="btn btn-outline" onclick="switchTab('cohort')">Compare Generations</button>
  </div>
</section>
```

### CSS for Hero

```css
.hero-section {
  text-align: center;
  padding: 2.5rem 1.5rem 2rem;
  background: var(--bg-card);
  border-bottom: 1px solid var(--border);
  margin-bottom: 1.5rem;
}

.hero-sentence {
  font-size: 1.15rem;
  color: var(--text-secondary);
  margin-bottom: 0.75rem;
}

.inline-select {
  border: none;
  border-bottom: 2px solid var(--accent-blue);
  background: transparent;
  color: var(--text-primary);
  font-weight: 700;
  font-size: inherit;
  font-family: inherit;
  padding: 0 1.2rem 0 0.15rem;
  cursor: pointer;
  appearance: none;
  -webkit-appearance: none;
  background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='10' height='6'%3E%3Cpath d='M0 0l5 6 5-6z' fill='%232563EB'/%3E%3C/svg%3E");
  background-repeat: no-repeat;
  background-position: right 0.1rem center;
  background-size: 10px 6px;
}

.inline-select:focus {
  outline: none;
  border-bottom-color: var(--accent-orange);
}

.hero-number {
  font-size: 2.75rem;
  font-weight: 700;
  color: var(--text-primary);
  line-height: 1.1;
  font-variant-numeric: tabular-nums;
}

.hero-unit {
  font-size: 1.5rem;
  font-weight: 400;
  color: var(--text-secondary);
}

.hero-real-benefit {
  font-size: 1rem;
  color: var(--text-secondary);
  margin-top: 0.25rem;
}

.hero-replacement {
  font-size: 1rem;
  color: var(--text-secondary);
  margin-top: 0.25rem;
}

.hero-replacement strong {
  color: var(--accent-blue);
  font-weight: 700;
}

.hero-customize {
  margin-top: 1.25rem;
  font-size: 0.82rem;
  color: var(--text-muted);
}

.customize-label {
  font-size: 0.75rem;
  text-transform: uppercase;
  letter-spacing: 0.04em;
  color: var(--text-muted);
  margin-right: 0.5rem;
}

.inline-select-sm {
  /* Same as .inline-select but smaller */
  border: none;
  border-bottom: 1px solid var(--border);
  background: transparent;
  color: var(--text-secondary);
  font-size: 0.82rem;
  font-family: inherit;
  padding: 0 1rem 0 0.1rem;
  margin: 0 0.5rem;
  cursor: pointer;
  appearance: none;
  -webkit-appearance: none;
  background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='8' height='5'%3E%3Cpath d='M0 0l4 5 4-5z' fill='%239ca3af'/%3E%3C/svg%3E");
  background-repeat: no-repeat;
  background-position: right 0.1rem center;
  background-size: 8px 5px;
}

.hero-ctas {
  margin-top: 1.5rem;
  display: flex;
  gap: 1rem;
  justify-content: center;
}

.hero-ctas .btn-primary {
  background-color: var(--accent-orange);
  border-color: var(--accent-orange);
  color: #fff;
  font-weight: 600;
  font-size: 0.9rem;
  padding: 0.5rem 1.5rem;
  border-radius: 0.375rem;
}

.hero-ctas .btn-outline {
  background: transparent;
  border: 1.5px solid var(--border);
  color: var(--text-secondary);
  font-weight: 600;
  font-size: 0.9rem;
  padding: 0.5rem 1.5rem;
  border-radius: 0.375rem;
}
```

### Default Values (medium earner, 1960, unisex, single)

| Field | Value | Source |
|-------|-------|--------|
| monthly_benefit | $2,093 | cohort/medium.json `monthly_benefit[2]` (same M/F) |
| initial_real_benefit | $2,093 | cohort/medium.json `initial_real_benefit[2]` (same M/F) |
| repl_rate | 50.1% | cohort/medium.json `repl_rate[2]` = 0.501173 (same M/F) |
| pv_benefits (unisex) | $451,714 | avg(428716, 474711) |
| pv_taxes | $335,518 | same M/F |
| ratio (unisex) | 1.35 | avg(1.2778, 1.4149) |
| irr (unisex) | 4.5% | avg(0.043565, 0.046883) |
| death_age (unisex) | 85.6 | avg(84.3, 86.8) |

---

## 6. Individual View -- Narrative Sections

Replace the current dashboard layout with four narrative sections that scroll vertically below the hero.

### Section 1: "Your Benefit"

```html
<section class="narrative-section">
  <h2 class="section-title">Your Benefit</h2>
  <p class="section-subtitle">
    Annual Social Security retirement benefits from age 65 to expected death
  </p>

  <!-- Benefits chart -->
  <div class="card">
    <div class="card-header d-flex justify-content-between align-items-center">
      <span>Annual Benefits by Age</span>
      <div class="toggle-group">
        <button data-toggle="nominal">Nominal $</button>
        <button class="active" data-toggle="real">Today's $</button>
      </div>
    </div>
    <div class="card-body p-2">
      <div class="chart-container" style="height: 380px;">
        <canvas id="benefitsChart"></canvas>
      </div>
    </div>
  </div>
</section>
```

**Benefits chart requirements:**
- Line chart with filled area below
- Death-age annotation: vertical dashed line at the final age with label "Expected lifespan: XX.X years"
- Default view: real dollars
- Tooltip: "Age 72: $25,403/year (today's dollars)" or "Age 72: $26,460/year (nominal)" depending on toggle
- Chart.js config: see Section 9

### Section 2: "Your Return on Taxes Paid"

```html
<section class="narrative-section">
  <h2 class="section-title">Your Return on Taxes Paid</h2>
  <p class="section-subtitle">
    How lifetime Social Security benefits compare to lifetime taxes
  </p>

  <div class="return-summary">
    <div class="return-ratio">
      <div class="return-ratio-label">For every $1 in Social Security taxes, you get back</div>
      <div class="return-ratio-value">$<span id="ratioValue">1.35</span></div>
      <div class="return-ratio-sublabel">in lifetime benefits</div>
    </div>

    <div class="return-details">
      <div class="return-detail-row">
        <span class="detail-label">Lifetime Benefits</span>
        <span class="detail-value" id="pvBenefitsValue">$452K</span>
      </div>
      <div class="return-detail-row">
        <span class="detail-label">Lifetime Taxes Paid</span>
        <span class="detail-value" id="pvTaxesValue">$336K</span>
      </div>
      <div class="return-detail-row muted">
        <span class="detail-label">Effective Rate of Return</span>
        <span class="detail-value" id="irrValue">4.5%</span>
      </div>
    </div>
  </div>

  <!-- Sex insight callout (when unisex is selected) -->
  <div class="insight-callout" id="sexInsight">
    Women receive the same monthly benefit but collect more over a lifetime
    due to longer life expectancy.
    <a href="#" onclick="setSex('female'); return false;">See the difference</a>
  </div>
</section>
```

**CSS for return section:**

```css
.return-summary {
  display: flex;
  gap: 2rem;
  align-items: center;
  padding: 1.5rem;
  background: var(--bg-card);
  border-radius: 0.5rem;
  box-shadow: var(--shadow-card);
}

.return-ratio {
  text-align: center;
  min-width: 200px;
}

.return-ratio-value {
  font-size: 2.25rem;
  font-weight: 700;
  color: var(--accent-blue);
  font-variant-numeric: tabular-nums;
}

.return-ratio-label,
.return-ratio-sublabel {
  font-size: 0.82rem;
  color: var(--text-secondary);
}

.return-details {
  flex: 1;
}

.return-detail-row {
  display: flex;
  justify-content: space-between;
  padding: 0.5rem 0;
  border-bottom: 1px solid var(--border);
  font-size: 0.9rem;
}

.return-detail-row .detail-label {
  color: var(--text-secondary);
}

.return-detail-row .detail-value {
  font-weight: 600;
  color: var(--text-primary);
  font-variant-numeric: tabular-nums;
}

.return-detail-row.muted .detail-value {
  color: var(--text-secondary);
}

.insight-callout {
  margin-top: 1rem;
  padding: 0.75rem 1rem;
  background: var(--bg-card-alt);
  border-left: 3px solid var(--accent-blue);
  border-radius: 0 0.375rem 0.375rem 0;
  font-size: 0.85rem;
  color: var(--text-secondary);
}

.insight-callout a {
  color: var(--accent-blue);
  font-weight: 600;
}
```

### Section 3: "How Your Social Security Taxes Actually Work"

This is the NMTR section. **VISIBLE by default -- not collapsed.**

```html
<section class="narrative-section">
  <h2 class="section-title">How Your Social Security Taxes Actually Work</h2>
  <p class="section-subtitle">
    Each year you work, your Social Security taxes buy you future retirement
    benefits. This chart shows how much of each year's taxes are offset by the
    additional benefits you earn.
  </p>

  <div class="card">
    <div class="card-body p-2">
      <div class="chart-container" style="height: 380px;">
        <canvas id="nmtrChart"></canvas>
      </div>
    </div>
  </div>

  <!-- Dynamic insight callout -->
  <div class="insight-callout" id="nmtrInsight">
    For a medium earner born in 1960, <strong>XX%</strong> of career Social
    Security taxes were fully offset by increased future benefits.
  </div>
</section>
```

**NMTR chart requirements:**
- Bar chart with DUAL COLORS:
  - Bars where NMTR < 0: `var(--nmtr-accrual)` (#0D9488) -- taxes offset by future benefits
  - Bars where NMTR >= 0: `var(--nmtr-tax)` (#D97706) -- net tax burden
- Zero line: solid 1.5px line in `var(--text-muted)` (#9ca3af)
- Zone annotations:
  - Below zero: "Taxes offset by future benefits" (in --nmtr-accrual color, positioned left)
  - Above zero: "Net tax burden" (in --nmtr-tax color, positioned right)
- Tooltip: "Age 35 | Tax Rate: -1.2% | Earnings: $25,655 | SS Tax: $1,591 | Benefit Increase: $3,432"
- Y-axis label: "Net Tax Rate (%)"
- X-axis label: "Age"

**Dynamic insight computation:**
Count the number of ages where NMTR < 0, divide by total ages (44 years, age 21-64), format as percentage.

### Section 4: "Detailed Data"

```html
<section class="narrative-section">
  <div class="card">
    <div class="collapsible-header" onclick="toggleSection('indDataTable')">
      <span>Detailed Data Table</span>
      <div>
        <button class="btn btn-outline-sm" onclick="event.stopPropagation(); exportCSV('benefits')">
          Download CSV
        </button>
        <i class="bi bi-chevron-down" id="indDataTableChevron"></i>
      </div>
    </div>
    <div class="collapsible-body collapsed" id="indDataTable">
      <div class="card-body p-2">
        <div id="benefitsTableWrapper"></div>
      </div>
    </div>
  </div>
</section>
```

**Collapsed by default.** Power users expand it.

---

## 7. Cohort View -- "The Generational Story"

Replace the 5-chart grid with a single hero chart + metric selector.

### HTML Structure

```html
<div class="tab-pane" id="cohortPane">
  <section class="narrative-section">
    <h2 class="section-title" id="cohortTitle">
      Each generation's return on Social Security taxes
    </h2>
    <p class="section-subtitle" id="cohortSubtitle">
      A ratio above 1.0 means you receive more in benefits than you paid in taxes
    </p>

    <!-- Metric selector buttons -->
    <div class="metric-selector" id="cohortMetricSelector">
      <button class="metric-btn active" data-metric="ratio">Benefit-Tax Ratio</button>
      <button class="metric-btn" data-metric="initial_real_benefit">Monthly Benefit</button>
      <button class="metric-btn" data-metric="repl_rate">Replacement Rate</button>
      <button class="metric-btn" data-metric="pv_benefits">Lifetime Benefits</button>
      <button class="metric-btn" data-metric="irr">Rate of Return</button>
    </div>

    <!-- Single hero chart -->
    <div class="card">
      <div class="card-body p-2">
        <div class="chart-container" style="height: 400px;">
          <canvas id="cohortHeroChart"></canvas>
        </div>
      </div>
    </div>

    <!-- Summary stats -->
    <div class="cohort-summary" id="cohortSummary">
      <!-- Dynamically populated: 3-4 key stats for the selected metric -->
    </div>

    <!-- Data table (collapsed) -->
    <div class="card mt-3">
      <div class="collapsible-header" onclick="toggleSection('cohDataTable')">
        <span>Detailed Data Table</span>
        <div>
          <button class="btn btn-outline-sm" onclick="event.stopPropagation(); exportCSV('cohort')">
            Download CSV
          </button>
          <i class="bi bi-chevron-down"></i>
        </div>
      </div>
      <div class="collapsible-body collapsed" id="cohDataTable">
        <div class="card-body p-2">
          <div id="cohortTableWrapper"></div>
        </div>
      </div>
    </div>
  </section>
</div>
```

### Metric Selector CSS

```css
.metric-selector {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
  margin-bottom: 1rem;
}

.metric-btn {
  background: var(--bg-card);
  border: 1.5px solid var(--border);
  color: var(--text-secondary);
  font-size: 0.82rem;
  font-weight: 600;
  padding: 0.4rem 1rem;
  border-radius: 2rem;    /* pill shape */
  cursor: pointer;
  transition: all 0.15s;
}

.metric-btn:hover {
  border-color: var(--accent-blue);
  color: var(--text-primary);
}

.metric-btn.active {
  background: var(--accent-blue);
  border-color: var(--accent-blue);
  color: #fff;
}
```

### Narrative Headlines per Metric

Each metric selector button updates the title, subtitle, chart, y-axis, and annotations:

| Metric Key | Button Label | Title | Subtitle | Y-Axis Label | Annotation |
|------------|-------------|-------|----------|-------------|------------|
| `ratio` | Benefit-Tax Ratio | Each generation's return on Social Security taxes | A ratio above 1.0 means you receive more in benefits than you paid in taxes | Benefits per $1 of Taxes | Dashed line at y=1.0, label "Break-even" |
| `initial_real_benefit` | Monthly Benefit | How monthly benefits have grown across generations | Initial monthly benefit at age 65, in 2025 dollars | Monthly Benefit (2025 $) | None |
| `repl_rate` | Replacement Rate | What share of earnings Social Security replaces | Higher replacement rates mean Social Security covers more of your pre-retirement income | % of Earnings Replaced | None |
| `pv_benefits` | Lifetime Benefits | Total lifetime Social Security benefits by generation | Present value of all benefits from age 65 to expected death, discounted to age 65 | Lifetime Benefits (2025 $) | None |
| `irr` | Rate of Return | The effective return on Social Security taxes | Internal rate of return treating SS taxes as contributions and benefits as payouts | Annual Return (%) | Dashed line at y=0, label "Zero return" |

### Y-Axis Formatting per Metric

| Metric | Format Function |
|--------|----------------|
| `ratio` | `v => v.toFixed(2)` |
| `initial_real_benefit` | `v => '$' + (v >= 1000 ? (v/1000).toFixed(1) + 'K' : v.toFixed(0))` |
| `repl_rate` | `v => (v * 100).toFixed(0) + '%'` |
| `pv_benefits` | `v => '$' + (v/1000).toFixed(0) + 'K'` |
| `irr` | `v => (v * 100).toFixed(1) + '%'` |

### Default Chart: Benefit-Tax Ratio

Default hero chart shows `ratio` field from cohort data, with the 1.0 break-even annotation line rendered boldly (borderWidth: 1.5, font size: 12, color: var(--text-secondary)).

---

## 8. Reform Sidebar

### Collapsed State (current -- all reforms disabled)

Replace the 320px sidebar with a 48px-wide icon strip.

```html
<aside class="sidebar-strip" id="sidebarStrip">
  <div class="strip-lock">
    <i class="bi bi-lock"></i>
    <span class="strip-badge">14</span>
  </div>
  <div class="strip-icons">
    <div class="strip-icon" title="Slow Initial Benefit Growth"><i class="bi bi-calculator"></i></div>
    <div class="strip-icon" title="Increase Retirement Age"><i class="bi bi-clock"></i></div>
    <div class="strip-icon" title="Modify COLAs"><i class="bi bi-graph-up"></i></div>
    <div class="strip-icon" title="Increase Taxable Maximum"><i class="bi bi-cash-stack"></i></div>
    <div class="strip-icon" title="Other Reforms"><i class="bi bi-gear"></i></div>
  </div>
  <div class="strip-label">Reforms</div>

  <!-- HIDDEN: full reform HTML preserved for future activation -->
  <div class="sidebar-full" style="display: none;">
    <!-- ... existing reform radio buttons, unchanged ... -->
  </div>
</aside>
```

### CSS for Sidebar Strip

```css
.sidebar-strip {
  width: 48px;
  min-width: 48px;
  background: var(--bg-dark);
  border-right: 1px solid var(--border-dark);
  display: flex;
  flex-direction: column;
  align-items: center;
  padding: 1rem 0;
  gap: 1.25rem;
  position: sticky;
  top: 56px;
  max-height: calc(100vh - 56px);
}

.strip-lock {
  position: relative;
  color: var(--text-muted-dark);
  font-size: 1rem;
}

.strip-badge {
  position: absolute;
  top: -6px;
  right: -10px;
  background: var(--accent-orange);
  color: #fff;
  font-size: 0.6rem;
  font-weight: 700;
  padding: 1px 4px;
  border-radius: 8px;
  line-height: 1.2;
}

.strip-icon {
  color: var(--text-muted-dark);
  font-size: 0.95rem;
  cursor: default;
  opacity: 0.5;
}

.strip-icon:hover {
  opacity: 0.8;
}

.strip-label {
  writing-mode: vertical-rl;
  text-orientation: mixed;
  font-size: 0.65rem;
  color: var(--text-muted-dark);
  letter-spacing: 0.1em;
  text-transform: uppercase;
  margin-top: auto;
}
```

### Tooltip on Hover (full strip)

On hover over the entire strip, show a tooltip: "14 reform options coming soon." Use Bootstrap's native tooltip or a CSS-only approach.

---

## 9. Chart.js Configuration

### Global Defaults (replace CHART_DEFAULTS in chart-manager.js)

```javascript
const CHART_DEFAULTS = {
  responsive: true,
  maintainAspectRatio: false,
  animation: {
    duration: 350,
    easing: 'easeOutQuart'
  },
  plugins: {
    legend: {
      display: false,
      labels: {
        color: '#6b7280',
        font: { family: 'Inter', size: 11 },
        boxWidth: 12
      }
    },
    tooltip: {
      backgroundColor: '#0f1741',
      titleColor: '#e8e8e8',
      bodyColor: '#e8e8e8',
      borderColor: '#2a3f5f',
      borderWidth: 1,
      cornerRadius: 6,
      padding: 10,
      bodyFont: { family: 'Inter', size: 12 },
      titleFont: { family: 'Inter', size: 12, weight: 'bold' }
    }
  },
  scales: {
    x: {
      ticks: { color: '#6b7280', font: { family: 'Inter', size: 11 } },
      grid: { color: 'rgba(0, 0, 0, 0.06)' }
    },
    y: {
      ticks: { color: '#6b7280', font: { family: 'Inter', size: 11 } },
      grid: { color: 'rgba(0, 0, 0, 0.06)' }
    }
  }
};
```

### Benefits Chart (Individual)

```javascript
// Line chart config
{
  borderColor: '#2563EB',
  backgroundColor: 'rgba(37, 99, 235, 0.08)',
  borderWidth: 2.5,
  pointRadius: 0,
  pointHoverRadius: 6,
  fill: true,
  tension: 0.2
}

// Death age annotation (using chartjs-plugin-annotation)
annotation: {
  deathAge: {
    type: 'line',
    xMin: deathAge,   // last age in clipped series
    xMax: deathAge,
    borderColor: '#9ca3af',
    borderWidth: 1.5,
    borderDash: [6, 4],
    label: {
      display: true,
      content: `Expected lifespan: age ${deathAge}`,
      position: 'start',
      color: '#6b7280',
      font: { size: 11, family: 'Inter' },
      backgroundColor: 'rgba(250, 251, 252, 0.9)',
      padding: 4
    }
  }
}

// Tooltip callback
tooltip: {
  callbacks: {
    label: (ctx) => {
      const age = ctx.label;
      const val = ctx.parsed.y;
      // Include both nominal and real in tooltip
      return `Age ${age}: ${Fmt.currency(val)}/year`;
    }
  }
}
```

### NMTR Chart (Individual)

```javascript
// Per-bar coloring using scriptable options
{
  backgroundColor: (ctx) => {
    const val = ctx.dataset.data[ctx.dataIndex];
    return val < 0 ? '#0D948899' : '#D9770699';  // 60% opacity
  },
  borderColor: (ctx) => {
    const val = ctx.dataset.data[ctx.dataIndex];
    return val < 0 ? '#0D9488' : '#D97706';
  },
  borderWidth: 1
}

// Zero line annotation
annotation: {
  zeroLine: {
    type: 'line',
    yMin: 0,
    yMax: 0,
    borderColor: '#9ca3af',
    borderWidth: 1.5
  },
  accrualZone: {
    type: 'label',
    content: 'Taxes offset by future benefits',
    position: { x: 'start', y: 'start' },
    color: '#0D9488',
    font: { size: 11, family: 'Inter', style: 'italic' }
  },
  taxZone: {
    type: 'label',
    content: 'Net tax burden',
    position: { x: 'end', y: 'end' },
    color: '#D97706',
    font: { size: 11, family: 'Inter', style: 'italic' }
  }
}
```

### Cohort Hero Chart

```javascript
{
  borderColor: '#2563EB',
  backgroundColor: 'transparent',
  borderWidth: 2.5,
  pointRadius: 4,
  pointHoverRadius: 7,
  pointBackgroundColor: '#2563EB',
  tension: 0.3
}

// Break-even annotation (for ratio metric)
annotation: {
  breakeven: {
    type: 'line',
    yMin: 1.0,
    yMax: 1.0,
    borderColor: '#9ca3af',
    borderWidth: 1.5,
    borderDash: [6, 4],
    label: {
      display: true,
      content: 'Break-even',
      position: 'end',
      color: '#6b7280',
      font: { size: 12, family: 'Inter' },
      backgroundColor: 'rgba(250, 251, 252, 0.9)',
      padding: 4
    }
  }
}
```

---

## 10. Data Layer Changes

### Unisex Computation (new function in data-loader.js)

```javascript
/**
 * Compute unisex metrics by averaging male and female values.
 * Monthly benefit and replacement rate are sex-independent (identical M/F).
 * PV, ratio, IRR, death_age differ by sex and need averaging.
 */
getUnisexMetrics(cohortData, marital, birthYear) {
  const maleData = this.getMetricsForBirthYear(cohortData, 'male', marital, birthYear);
  const femaleData = this.getMetricsForBirthYear(cohortData, 'female', marital, birthYear);
  if (!maleData || !femaleData) return maleData || femaleData;

  return {
    monthly_benefit: maleData.monthly_benefit,  // identical M/F
    initial_real_benefit: maleData.initial_real_benefit,  // identical M/F
    repl_rate: maleData.repl_rate,  // identical M/F
    pv_benefits: (maleData.pv_benefits + femaleData.pv_benefits) / 2,
    pv_taxes: maleData.pv_taxes,  // identical M/F (same earnings)
    ratio: (maleData.ratio + femaleData.ratio) / 2,
    irr: (maleData.irr + femaleData.irr) / 2,
    death_age: (maleData.death_age + femaleData.death_age) / 2,
    couple_pv_benefits: maleData.couple_pv_benefits,  // already cross-sex
    couple_pv_taxes: maleData.couple_pv_taxes,
    couple_ratio: maleData.couple_ratio
  };
}

/**
 * Compute unisex cohort series (all birth years) for cohort charts.
 */
getUnisexCohortSeries(cohortData, marital) {
  const male = this.getCohortSeries(cohortData, 'male', marital);
  const female = this.getCohortSeries(cohortData, 'female', marital);
  if (!male || !female) return male || female;

  return {
    birth_years: male.birth_years,
    monthly_benefit: male.monthly_benefit,
    initial_real_benefit: male.initial_real_benefit,
    repl_rate: male.repl_rate,
    pv_benefits: male.pv_benefits.map((v, i) => (v + female.pv_benefits[i]) / 2),
    pv_taxes: male.pv_taxes,
    ratio: male.ratio.map((v, i) => (v + female.ratio[i]) / 2),
    irr: male.irr.map((v, i) => (v + female.irr[i]) / 2),
    death_age: male.death_age.map((v, i) => (v + female.death_age[i]) / 2)
  };
}
```

### Benefits Chart Unisex Handling

For the benefits-over-time chart in unisex mode:
- Use the male benefit series data (amounts are identical M/F)
- Clip at `Math.round(avg_death_age)` where `avg_death_age = (male_death + female_death) / 2`
- Death-age annotation label shows the averaged value

### Sex Selection State

Add `"unisex"` as a valid sex option. When sex is `"unisex"`, use the averaging functions above. When sex is `"male"` or `"female"`, use existing direct lookups.

---

## 11. Responsive / Mobile

### Breakpoints

| Range | Name | Layout |
|-------|------|--------|
| >= 1200px | Desktop | 48px sidebar strip + full content |
| 992-1199px | Tablet | No sidebar strip (reform icon in navbar), full-width content |
| < 768px | Mobile | Single column, stacked sections |

### Mobile Adaptations (< 768px)

```css
@media (max-width: 767px) {
  .hero-number { font-size: 2rem; }
  .hero-unit { font-size: 1.1rem; }
  .hero-sentence { font-size: 1rem; }
  .hero-ctas { flex-direction: column; gap: 0.75rem; }
  .hero-ctas .btn { width: 100%; }

  .chart-container { height: 240px !important; }

  .return-summary { flex-direction: column; gap: 1rem; }
  .return-ratio { min-width: unset; }

  .metric-selector {
    flex-wrap: nowrap;
    overflow-x: auto;
    -webkit-overflow-scrolling: touch;
    gap: 0.35rem;
    padding-bottom: 0.5rem;
  }
  .metric-btn { white-space: nowrap; flex-shrink: 0; }

  .sidebar-strip { display: none; }
  /* Reform access via bottom sheet triggered by floating button */
  .reform-fab {
    position: fixed;
    bottom: 1rem;
    right: 1rem;
    width: 48px;
    height: 48px;
    border-radius: 50%;
    background: var(--bg-dark);
    color: var(--text-on-dark);
    border: none;
    box-shadow: 0 2px 8px rgba(0,0,0,0.2);
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 1.2rem;
    z-index: 100;
  }
}

@media (min-width: 768px) {
  .reform-fab { display: none; }
}
```

### Mobile Chart.js Adjustments

```javascript
const isMobile = window.innerWidth < 768;
// In chart configs:
pointHoverRadius: isMobile ? 8 : 6
// Touch-friendly tooltip
interaction: {
  mode: isMobile ? 'nearest' : 'index',
  intersect: false
}
```

### Mobile Data Tables

On screens < 768px, transform table rows into card layout:

```css
@media (max-width: 767px) {
  .data-table thead { display: none; }
  .data-table tr {
    display: block;
    margin-bottom: 0.75rem;
    padding: 0.75rem;
    background: var(--bg-card);
    border-radius: 0.375rem;
    box-shadow: var(--shadow-card);
  }
  .data-table td {
    display: flex;
    justify-content: space-between;
    padding: 0.25rem 0;
    border: none;
    text-align: right;
  }
  .data-table td::before {
    content: attr(data-label);
    font-weight: 600;
    color: var(--text-secondary);
    text-align: left;
  }
}
```

Table cells need `data-label` attributes added by tableManager during rendering.

---

## 12. Notes & Methodology Panels

### Individual Tab Notes

```
All calculations under current law using 2025 Trustees Report assumptions.
Benefits assume claiming at age 65. Present values discounted at the Trustees'
real interest rate. Lifetime taxes include both employee and employer shares of
the payroll tax. Unisex figures average male and female mortality from the
Trustees' intermediate assumptions.
```

### Cohort Tab Notes

```
Each point represents a hypothetical worker with the selected earnings profile
who claims benefits at age 65. Benefit-tax ratio compares the present value of
lifetime benefits to lifetime taxes (including employer share), both discounted
to age 65. All dollar amounts in 2025 constant dollars.
```

### CSS

```css
.notes-panel {
  font-size: 0.75rem;
  color: var(--text-muted);
  padding: 0.75rem 1rem;
  line-height: 1.6;
  border-top: 1px solid var(--border);
  margin-top: 1.5rem;
}
```

---

## 13. File Manifest

Files that need modification:

| File | Changes |
|------|---------|
| `docs/index.html` | Complete restructure: hero section, narrative sections, collapsed sidebar, new tab labels |
| `docs/css/style.css` | Complete rewrite: light theme, new color system, hero styles, narrative layout, responsive |
| `docs/js/app.js` | Hero wiring, unisex default, tab switching, section rendering |
| `docs/js/chart-manager.js` | New defaults, death-age annotation, NMTR dual-color, cohort single-chart, tooltip callbacks |
| `docs/js/data-loader.js` | Add `getUnisexMetrics()`, `getUnisexCohortSeries()`, handle "unisex" sex option |
| `docs/js/ui-controls.js` | Hero inline selects, sex/marital secondary controls, metric selector buttons, collapsed sidebar |
| `docs/js/table-manager.js` | Mobile card layout (data-label attributes), updated column headers |
| `docs/js/formatters.js` | No changes expected (formatters are adequate) |

Files that do NOT change:
- All data files (`docs/data/**/*.json`)
- `docs/data/manifest.json`

---

## 14. Phase 2 Ideas

These are NOT part of the initial redesign but are documented for future consideration:

1. **Multi-worker cohort overlay:** Show all 6 worker types as lines on the cohort chart, with the selected one highlighted and others dimmed. Tells the progressivity story visually. Requires a 6-color accessible palette.

2. **Reform sidebar expansion:** When reforms are enabled, the 48px strip expands to 320px on click. The full reform radio button HTML is preserved in the DOM (hidden) and just needs `display: block` + re-enabling.

3. **Animated transitions:** When switching cohort metrics, animate the chart data transition (Chart.js supports this natively). When switching tabs, subtle fade transition.

4. **Comparison mode:** "Compare two workers side by side" -- split screen showing two different worker type / birth year combinations.

5. **Shareable URLs:** Encode the current configuration (worker type, birth year, sex, marital, active tab, cohort metric) in the URL hash so users can share specific views.

6. **Print stylesheet:** Clean print layout for the current view, stripping sidebar and interactive elements.
