// =============================================================================
// App — Entry point. Wires data, controls, hero, charts, summary cards, table.
// =============================================================================

const SPOUSE_PHRASE = {
  none:     'filing as a single individual',
  very_low: 'married to a very-low-earning spouse',
  low:      'married to a low-earning spouse',
  medium:   'married to a medium-earning spouse',
  high:     'married to a high-earning spouse',
  max:      'married to a maximum-earning spouse'
};

const WORKER_LABEL_LOWER = {
  very_low: 'very-low',
  low:      'low',
  medium:   'medium',
  high:     'high',
  max:      'maximum'
};

document.addEventListener('DOMContentLoaded', async () => {
  try {
    await dataLoader.init();
  } catch (err) {
    console.error('Failed to load data:', err);
    showLoadError(err);
    return;
  }

  // Methodology footer: "Built {date}"
  const m = dataLoader.meta();
  if (m && m.generated) {
    const date = new Date(m.generated);
    const display = isNaN(date) ? m.generated
      : date.toLocaleDateString(undefined, { year: 'numeric', month: 'short', day: 'numeric' });
    const el = document.getElementById('metaGenerated');
    if (el) el.textContent = display;
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
  renderHero(cfg, state);
  renderSummaryCards(cfg);
  renderLifetimeProfile(cfg, state);
  renderAnnualBenefitsChart(cfg, state.real);
  renderNetTaxRateChart(cfg);
  tableManager.render(cfg);

  if (!document.getElementById('panel-cohort').hidden) {
    renderCohortCharts(state);
  }
}

function handleTabChange(tab) {
  const state = uiControls.getState();
  if (tab === 'cohort') renderCohortCharts(state);
}

// -----------------------------------------------------------------------------
// Hero
// -----------------------------------------------------------------------------

function renderHero(cfg, state) {
  const workerWord = WORKER_LABEL_LOWER[state.workerType] || state.workerType;
  const spousePhrase = SPOUSE_PHRASE[state.spouseType] || '';
  const ledePieces = [`A <strong>${workerWord}</strong> earner born in <strong>${state.birthYear}</strong>`];
  if (state.spouseType !== 'none') ledePieces.push(spousePhrase);
  const lede = `${ledePieces.join(', ')}, can expect at age 65:`;

  document.getElementById('heroLede').innerHTML = lede;
  document.getElementById('heroValue').textContent = Fmt.currency(cfg.summary.monthly_real_at_65);

  const yrs = (cfg.summary.death_age != null) ? cfg.summary.death_age - 65 : null;
  const rrCareerPct = Fmt.percent(cfg.summary.rep_rate_career);
  const subParts = ['in 2026 dollars'];
  subParts.push(`Replaces about <strong>${rrCareerPct}</strong> of average career earnings`);
  if (yrs != null) subParts.push(`Expected to be received for ~<strong>${yrs}</strong> years from age 65`);
  document.getElementById('heroSubline').innerHTML = subParts.join(' · ');
}

// -----------------------------------------------------------------------------
// Summary cards
// -----------------------------------------------------------------------------

function renderSummaryCards(cfg) {
  const s = cfg.summary;
  const cards = [
    {
      label: 'First-year monthly benefit',
      value: Fmt.currency(s.monthly_real_at_65),
      subtitle: 'Real 2026 dollars at age 65. Real benefit drifts up over retirement (CPI-W COLA vs. GDP-PI deflator).'
    },
    {
      label: 'PV Lifetime Benefits',
      value: Fmt.currency(s.pv_benefits)
    },
    {
      label: 'PV Lifetime Taxes',
      value: Fmt.currency(s.pv_taxes)
    },
    {
      label: 'Benefit / Tax Ratio',
      value: (s.ben_tax_ratio == null) ? '--' : s.ben_tax_ratio.toFixed(2)
    },
    {
      label: 'Replacement Rate (Career)',
      value: Fmt.percent(s.rep_rate_career),
      subtitle: "First-year benefit ÷ worker's average real lifetime earnings.",
      info: "How much of the worker's own real average career earnings the initial Social Security benefit replaces. This is the SSA convention used in the Trustees Report scaled-earner tables."
    },
    {
      label: 'Replacement Rate (AWI)',
      value: Fmt.percent(s.rep_rate_awi),
      subtitle: "First-year benefit ÷ economy-wide average wage in claim year.",
      info: "How the initial benefit compares to overall U.S. average earnings in the year the worker claims. Closer to the everyday meaning of 'how generous is Social Security right now,' but differs from the SSA scaled-earner replacement rate."
    }
  ];

  const host = document.getElementById('summaryCards');
  host.innerHTML = cards.map(c => `
    <div class="summary-card">
      <div class="summary-label">${c.label}${c.info ? `<span class="summary-info" title="${escapeAttr(c.info)}"><i class="bi bi-info-circle"></i></span>` : ''}</div>
      <div class="summary-value">${c.value}</div>
      ${c.subtitle ? `<div class="summary-subtitle">${c.subtitle}</div>` : ''}
    </div>
  `).join('');
}

function escapeAttr(s) {
  return String(s).replace(/&/g, '&amp;').replace(/"/g, '&quot;').replace(/</g, '&lt;');
}

// -----------------------------------------------------------------------------
// Charts — Individual tab
// -----------------------------------------------------------------------------

function renderLifetimeProfile(cfg, state) {
  const profile = dataLoader.getLifetimeProfile(state.workerType, state.spouseType, state.birthYear);
  chartManager.lifetimeProfileChart('lifetimeProfileChart', {
    ages:           profile.ages,
    values:         profile.values,
    transitionIdx:  profile.transitionIdx,
    leAge:          profile.leAge
  });
}

function renderAnnualBenefitsChart(cfg, real) {
  const data = real ? cfg.annual.real : cfg.annual.nominal;
  const leAge = cfg.summary.death_age;
  const fadeIdx = (leAge != null) ? cfg.annual.ages.indexOf(leAge) : null;
  const subtitle = real
    ? `Real 2026 dollars (GDP price index)${leAge ? `. Dashed segment beyond age ${leAge} = post-life-expectancy projection.` : '.'}`
    : 'Nominal dollars (year of receipt).';
  document.getElementById('annualBenefitsSubtitle').textContent = subtitle;

  chartManager.lineChart('annualBenefitsChart', {
    labels: cfg.annual.ages,
    data,
    yFormat: 'currency',
    leMarker: leAge,
    fadeAfterIdx: (fadeIdx != null && fadeIdx >= 0) ? fadeIdx : null
  });
}

function renderNetTaxRateChart(cfg) {
  chartManager.netTaxRateChart('netTaxRateChart', {
    ages:   cfg.nmtr.ages,
    values: cfg.nmtr.values
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

  // Two-color encoding for Benefit/Tax Ratio: blue ≥ 1.0, amber < 1.0.
  const ratio = dataLoader.getCohortSeries(w, s, 'ben_tax_ratio');
  chartManager.barChart('cohortRatioChart', {
    labels: ratio.years, data: ratio.values, yFormat: 'number', twoColorThreshold: 1.0
  });
}

// -----------------------------------------------------------------------------
// Errors
// -----------------------------------------------------------------------------

function showLoadError(err) {
  const host = document.querySelector('.app-container');
  host.innerHTML = `<div class="alert alert-danger m-4">Failed to load dashboard data: ${err.message}</div>`;
}
