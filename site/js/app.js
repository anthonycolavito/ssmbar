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
  renderSummaryCards(cfg);
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
// Summary cards
// -----------------------------------------------------------------------------

function renderSummaryCards(cfg) {
  const s = cfg.summary;
  const cards = [
    { label: 'Monthly Benefit at 65',
      value: Fmt.currency(s.monthly_real_at_65),
      subtitle: '(real $, age 65)' },
    { label: 'PV Lifetime Benefits',
      value: Fmt.currency(s.pv_benefits) },
    { label: 'PV Lifetime Taxes',
      value: Fmt.currency(s.pv_taxes) },
    { label: 'Benefit / Tax Ratio',
      value: (s.ben_tax_ratio == null) ? '--' : s.ben_tax_ratio.toFixed(2) },
    { label: 'Replacement Rate (Career)',
      value: Fmt.percent(s.rep_rate_career),
      subtitle: "Initial benefit as a share of the worker's average real career earnings." },
    { label: 'Replacement Rate (AWI)',
      value: Fmt.percent(s.rep_rate_awi),
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
    yFormat: 'percent'
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
    labels: ratio.years, data: ratio.values, yFormat: 'number'
  });
}

// -----------------------------------------------------------------------------
// Errors
// -----------------------------------------------------------------------------

function showLoadError(err) {
  const host = document.querySelector('.app-container');
  host.innerHTML = `<div class="alert alert-danger m-4">Failed to load dashboard data: ${err.message}</div>`;
}
