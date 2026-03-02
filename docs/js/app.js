// =============================================================================
// App — Main controller, event wiring, data flow
// =============================================================================

let indDataCache = {};
let cohDataCache = {};

// =============================================================================
// Initialization
// =============================================================================

document.addEventListener('DOMContentLoaded', async () => {
  try {
    await dataLoader.init();

    // Populate dropdowns (both tabs)
    populateWorkerTypes('indWorkerType', 'medium');
    populateWorkerTypes('cohWorkerType', 'medium');
    populateBirthYears('indBirthYear', 1940, 2010, 1960);

    // Wire up input change listeners — individual tab
    ['indBirthYear', 'indWorkerType'].forEach(id => {
      document.getElementById(id)?.addEventListener('change', onInputChange);
    });
    document.querySelectorAll('input[name="indSex"], input[name="indMarital"]').forEach(r => {
      r.addEventListener('change', onInputChange);
    });

    // Wire up input change listeners — cohort tab
    ['cohWorkerType'].forEach(id => {
      document.getElementById(id)?.addEventListener('change', onInputChange);
    });
    document.querySelectorAll('input[name="cohSex"], input[name="cohMarital"]').forEach(r => {
      r.addEventListener('change', onInputChange);
    });

    // Disable all reform buttons
    disableAllReforms();
    updateReformBadge();

    // Handle URL hash for tab routing
    const hash = window.location.hash.replace('#', '');
    if (hash === 'cohort') {
      switchTab('cohort');
    } else {
      switchTab('individual');
    }
  } catch (err) {
    console.error('Init error:', err);
  }
});

// =============================================================================
// Main input change handler
// =============================================================================

async function onInputChange() {
  const activeTab = document.getElementById('individualPane').classList.contains('active')
    ? 'individual' : 'cohort';
  if (activeTab === 'individual') {
    await updateIndividualTab();
  } else {
    await updateCohortTab();
  }
}

// =============================================================================
// Individual Tab
// =============================================================================

async function updateIndividualTab() {
  try {
    const type = document.getElementById('indWorkerType').value;
    const birthYear = parseInt(document.getElementById('indBirthYear').value);
    const sex = getIndSex();
    const marital = getIndMarital();

    showLoading('benefitsChartContainer');

    // Fetch all three data files for this worker type
    const [cohortData, benefitsData, nmtrData] = await Promise.all([
      dataLoader.getCohortData(type),
      dataLoader.getIndividualBenefits(type),
      dataLoader.getIndividualNMTR(type)
    ]);

    indDataCache = { cohortData, benefitsData, nmtrData, type, birthYear, sex, marital };

    hideLoading('benefitsChartContainer');

    // Extract metrics for selected configuration
    const metrics = dataLoader.getMetricsForBirthYear(cohortData, sex, marital, birthYear);
    updateIndividualMetrics(metrics, marital === 'married');

    // Benefits chart
    renderIndividualBenefitsChart();

    // NMTR chart
    renderIndividualNMTRChart();

    // Benefits table
    const benefitSeries = dataLoader.getBenefitSeries(benefitsData, sex, marital, birthYear);
    tableManager.renderBenefitsTable(benefitSeries);

  } catch (err) {
    console.error('updateIndividualTab error:', err);
  }
}

function renderIndividualBenefitsChart() {
  const { benefitsData, cohortData, birthYear, sex, marital } = indDataCache;
  if (!benefitsData) return;

  const series = dataLoader.getBenefitSeries(benefitsData, sex, marital, birthYear);
  const metrics = dataLoader.getMetricsForBirthYear(cohortData, sex, marital, birthYear);
  const deathAge = metrics?.death_age;

  chartManager.renderBenefitsChart(series, null, null, currentBenefitView, deathAge);
}

function renderIndividualNMTRChart() {
  const { nmtrData, birthYear, sex, marital } = indDataCache;
  if (!nmtrData) return;

  const nmtrSeries = dataLoader.getNMTRSeries(nmtrData, sex, marital, birthYear);
  if (nmtrSeries) {
    document.getElementById('nmtrRow').style.display = '';
    chartManager.renderNMTRChart(nmtrSeries, null, null);
  } else {
    document.getElementById('nmtrRow').style.display = 'none';
  }
}

function updateIndividualMetrics(data, isMarried) {
  const metrics = [
    { id: 'metricMonthlyBenefit', key: 'monthly_benefit', format: v => Fmt.currency(v), label: 'Monthly Benefit' },
    { id: 'metricPVBenefits', key: isMarried ? 'couple_pv_benefits' : 'pv_benefits', format: v => Fmt.currency(v, { compact: true }), label: isMarried ? 'PV Couple Benefits' : 'PV Lifetime Benefits' },
    { id: 'metricPVTaxes', key: isMarried ? 'couple_pv_taxes' : 'pv_taxes', format: v => Fmt.currency(v, { compact: true }), label: isMarried ? 'PV Couple Taxes' : 'PV Lifetime Taxes' },
    { id: 'metricRatio', key: isMarried ? 'couple_ratio' : 'ratio', format: v => Fmt.number(v), label: 'Benefit-Tax Ratio' },
    { id: 'metricIRR', key: 'irr', format: v => Fmt.percent(v), label: 'Internal Rate of Return' },
    { id: 'metricReplRate', key: 'repl_rate', format: v => Fmt.percent(v), label: 'Replacement Rate' }
  ];

  for (const m of metrics) {
    const el = document.getElementById(m.id);
    if (!el) continue;

    const val = data?.[m.key];
    if (val == null) {
      el.innerHTML = `<div class="metric-label">${m.label}</div><div class="metric-value">--</div>`;
      continue;
    }

    const colorClass = m.key.includes('ratio') ? (val >= 1 ? 'positive' : 'negative') :
                        m.key === 'irr' ? (val >= 0 ? 'positive' : 'negative') : '';
    el.innerHTML = `
      <div class="metric-label">${m.label}</div>
      <div class="metric-value ${colorClass}">${m.format(val)}</div>
    `;
  }
}

// =============================================================================
// Cohort Tab
// =============================================================================

async function updateCohortTab() {
  try {
    const type = document.getElementById('cohWorkerType').value;
    const sex = getCohSex();
    const marital = getCohMarital();

    const cohortData = await dataLoader.getCohortData(type);
    if (!cohortData) { console.error('Failed to load cohort data for', type); return; }

    cohDataCache = { cohortData, type, sex, marital };

    const series = dataLoader.getCohortSeries(cohortData, sex, marital);
    chartManager.renderCohortCharts(series, null, null);
    updateCohortMetrics(series);
    tableManager.renderCohortTable(series, null, null, 'repl_rate');

  } catch (err) {
    console.error('updateCohortTab error:', err);
  }
}

function updateCohortMetrics(data) {
  if (!data) return;

  const avg = (arr) => {
    if (!arr) return null;
    const valid = arr.filter(v => v != null && !isNaN(v));
    return valid.length > 0 ? valid.reduce((a, b) => a + b, 0) / valid.length : null;
  };

  const metrics = [
    { id: 'cohMetricRepl', field: 'repl_rate', label: 'Avg Replacement Rate', format: v => Fmt.percent(v) },
    { id: 'cohMetricPV', field: 'pv_benefits', label: 'Avg PV Benefits', format: v => Fmt.currency(v, { compact: true }) },
    { id: 'cohMetricRatio', field: 'ratio', label: 'Avg Benefit-Tax Ratio', format: v => Fmt.number(v) },
    { id: 'cohMetricIRR', field: 'irr', label: 'Avg IRR', format: v => Fmt.percent(v) }
  ];

  for (const m of metrics) {
    const el = document.getElementById(m.id);
    if (!el) continue;
    const val = avg(data[m.field]);
    el.innerHTML = val != null
      ? `<div class="metric-label">${m.label}</div><div class="metric-value">${m.format(val)}</div>`
      : `<div class="metric-label">${m.label}</div><div class="metric-value">--</div>`;
  }
}

// =============================================================================
// Loading indicators
// =============================================================================

function showLoading(containerId) {
  const container = document.getElementById(containerId);
  if (!container) return;
  if (container.querySelector('.loading-overlay')) return;
  const overlay = document.createElement('div');
  overlay.className = 'loading-overlay';
  overlay.innerHTML = '<div class="spinner"></div>';
  container.style.position = 'relative';
  container.appendChild(overlay);
}

function hideLoading(containerId) {
  const container = document.getElementById(containerId);
  if (!container) return;
  const overlay = container.querySelector('.loading-overlay');
  if (overlay) overlay.remove();
}
