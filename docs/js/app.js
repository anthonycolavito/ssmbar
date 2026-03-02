// =============================================================================
// App — Main controller, event wiring, data flow
// =============================================================================

// State
let currentTab = 'individual';
let indDataCache = {};
let cohDataCache = {};

// =============================================================================
// Initialization
// =============================================================================

document.addEventListener('DOMContentLoaded', async () => {
  await dataLoader.init();

  // Populate dropdowns
  populateWorkerTypes('indWorkerType', 'medium');
  populateWorkerTypes('cohWorkerType', 'medium');
  populateClaimAges('indClaimAge', 65);
  populateClaimAges('cohClaimAge', 65);
  populateBirthYears('indBirthYear', 1940, 2010, 1960);

  // Wire up input change listeners
  const indInputs = ['indBirthYear', 'indClaimAge', 'indWorkerType'];
  indInputs.forEach(id => {
    document.getElementById(id)?.addEventListener('change', onInputChange);
  });

  const cohInputs = ['cohWorkerType', 'cohClaimAge'];
  cohInputs.forEach(id => {
    document.getElementById(id)?.addEventListener('change', onInputChange);
  });

  // Wire up all reform radio buttons (6 groups)
  REFORM_RADIO_GROUPS.forEach(groupName => {
    document.querySelectorAll(`input[name="${groupName}"]`).forEach(radio => {
      radio.addEventListener('change', () => {
        updateReformBadge();
        onInputChange();
      });
    });
  });

  // Handle URL hash for tab routing
  const hash = window.location.hash.replace('#', '');
  if (hash === 'cohort') {
    switchTab('cohort');
  } else {
    switchTab('individual');
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
  const type = document.getElementById('indWorkerType').value;
  const birthYear = parseInt(document.getElementById('indBirthYear').value);
  const comboKey = getComboKey();

  showLoading('benefitsChartContainer');

  // Fetch cohort data (for metrics) and benefits data (for chart)
  const [cohortData, benefitsData] = await Promise.all([
    dataLoader.getCohortData(type),
    dataLoader.getIndividualBenefits(type)
  ]);

  indDataCache = { cohortData, benefitsData, type, birthYear, comboKey };

  hideLoading('benefitsChartContainer');

  // Extract metrics for selected birth year
  const baselineMetrics = dataLoader.getMetricsForBirthYear(cohortData, 'baseline', birthYear);
  const reformMetrics = comboKey !== 'baseline'
    ? dataLoader.getMetricsForBirthYear(cohortData, comboKey, birthYear) : null;

  // Update metric cards
  updateIndividualMetrics(baselineMetrics, reformMetrics, comboKey !== 'baseline' ? comboKey : null);

  // Benefits chart
  renderIndividualBenefitsChart();

  // Benefits table
  const baselineSeries = dataLoader.getBenefitSeries(benefitsData, 'baseline', birthYear);
  const reformSeries = comboKey !== 'baseline'
    ? dataLoader.getBenefitSeries(benefitsData, comboKey, birthYear) : null;
  const reformLabel = comboKey !== 'baseline' ? dataLoader.getReformLabel(comboKey) : null;
  tableManager.renderBenefitsTable(baselineSeries, reformSeries, reformLabel);
}

/**
 * Render/re-render benefits chart (called on view toggle too)
 */
function renderIndividualBenefitsChart() {
  const { benefitsData, cohortData, birthYear, comboKey } = indDataCache;
  if (!benefitsData) return;

  const baselineSeries = dataLoader.getBenefitSeries(benefitsData, 'baseline', birthYear);
  const reformSeries = comboKey !== 'baseline'
    ? dataLoader.getBenefitSeries(benefitsData, comboKey, birthYear) : null;
  const reformLabel = comboKey !== 'baseline' ? dataLoader.getReformLabel(comboKey) : null;

  // Get death age from cohort data for clipping
  const baselineMetrics = dataLoader.getMetricsForBirthYear(cohortData, 'baseline', birthYear);
  const deathAge = baselineMetrics?.death_age;

  chartManager.renderBenefitsChart(baselineSeries, reformSeries, reformLabel, currentBenefitView, deathAge);
}

/**
 * Update individual metric cards
 */
function updateIndividualMetrics(baseline, reformed, comboKey) {
  const metrics = [
    { id: 'metricMonthlyBenefit', key: 'monthly_benefit', format: v => Fmt.currency(v), label: 'Monthly Benefit' },
    { id: 'metricPVBenefits', key: 'pv_benefits', format: v => Fmt.currency(v, { compact: true }), label: 'PV Lifetime Benefits' },
    { id: 'metricPVTaxes', key: 'pv_taxes', format: v => Fmt.currency(v, { compact: true }), label: 'PV Lifetime Taxes' },
    { id: 'metricRatio', key: 'ratio', format: v => Fmt.number(v), label: 'Benefit-Tax Ratio' },
    { id: 'metricIRR', key: 'irr', format: v => Fmt.percent(v), label: 'Internal Rate of Return' },
    { id: 'metricReplRate', key: 'repl_rate', format: v => Fmt.percent(v), label: 'Replacement Rate' }
  ];

  for (const m of metrics) {
    const el = document.getElementById(m.id);
    if (!el) continue;

    const baseVal = baseline?.[m.key];
    const refVal = reformed?.[m.key];

    if (baseVal == null) {
      el.innerHTML = `<div class="metric-label">${m.label}</div><div class="metric-value">--</div>`;
      continue;
    }

    if (comboKey && refVal != null) {
      const pctChange = baseVal !== 0 ? ((refVal - baseVal) / Math.abs(baseVal)) * 100 : 0;
      const dirClass = Fmt.directionClass(pctChange);
      el.innerHTML = `
        <div class="metric-label">${m.label}</div>
        <div class="metric-comparison">
          <span class="baseline-val">${m.format(baseVal)}</span>
          <span class="arrow">&rarr;</span>
          <span class="reform-val ${dirClass}">${m.format(refVal)}</span>
        </div>
        <div class="metric-change ${dirClass}">${Fmt.change(pctChange)}</div>
      `;
    } else {
      const colorClass = m.key === 'ratio' ? (baseVal >= 1 ? 'positive' : 'negative') :
                          m.key === 'irr' ? (baseVal >= 0 ? 'positive' : 'negative') : '';
      el.innerHTML = `
        <div class="metric-label">${m.label}</div>
        <div class="metric-value ${colorClass}">${m.format(baseVal)}</div>
      `;
    }
  }
}

// =============================================================================
// Cohort Tab
// =============================================================================

async function updateCohortTab() {
  const type = document.getElementById('cohWorkerType').value;
  const comboKey = getComboKey();

  const cohortData = await dataLoader.getCohortData(type);
  if (!cohortData) return;

  cohDataCache = { cohortData, type, comboKey };

  // Extract baseline and reform data (full birth year range, no filtering)
  const baselineData = dataLoader.getCohortSeries(cohortData, 'baseline');
  const reformData = comboKey !== 'baseline'
    ? dataLoader.getCohortSeries(cohortData, comboKey) : null;
  const reformLabel = comboKey !== 'baseline' ? dataLoader.getReformLabel(comboKey) : null;

  // Render charts (hardcoded to repl_rate_real_all)
  chartManager.renderCohortCharts(baselineData, reformData, reformLabel);

  // Update summary metrics
  updateCohortMetrics(baselineData, reformData, reformLabel);

  // Update table
  tableManager.renderCohortTable(baselineData, reformData, reformLabel, 'repl_rate_real_all');
}

/**
 * Update cohort summary metric cards
 */
function updateCohortMetrics(baselineData, reformData, reformLabel) {
  if (!baselineData) return;

  const avg = (arr) => {
    if (!arr) return null;
    const valid = arr.filter(v => v != null && !isNaN(v));
    return valid.length > 0 ? valid.reduce((a, b) => a + b, 0) / valid.length : null;
  };

  const metrics = [
    { id: 'cohMetricRepl', field: 'repl_rate_real_all', label: 'Avg Replacement Rate', format: v => Fmt.percent(v) },
    { id: 'cohMetricPV', field: 'pv_benefits', label: 'Avg PV Benefits', format: v => Fmt.currency(v, { compact: true }) },
    { id: 'cohMetricRatio', field: 'ratio', label: 'Avg Benefit-Tax Ratio', format: v => Fmt.number(v) },
    { id: 'cohMetricIRR', field: 'irr', label: 'Avg IRR', format: v => Fmt.percent(v) }
  ];

  for (const m of metrics) {
    const baseAvg = avg(baselineData[m.field]);
    const refAvg = reformData ? avg(reformData[m.field]) : null;
    updateSingleMetric(m.id, m.label, baseAvg, refAvg, reformLabel, m.format);
  }
}

/**
 * Generic metric card updater
 */
function updateSingleMetric(elementId, label, baseVal, refVal, reformName, formatFn, opts = {}) {
  const el = document.getElementById(elementId);
  if (!el) return;

  if (baseVal == null) {
    el.innerHTML = `<div class="metric-label">${label}</div><div class="metric-value">--</div>`;
    return;
  }

  if (reformName && refVal != null) {
    const diff = refVal - baseVal;
    const pctChange = baseVal !== 0 ? (diff / Math.abs(baseVal)) * 100 : 0;
    const dirClass = Fmt.directionClass(pctChange, opts);
    el.innerHTML = `
      <div class="metric-label">${label}</div>
      <div class="metric-comparison">
        <span class="baseline-val">${formatFn(baseVal)}</span>
        <span class="arrow">&rarr;</span>
        <span class="reform-val ${dirClass}">${formatFn(refVal)}</span>
      </div>
      <div class="metric-change ${dirClass}">${Fmt.change(pctChange)}</div>
    `;
  } else {
    el.innerHTML = `
      <div class="metric-label">${label}</div>
      <div class="metric-value">${formatFn(baseVal)}</div>
    `;
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
