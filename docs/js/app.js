// =============================================================================
// App — Main controller, event wiring, data flow
// =============================================================================

// State
let currentTab = 'individual';
let indDataCache = {};   // Cached data for current individual config
let cohDataCache = {};   // Cached data for current cohort config

// =============================================================================
// Initialization
// =============================================================================

document.addEventListener('DOMContentLoaded', async () => {
  // Initialize DataLoader
  await dataLoader.init();

  // Populate dropdowns
  populateWorkerTypes('indWorkerType', 'medium');
  populateWorkerTypes('cohWorkerType', 'medium');
  populateClaimAges('indClaimAge', 65);
  populateClaimAges('cohClaimAge', 65);
  populateBirthYears('indBirthYear', 1939, 2010, 1960);

  // Wire up input change listeners
  const indInputs = ['indBirthYear', 'indClaimAge', 'indWorkerType', 'indSex'];
  indInputs.forEach(id => {
    document.getElementById(id)?.addEventListener('change', onInputChange);
  });

  const cohInputs = ['cohWorkerType', 'cohSex', 'cohClaimAge', 'cohReplType'];
  cohInputs.forEach(id => {
    document.getElementById(id)?.addEventListener('change', onInputChange);
  });

  // Wire up reform radio buttons
  document.querySelectorAll('input[name="reform"]').forEach(radio => {
    radio.addEventListener('change', () => {
      updateReformBadge(getSelectedReform());
      onInputChange();
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
  const sex = document.getElementById('indSex').value;
  const claimAge = parseInt(document.getElementById('indClaimAge').value);
  const birthYear = parseInt(document.getElementById('indBirthYear').value);
  const reform = getSelectedReform();

  // Show loading
  showLoading('benefitsChartContainer');
  showLoading('nmtrChartContainer');

  // Fetch data (metrics always, benefits and marginal lazy-loaded)
  const [metricsData, benefitsData, marginalData] = await Promise.all([
    dataLoader.getIndividualMetrics(type, sex, claimAge),
    dataLoader.getIndividualBenefits(type, sex, claimAge),
    dataLoader.getIndividualMarginal(type, sex, claimAge)
  ]);

  // Cache for re-renders (e.g., nominal/real toggle)
  indDataCache = { metricsData, benefitsData, marginalData, type, sex, claimAge, birthYear, reform };

  hideLoading('benefitsChartContainer');
  hideLoading('nmtrChartContainer');

  // Extract data for selected birth year
  const baselineMetrics = dataLoader.getMetricsForBirthYear(metricsData, 'baseline', birthYear);
  const reformMetrics = reform ? dataLoader.getMetricsForBirthYear(metricsData, reform, birthYear) : null;

  // Update metric cards
  updateIndividualMetrics(baselineMetrics, reformMetrics, reform);

  // Benefits chart
  renderIndividualBenefitsChart();

  // NMTR chart
  const baselineMarginal = dataLoader.getMarginalData(marginalData, 'baseline', birthYear);
  const reformMarginal = reform ? dataLoader.getMarginalData(marginalData, reform, birthYear) : null;
  const reformLabel = reform ? dataLoader.getReformLabel(reform) : null;

  if (baselineMarginal) {
    chartManager.renderNMTRChart(baselineMarginal, reformMarginal, reformLabel);

    // Update mean NMTR and marginal IRR metrics from marginal data
    updateMarginalMetrics(baselineMarginal, reformMarginal, reform);
  }

  // Tables
  const baselineSeries = dataLoader.getBenefitSeries(benefitsData, 'baseline', birthYear);
  const reformSeries = reform ? dataLoader.getBenefitSeries(benefitsData, reform, birthYear) : null;
  tableManager.renderBenefitsTable(baselineSeries, reformSeries, reformLabel);
  tableManager.renderMarginalTable(baselineMarginal, reformMarginal, reformLabel);
}

/**
 * Render/re-render benefits chart (called on view toggle too)
 */
function renderIndividualBenefitsChart() {
  const { benefitsData, birthYear, reform } = indDataCache;
  if (!benefitsData) return;

  const baselineSeries = dataLoader.getBenefitSeries(benefitsData, 'baseline', birthYear);
  const reformSeries = reform ? dataLoader.getBenefitSeries(benefitsData, reform, birthYear) : null;
  const reformLabel = reform ? dataLoader.getReformLabel(reform) : null;

  chartManager.renderBenefitsChart(baselineSeries, reformSeries, reformLabel, currentBenefitView);
}

/**
 * Update individual metric cards
 */
function updateIndividualMetrics(baseline, reformed, reformName) {
  const metrics = [
    { id: 'metricMonthlyBenefit', key: 'monthly_benefit', format: v => Fmt.currency(v), label: 'Monthly Benefit' },
    { id: 'metricPVBenefits', key: 'pv_benefits', format: v => Fmt.currency(v, { compact: true }), label: 'PV Lifetime Benefits' },
    { id: 'metricPVTaxes', key: 'pv_taxes', format: v => Fmt.currency(v, { compact: true }), label: 'PV Lifetime Taxes' },
    { id: 'metricRatio', key: 'ratio', format: v => Fmt.number(v), label: 'Benefit-Tax Ratio' },
    { id: 'metricIRR', key: 'irr', format: v => Fmt.percent(v), label: 'Internal Rate of Return' }
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

    if (reformName && refVal != null) {
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

/**
 * Update marginal-specific metrics (mean NMTR, marginal IRR)
 */
function updateMarginalMetrics(baselineMarginal, reformMarginal, reformName) {
  // Mean NMTR
  const meanNMTR = (data) => {
    if (!data?.nmtr) return null;
    const valid = data.nmtr.filter(v => v != null && !isNaN(v));
    return valid.length > 0 ? valid.reduce((a, b) => a + b, 0) / valid.length : null;
  };

  const baseMean = meanNMTR(baselineMarginal);
  const refMean = reformName ? meanNMTR(reformMarginal) : null;

  updateSingleMetric('metricMeanNMTR', 'Mean NMTR', baseMean, refMean, reformName,
    v => Fmt.percent(v), { positiveIsGood: false });

  // Marginal IRR (last working year)
  const lastIRR = (data) => {
    if (!data?.marginal_irr) return null;
    const arr = data.marginal_irr;
    for (let i = arr.length - 1; i >= 0; i--) {
      if (arr[i] != null && !isNaN(arr[i])) return arr[i];
    }
    return null;
  };

  const baseIRR = lastIRR(baselineMarginal);
  const refIRR = reformName ? lastIRR(reformMarginal) : null;

  updateSingleMetric('metricMargIRR', 'Marginal IRR (Last Year)', baseIRR, refIRR, reformName,
    v => Fmt.percent(v));
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
// Cohort Tab
// =============================================================================

async function updateCohortTab() {
  const type = document.getElementById('cohWorkerType').value;
  const sex = document.getElementById('cohSex').value;
  const claimAge = parseInt(document.getElementById('cohClaimAge').value);
  const replType = document.getElementById('cohReplType').value;
  const minYear = parseInt(document.getElementById('cohBirthYearMin').value);
  const maxYear = parseInt(document.getElementById('cohBirthYearMax').value);
  const reform = getSelectedReform();

  // Fetch cohort data
  const cohortData = await dataLoader.getCohortData(type, sex, claimAge);
  if (!cohortData) return;

  cohDataCache = { cohortData, type, sex, claimAge, replType, minYear, maxYear, reform };

  // Extract baseline and reform data, filtered by range
  const baselineData = dataLoader.getCohortSeries(cohortData, 'baseline', minYear, maxYear);
  const reformData = reform ? dataLoader.getCohortSeries(cohortData, reform, minYear, maxYear) : null;
  const reformLabel = reform ? dataLoader.getReformLabel(reform) : null;

  // Render charts
  chartManager.renderCohortCharts(baselineData, reformData, reformLabel, replType);

  // Update summary metrics
  updateCohortMetrics(baselineData, reformData, reformLabel, replType);

  // Update table
  tableManager.renderCohortTable(baselineData, reformData, reformLabel, replType);
}

/**
 * Update cohort summary metric cards
 */
function updateCohortMetrics(baselineData, reformData, reformLabel, replField) {
  if (!baselineData) return;

  const avg = (arr) => {
    if (!arr) return null;
    const valid = arr.filter(v => v != null && !isNaN(v));
    return valid.length > 0 ? valid.reduce((a, b) => a + b, 0) / valid.length : null;
  };

  const metrics = [
    { id: 'cohMetricRepl', field: replField, label: 'Avg Replacement Rate', format: v => Fmt.percent(v) },
    { id: 'cohMetricPV', field: 'pv_benefits', label: 'Avg PV Benefits', format: v => Fmt.currency(v, { compact: true }) },
    { id: 'cohMetricRatio', field: 'ratio', label: 'Avg Benefit-Tax Ratio', format: v => Fmt.number(v) },
    { id: 'cohMetricIRR', field: 'irr', label: 'Avg IRR', format: v => Fmt.percent(v) }
  ];

  for (const m of metrics) {
    const baseAvg = avg(baselineData[m.field]);
    const refAvg = reformData ? avg(reformData[m.field]) : null;

    const opts = m.field === 'ratio' ? {} : {};
    updateSingleMetric(m.id, m.label, baseAvg, refAvg, reformLabel ? reformLabel : null, m.format, opts);
  }
}

// =============================================================================
// Loading indicators
// =============================================================================

function showLoading(containerId) {
  const container = document.getElementById(containerId);
  if (!container) return;

  // Don't double-add
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
