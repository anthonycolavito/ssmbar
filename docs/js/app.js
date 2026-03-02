// =============================================================================
// App — Main controller, event wiring, data flow
// =============================================================================

let appDataCache = {};

// =============================================================================
// Initialization
// =============================================================================

document.addEventListener('DOMContentLoaded', async () => {
  try {
    await dataLoader.init();

    // Wire up hero control change listeners
    ['heroWorkerType', 'heroBirthYear', 'heroSex', 'heroMarital', 'heroSpouseType'].forEach(id => {
      document.getElementById(id)?.addEventListener('change', onHeroChange);
    });

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
// Main change handler — triggered by any hero control change
// =============================================================================

async function onHeroChange() {
  const type = getHeroWorkerType();
  const birthYear = getHeroBirthYear();
  const sex = getHeroSex();
  const marital = getHeroMarital();
  const spouseType = marital === 'married' ? getHeroSpouseType() : null;

  // Enforce sex/marital constraints and spouse type visibility
  updateHeroConstraints();

  // Fetch data
  const [cohortData, benefitsData, nmtrData] = await Promise.all([
    dataLoader.getCohortData(type),
    dataLoader.getIndividualBenefits(type),
    dataLoader.getIndividualNMTR(type)
  ]);

  appDataCache = { cohortData, benefitsData, nmtrData, type, birthYear, sex, marital, spouseType };

  // Update hero
  updateHero();

  // Update active tab
  const activeTab = document.getElementById('individualPane').classList.contains('active')
    ? 'individual' : 'cohort';
  if (activeTab === 'individual') {
    updateIndividualTab();
  } else {
    updateCohortTab();
  }
}

// =============================================================================
// Hero Section
// =============================================================================

function updateHero() {
  const { cohortData, birthYear, sex, marital, spouseType } = appDataCache;
  if (!cohortData) return;

  const metrics = sex === 'unisex'
    ? dataLoader.getUnisexMetrics(cohortData, marital, birthYear, spouseType)
    : dataLoader.getMetricsForBirthYear(cohortData, sex, marital, birthYear, spouseType);

  if (!metrics) return;

  // Monthly benefit
  const nomEl = document.getElementById('heroNominalBenefit');
  if (nomEl) {
    nomEl.textContent = metrics.monthly_benefit != null
      ? Math.round(metrics.monthly_benefit).toLocaleString('en-US')
      : '--';
  }

  // Real benefit subtitle
  const realEl = document.getElementById('heroRealBenefit');
  if (realEl && metrics.initial_real_benefit != null) {
    const realVal = Math.round(metrics.initial_real_benefit).toLocaleString('en-US');
    realEl.textContent = `$${realVal}/month in today's dollars`;
  }

  // Replacement rate
  const replEl = document.getElementById('heroReplacement');
  if (replEl && metrics.repl_rate != null) {
    replEl.innerHTML = `That replaces <strong>${(metrics.repl_rate * 100).toFixed(1)}%</strong> of pre-retirement earnings`;
  }

  // Show sex insight when unisex is selected
  const insightEl = document.getElementById('sexInsight');
  if (insightEl) {
    insightEl.style.display = sex === 'unisex' ? '' : 'none';
  }
}

// =============================================================================
// Individual Tab
// =============================================================================

function updateIndividualTab() {
  renderIndividualBenefitsChart();
  renderIndividualNMTRChart();
  updateReturnSection();
  renderBenefitsTable();
}

function renderIndividualBenefitsChart() {
  const { benefitsData, cohortData, birthYear, sex, marital, spouseType } = appDataCache;
  if (!benefitsData) return;

  // For unisex, use male series (amounts are identical) and average death age
  const lookupSex = sex === 'unisex' ? 'male' : sex;
  const series = dataLoader.getBenefitSeries(benefitsData, lookupSex, marital, birthYear, spouseType);

  let deathAge = null;
  if (sex === 'unisex') {
    const metrics = dataLoader.getUnisexMetrics(cohortData, marital, birthYear, spouseType);
    deathAge = metrics?.death_age;
  } else {
    const metrics = dataLoader.getMetricsForBirthYear(cohortData, sex, marital, birthYear, spouseType);
    deathAge = metrics?.death_age;
  }

  chartManager.renderBenefitsChart(series, currentBenefitView, deathAge);
}

function renderIndividualNMTRChart() {
  const { nmtrData, birthYear, sex, marital, spouseType } = appDataCache;
  if (!nmtrData) return;

  const lookupSex = sex === 'unisex' ? 'male' : sex;
  const nmtrSeries = dataLoader.getNMTRSeries(nmtrData, lookupSex, marital, birthYear, spouseType);

  const section = document.getElementById('nmtrSection');
  if (nmtrSeries) {
    if (section) section.style.display = '';
    chartManager.renderNMTRChart(nmtrSeries);
    updateNMTRInsight(nmtrSeries);
  } else {
    if (section) section.style.display = 'none';
  }
}

function updateNMTRInsight(nmtrSeries) {
  const insightEl = document.getElementById('nmtrInsight');
  if (!insightEl || !nmtrSeries) return;

  const nmtrVals = nmtrSeries.nmtr.filter(v => v != null);
  const offsetCount = nmtrVals.filter(v => v < 0).length;
  const pct = Math.round((offsetCount / nmtrVals.length) * 100);

  insightEl.innerHTML = `For this worker, <strong>${pct}%</strong> of career Social Security taxes were fully offset by increased future benefits.`;
}

function updateReturnSection() {
  const { cohortData, birthYear, sex, marital, spouseType } = appDataCache;
  if (!cohortData) return;

  const isMarried = marital === 'married';
  const metrics = sex === 'unisex'
    ? dataLoader.getUnisexMetrics(cohortData, marital, birthYear, spouseType)
    : dataLoader.getMetricsForBirthYear(cohortData, sex, marital, birthYear, spouseType);

  if (!metrics) return;

  const ratio = isMarried ? metrics.couple_ratio : metrics.ratio;
  const pvBen = isMarried ? metrics.couple_pv_benefits : metrics.pv_benefits;
  const pvTax = isMarried ? metrics.couple_pv_taxes : metrics.pv_taxes;
  const irr = metrics.irr;

  const ratioEl = document.getElementById('ratioValue');
  const pvBenEl = document.getElementById('pvBenefitsValue');
  const pvTaxEl = document.getElementById('pvTaxesValue');
  const irrEl = document.getElementById('irrValue');

  if (ratioEl) ratioEl.textContent = ratio != null ? ratio.toFixed(2) : '--';
  if (pvBenEl) pvBenEl.textContent = pvBen != null ? Fmt.currency(pvBen, { compact: true }) : '--';
  if (pvTaxEl) pvTaxEl.textContent = pvTax != null ? Fmt.currency(pvTax, { compact: true }) : '--';
  if (irrEl) irrEl.textContent = irr != null ? Fmt.percent(irr) : '--';
}

function renderBenefitsTable() {
  const { benefitsData, sex, marital, birthYear, spouseType } = appDataCache;
  if (!benefitsData) return;
  const lookupSex = sex === 'unisex' ? 'male' : sex;
  const series = dataLoader.getBenefitSeries(benefitsData, lookupSex, marital, birthYear, spouseType);
  tableManager.renderBenefitsTable(series);
}

// =============================================================================
// Cohort Tab
// =============================================================================

function updateCohortTab() {
  renderCohortChart();
  updateCohortSummary();
  renderCohortTable();
}

function renderCohortChart() {
  const { cohortData, sex, marital, spouseType } = appDataCache;
  if (!cohortData) return;

  const series = sex === 'unisex'
    ? dataLoader.getUnisexCohortSeries(cohortData, marital, spouseType)
    : dataLoader.getCohortSeries(cohortData, sex, marital, spouseType);

  chartManager.renderCohortHeroChart(series, currentCohortMetric);
}

function updateCohortSummary() {
  const { cohortData, sex, marital, spouseType } = appDataCache;
  if (!cohortData) return;

  const series = sex === 'unisex'
    ? dataLoader.getUnisexCohortSeries(cohortData, marital, spouseType)
    : dataLoader.getCohortSeries(cohortData, sex, marital, spouseType);

  if (!series) return;

  const summaryEl = document.getElementById('cohortSummary');
  if (!summaryEl) return;

  const data = series[currentCohortMetric];
  if (!data || data.length === 0) { summaryEl.innerHTML = ''; return; }

  const valid = data.filter(v => v != null && !isNaN(v));
  const avg = valid.reduce((a, b) => a + b, 0) / valid.length;
  const min = Math.min(...valid);
  const max = Math.max(...valid);
  const latest = data[data.length - 1];

  const fmt = getCohortMetricFormatter(currentCohortMetric);

  summaryEl.innerHTML = `
    <div class="cohort-stat">
      <div class="stat-label">Average</div>
      <div class="stat-value">${fmt(avg)}</div>
    </div>
    <div class="cohort-stat">
      <div class="stat-label">Earliest (${series.birth_years[0]})</div>
      <div class="stat-value">${fmt(data[0])}</div>
    </div>
    <div class="cohort-stat">
      <div class="stat-label">Latest (${series.birth_years[series.birth_years.length - 1]})</div>
      <div class="stat-value">${fmt(latest)}</div>
    </div>
  `;
}

function getCohortMetricFormatter(metric) {
  switch (metric) {
    case 'ratio': return (v) => v != null ? v.toFixed(2) : '--';
    case 'initial_real_benefit': return (v) => v != null ? Fmt.currency(v) : '--';
    case 'repl_rate': return (v) => v != null ? Fmt.percent(v) : '--';
    case 'pv_benefits': return (v) => v != null ? Fmt.currency(v, { compact: true }) : '--';
    case 'irr': return (v) => v != null ? Fmt.percent(v) : '--';
    default: return (v) => v != null ? String(v) : '--';
  }
}

function renderCohortTable() {
  const { cohortData, sex, marital, spouseType } = appDataCache;
  if (!cohortData) return;

  const series = sex === 'unisex'
    ? dataLoader.getUnisexCohortSeries(cohortData, marital, spouseType)
    : dataLoader.getCohortSeries(cohortData, sex, marital, spouseType);

  tableManager.renderCohortTable(series);
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
