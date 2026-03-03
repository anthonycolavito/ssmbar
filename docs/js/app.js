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

    // Open the PIA reform category by default
    toggleReformCategory('pia');

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
  const comboKey = getActiveComboKey();

  // Enforce sex/marital constraints and spouse type visibility
  updateHeroConstraints();

  // Fetch baseline data
  const fetches = [
    dataLoader.getCohortData(type),
    dataLoader.getIndividualBenefits(type),
    dataLoader.getIndividualNMTR(type)
  ];

  // Fetch reform data if a reform is active
  if (comboKey) {
    fetches.push(dataLoader.getReformCohortData(type));
    fetches.push(dataLoader.getReformIndividualData(type));
    fetches.push(dataLoader.getReformNMTRData(type));
  }

  const results = await Promise.all(fetches);
  const [cohortData, benefitsData, nmtrData] = results;
  const reformCohortData = comboKey ? results[3] : null;
  const reformIndData = comboKey ? results[4] : null;
  const reformNmtrData = comboKey ? results[5] : null;

  appDataCache = {
    cohortData, benefitsData, nmtrData,
    reformCohortData, reformIndData, reformNmtrData,
    type, birthYear, sex, marital, spouseType, comboKey
  };

  // Update hero
  updateHero();

  // Show/hide reform married note
  const reformMarriedNote = document.getElementById('reformMarriedNote');
  if (reformMarriedNote) {
    reformMarriedNote.style.display = (comboKey && marital === 'married') ? '' : 'none';
  }

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
  const { cohortData, reformCohortData, birthYear, sex, marital, spouseType, comboKey } = appDataCache;
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
    insightEl.style.display = (sex === 'unisex' && !comboKey) ? '' : 'none';
  }

  // Reform comparison in hero
  const reformEl = document.getElementById('heroReformComparison');
  if (reformEl) {
    if (comboKey && reformCohortData) {
      const reformMetrics = dataLoader.getReformMetrics(reformCohortData, comboKey, birthYear);
      if (reformMetrics && reformMetrics.monthly_benefit != null) {
        const baselineMonthly = metrics.monthly_benefit;
        const reformMonthly = reformMetrics.monthly_benefit;
        const delta = reformMonthly - baselineMonthly;
        const deltaPct = baselineMonthly > 0 ? (delta / baselineMonthly * 100) : 0;
        const sign = delta >= 0 ? '+' : '';
        const dirClass = delta < 0 ? 'negative' : (delta > 0 ? 'positive' : '');
        const reformLabel = getActiveReformLabel();
        reformEl.innerHTML = `
          <div class="reform-label">${reformLabel}</div>
          <span>$${Math.round(reformMonthly).toLocaleString('en-US')}/month</span>
          <span class="reform-delta ${dirClass}">(${sign}$${Math.abs(Math.round(delta)).toLocaleString('en-US')}, ${sign}${deltaPct.toFixed(1)}%)</span>
        `;
        reformEl.style.display = '';
      } else {
        // Reform has no data for this birth year (not affected)
        reformEl.innerHTML = '<span class="reform-label">' + (getActiveReformLabel() || '') + '</span> <span>No impact for this birth year</span>';
        reformEl.style.display = '';
      }
    } else {
      reformEl.style.display = 'none';
      reformEl.innerHTML = '';
    }
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
  const { benefitsData, reformIndData, cohortData, birthYear, sex, marital, spouseType, comboKey } = appDataCache;
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

  // Get reform benefit series if active
  let reformSeries = null;
  if (comboKey && reformIndData) {
    reformSeries = dataLoader.getReformBenefitSeries(reformIndData, comboKey, birthYear);
  }

  chartManager.renderBenefitsChart(series, currentBenefitView, deathAge, reformSeries);
}

function renderIndividualNMTRChart() {
  const { nmtrData, reformNmtrData, birthYear, sex, marital, spouseType, comboKey } = appDataCache;
  if (!nmtrData) return;

  const lookupSex = sex === 'unisex' ? 'male' : sex;
  const nmtrSeries = dataLoader.getNMTRSeries(nmtrData, lookupSex, marital, birthYear, spouseType);

  // Get reform NMTR series if active
  let reformNmtrSeries = null;
  if (comboKey && reformNmtrData) {
    reformNmtrSeries = dataLoader.getReformNMTRSeries(reformNmtrData, comboKey, birthYear);
  }

  const section = document.getElementById('nmtrSection');
  if (nmtrSeries) {
    if (section) section.style.display = '';
    chartManager.renderNMTRChart(nmtrSeries, reformNmtrSeries);
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
  const { cohortData, reformCohortData, birthYear, sex, marital, spouseType, comboKey } = appDataCache;
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

  // Show reform comparison in return section
  const reformMetrics = (comboKey && reformCohortData)
    ? dataLoader.getReformMetrics(reformCohortData, comboKey, birthYear)
    : null;

  // Update ratio display with reform comparison
  const ratioLabel = document.querySelector('.return-ratio-sublabel');
  if (ratioLabel && reformMetrics && reformMetrics.ratio != null && ratio != null) {
    const delta = reformMetrics.ratio - ratio;
    const sign = delta >= 0 ? '+' : '';
    const cls = delta < 0 ? 'text-negative' : 'text-positive';
    ratioLabel.innerHTML = `in lifetime benefits <span class="${cls}" style="font-weight:700">(${sign}${delta.toFixed(2)} with reform)</span>`;
  } else if (ratioLabel) {
    ratioLabel.textContent = 'in lifetime benefits';
  }
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
  const { cohortData, reformCohortData, sex, marital, spouseType, comboKey } = appDataCache;
  if (!cohortData) return;

  const series = sex === 'unisex'
    ? dataLoader.getUnisexCohortSeries(cohortData, marital, spouseType)
    : dataLoader.getCohortSeries(cohortData, sex, marital, spouseType);

  // Get reform cohort series if active
  let reformSeries = null;
  if (comboKey && reformCohortData) {
    reformSeries = dataLoader.getReformCohortSeries(reformCohortData, comboKey);
  }

  chartManager.renderCohortHeroChart(series, currentCohortMetric, reformSeries);
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
