// =============================================================================
// App — Entry point. Wires data, controls, hero, charts, summary cards, table.
//
// Every metric is shown under both the scheduled and payable scenarios. The
// JSON config has parallel scheduled / payable sub-objects in `annual` and
// `summary`; PV taxes is invariant and lives at the top of `summary`.
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

  const cohortCsvBtn = document.getElementById('downloadCohortCsvBtn');
  if (cohortCsvBtn) {
    cohortCsvBtn.addEventListener('click', () => tableManager.downloadCohortCsv(uiControls.getState()));
  }
});

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
  document.getElementById('heroValue').textContent        = Fmt.currency(cfg.summary.scheduled.monthly_real_at_65);
  document.getElementById('heroValuePayable').textContent = Fmt.currency(cfg.summary.payable.monthly_real_at_65);

  const yrs = (cfg.summary.death_age != null) ? cfg.summary.death_age - 65 : null;
  const rrSched = Fmt.percent(cfg.summary.scheduled.rep_rate_career);
  const rrPay   = Fmt.percent(cfg.summary.payable.rep_rate_career);
  const subParts = ['in 2026 dollars'];
  subParts.push(`Replaces about <strong>${rrSched}</strong> of average career earnings (<strong>${rrPay}</strong> payable)`);
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
      label: 'Initial Monthly Benefit',
      scheduled: Fmt.currency(s.scheduled.monthly_real_at_65),
      payable:   Fmt.currency(s.payable.monthly_real_at_65),
      info:  'Initial benefit at age 65, 2026 dollars.'
    },
    {
      label: 'PV Lifetime Benefits',
      scheduled: Fmt.currency(s.scheduled.pv_benefits),
      payable:   Fmt.currency(s.payable.pv_benefits),
      info:  'Lifetime value of benefits discounted to age 65, 2026 dollars. Per member for couples — household total split equally between spouses.'
    },
    {
      label: 'PV Lifetime Taxes',
      value: Fmt.currency(s.pv_taxes),
      info:  'Lifetime value of taxes discounted to age 65, 2026 dollars. Per member for couples — household total split equally between spouses. Identical under scheduled and payable scenarios.'
    },
    {
      label: 'Benefit / Tax Ratio',
      scheduled: (s.scheduled.ben_tax_ratio == null) ? '--' : s.scheduled.ben_tax_ratio.toFixed(2),
      payable:   (s.payable.ben_tax_ratio   == null) ? '--' : s.payable.ben_tax_ratio.toFixed(2),
      info:  'PV Lifetime Benefits divided by PV Lifetime Taxes.'
    },
    {
      label: 'Replacement Rate (Career)',
      scheduled: Fmt.percent(s.scheduled.rep_rate_career),
      payable:   Fmt.percent(s.payable.rep_rate_career),
      info:  "Initial real benefit divided by worker's average real annual career earnings."
    },
    {
      label: 'Replacement Rate (Average Wage)',
      scheduled: Fmt.percent(s.scheduled.rep_rate_awi),
      payable:   Fmt.percent(s.payable.rep_rate_awi),
      info:  'Initial benefit divided by the national average wage.'
    }
  ];

  const host = document.getElementById('summaryCards');
  host.innerHTML = cards.map(c => {
    const infoIcon = c.info
      ? `<span class="summary-info" data-tip="${escapeAttr(c.info)}" tabindex="0" aria-label="${escapeAttr(c.info)}"><i class="bi bi-question-circle"></i></span>`
      : '';
    const valueBlock = (c.value != null)
      ? `<div class="summary-value">${c.value}</div>`
      : `
        <div class="summary-value-dual">
          <div class="summary-value-row">
            <span class="summary-value">${c.scheduled}</span>
            <span class="summary-scenario-label">scheduled</span>
          </div>
          <div class="summary-value-row summary-value-row--secondary">
            <span class="summary-value summary-value--secondary">${c.payable}</span>
            <span class="summary-scenario-label">payable</span>
          </div>
        </div>`;
    return `
      <div class="summary-card">
        <div class="summary-label">${c.label}${infoIcon}</div>
        ${valueBlock}
      </div>`;
  }).join('');
}

function escapeAttr(s) {
  return String(s).replace(/&/g, '&amp;').replace(/"/g, '&quot;').replace(/</g, '&lt;');
}

// -----------------------------------------------------------------------------
// Charts — Individual tab
// -----------------------------------------------------------------------------

function renderLifetimeProfile(cfg, state) {
  const view = state.lifetimeView || 'primary';
  const profile = dataLoader.getLifetimeProfile(
    state.workerType, state.spouseType, state.birthYear, state.real, view
  );

  const subtitleEl = document.getElementById('lifetimeProfileSubtitle');
  if (subtitleEl) {
    const dollars = state.real ? 'Real 2026 dollars (GDP price index)' : 'Nominal dollars (year of receipt)';
    let scope;
    if (view === 'household') {
      scope = state.spouseType === 'none'
        ? 'Single-worker household — earnings (ages 21–64) and Social Security benefits (age 65 to life expectancy).'
        : 'Household totals — combined earnings of both spouses (ages 21–64) and combined Social Security benefits (age 65 to life expectancy).';
    } else {
      scope = "Primary worker only — own earnings (ages 21–64) and own Social Security benefit including any supplemental spousal benefit (age 65 to life expectancy).";
    }
    const fallback = profile.earnings_available
      ? ''
      : ' Working-year earnings data not yet available for this cohort — only retirement years shown.';
    subtitleEl.textContent = `${dollars}. ${scope}${fallback} Solid line = scheduled benefits; dashed line = payable.`;
  }

  chartManager.lifetimeProfileChart('lifetimeProfileChart', {
    ages:            profile.ages,
    values:          profile.scheduled,
    valuesSecondary: profile.payable,
    transitionIdx:   profile.transitionIdx,
    leAge:           profile.leAge
  });
}

function renderAnnualBenefitsChart(cfg, real) {
  const sched = real ? cfg.annual.scheduled.real : cfg.annual.scheduled.nominal;
  const pay   = real ? cfg.annual.payable.real   : cfg.annual.payable.nominal;
  const leAge = cfg.summary.death_age;
  const fadeIdx = (leAge != null) ? cfg.annual.ages.indexOf(leAge) : null;
  const subtitle = real
    ? 'Real 2026 dollars (GDP price index). Solid = scheduled, dashed = payable.'
    : 'Nominal dollars (year of receipt). Solid = scheduled, dashed = payable.';
  document.getElementById('annualBenefitsSubtitle').textContent = subtitle;

  chartManager.lineChart('annualBenefitsChart', {
    labels:        cfg.annual.ages,
    data:          sched,
    dataSecondary: pay,
    yFormat:       'currency',
    yMin:          0,
    leMarker:      leAge,
    fadeAfterIdx:  (fadeIdx != null && fadeIdx >= 0) ? fadeIdx : null
  });
}

function renderNetTaxRateChart(cfg) {
  const canvas    = document.getElementById('netTaxRateChart');
  const empty     = document.getElementById('netTaxRateEmpty');
  const subtitle  = document.getElementById('netTaxRateSubtitle');
  const pending   = dataLoader.nmtrValuesPending();
  const pbPending = dataLoader.pbNmtrPending();
  const hasData   = dataLoader.hasNmtr(cfg) && !pending;

  if (canvas) canvas.hidden = !hasData;
  if (empty) {
    empty.hidden = hasData;
    const msg = empty.querySelector('span');
    if (msg) {
      msg.textContent = pending
        ? 'Net tax rate data is being recomputed — currently unavailable.'
        : 'Net tax data is not yet available for this cohort.';
    }
  }

  if (subtitle) {
    subtitle.textContent = pbPending
      ? 'Payroll tax rate net of accrued value of benefits by age. Scheduled scenario only — payable scenario coming soon.'
      : 'Payroll tax rate net of accrued value of benefits by age. Solid = scheduled, dashed = payable.';
  }

  if (!hasData) {
    chartManager.destroyChart('netTaxRateChart');
    return;
  }

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
  chartManager.cohortLineChart('cohortMonthlyChart', {
    labels: monthly.years, data: monthly.scheduled, dataSecondary: monthly.payable,
    yFormat: 'currency'
  });

  // Benefit/Tax Ratio: solid scheduled + dashed payable, dashed reference at
  // 1.0; threshold colouring suppressed when both lines are shown so the
  // dual-line story stays readable.
  const ratio = dataLoader.getCohortSeries(w, s, 'ben_tax_ratio');
  chartManager.cohortLineChart('cohortRatioChart', {
    labels: ratio.years, data: ratio.scheduled, dataSecondary: ratio.payable,
    yFormat: 'number',
    referenceY: 1.0,
    referenceLabel: 'Break-even (1.0)'
  });

  const pvBen = dataLoader.getCohortSeries(w, s, 'pv_benefits');
  chartManager.cohortLineChart('cohortPvBenChart', {
    labels: pvBen.years, data: pvBen.scheduled, dataSecondary: pvBen.payable,
    yFormat: 'currency'
  });

  // PV Taxes is identical under scheduled and payable — single line.
  const pvTax = dataLoader.getCohortSeries(w, s, 'pv_taxes');
  chartManager.cohortLineChart('cohortPvTaxChart', {
    labels: pvTax.years, data: pvTax.scheduled,
    yFormat: 'currency'
  });

  // The two replacement-rate charts share a common y-axis (0 to the larger of
  // the two metrics' max across cohorts and scenarios) so they're directly
  // comparable.
  const rrCareer = dataLoader.getCohortSeries(w, s, 'rep_rate_career');
  const rrAwi    = dataLoader.getCohortSeries(w, s, 'rep_rate_awi');
  const rrAll = [
    ...rrCareer.scheduled, ...rrCareer.payable,
    ...rrAwi.scheduled,    ...rrAwi.payable
  ].filter(v => v != null);
  const rrMax = rrAll.length ? Math.max(...rrAll) * 1.05 : 1;

  chartManager.cohortLineChart('cohortRrCareerChart', {
    labels: rrCareer.years, data: rrCareer.scheduled, dataSecondary: rrCareer.payable,
    yFormat: 'percent', yMin: 0, yMax: rrMax
  });
  chartManager.cohortLineChart('cohortRrAwiChart', {
    labels: rrAwi.years, data: rrAwi.scheduled, dataSecondary: rrAwi.payable,
    yFormat: 'percent', yMin: 0, yMax: rrMax
  });
}

// -----------------------------------------------------------------------------
// Errors
// -----------------------------------------------------------------------------

function showLoadError(err) {
  const host = document.querySelector('.app-container');
  host.innerHTML = `<div class="alert alert-danger m-4">Failed to load dashboard data: ${err.message}</div>`;
}
