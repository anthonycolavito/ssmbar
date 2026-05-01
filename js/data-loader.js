// =============================================================================
// DataLoader — Fetches site_data.json and exposes config lookups.
//
// JSON shape (per config):
//   annual:   { ages, years, earnings, scheduled:{...}, payable:{...} }
//   nmtr:     { ages, years, values, earnings_*, household_earnings_* }   // shared
//   summary:  { death_age, pv_taxes, scheduled:{...}, payable:{...} }
// =============================================================================

const dataLoader = (() => {
  let payload = null;

  async function init() {
    const resp = await fetch('data/site_data.json', { cache: 'no-store' });
    if (!resp.ok) throw new Error(`Failed to load site_data.json: ${resp.status}`);
    payload = await resp.json();
  }

  function ready() { return payload != null; }

  function meta() { return payload.meta; }

  function dimensions() { return payload.dimensions; }

  function configKey(workerType, spouseType, birthYear) {
    return `${workerType}|${spouseType}|${birthYear}`;
  }

  function getConfig(workerType, spouseType, birthYear) {
    const key = configKey(workerType, spouseType, birthYear);
    const cfg = payload.configs[key];
    if (!cfg) throw new Error(`No data for config: ${key}`);
    return cfg;
  }

  function hasNmtr(cfg) {
    return cfg && cfg.nmtr && Array.isArray(cfg.nmtr.ages) && cfg.nmtr.ages.length > 0;
  }

  function nmtrMissingBirthYears() {
    return (payload.meta && payload.meta.nmtr_missing_birth_years) || [];
  }

  function nmtrValuesPending() {
    return Boolean(payload.meta && payload.meta.nmtr_values_pending);
  }

  function pbNmtrPending() {
    return Boolean(payload.meta && payload.meta.pb_nmtr_pending);
  }

  // Within-cohort series keyed by worker type. For a fixed (spouse, birth
  // year), returns a parallel value per worker type (very_low → max). PV
  // taxes is scenario-invariant; the caller can render a single bar.
  function getWorkerCompareSeries(spouseType, birthYear, summaryField) {
    const types = payload.dimensions.worker_types.map(t => t.key);
    const scheduled = [];
    const payable   = [];
    for (const w of types) {
      const cfg = payload.configs[configKey(w, spouseType, birthYear)];
      if (!cfg) { scheduled.push(null); payable.push(null); continue; }
      if (summaryField === 'pv_taxes') {
        const v = cfg.summary.pv_taxes;
        scheduled.push(v ?? null);
        payable.push(v ?? null);
      } else {
        scheduled.push(cfg.summary.scheduled?.[summaryField] ?? null);
        payable.push(cfg.summary.payable?.[summaryField]     ?? null);
      }
    }
    return { types, scheduled, payable };
  }

  // Lifetime-profile overlay for the worker-comparison tab: one primary-only
  // earnings-then-benefit series per worker type, all five aligned on the
  // same age axis. Returns { types, ages, perType: {key→data}, transitionIdx,
  // leAge }. leAge is the maximum across types so every series finishes
  // within the chart range.
  function getWorkerCompareLifetimeProfiles(spouseType, birthYear, scenario = 'scheduled', real = true) {
    const types = payload.dimensions.worker_types.map(t => t.key);
    const profiles = types.map(w => ({
      key: w,
      profile: getLifetimeProfile(w, spouseType, birthYear, real, 'primary')
    }));
    const leAge = Math.max(...profiles.map(p => p.profile.leAge ?? 65));
    const startAge = Math.min(...profiles.map(p => p.profile.ages[0] ?? 65));
    const ages = [];
    for (let a = startAge; a <= leAge; a++) ages.push(a);

    const perType = {};
    let transitionIdx = ages.indexOf(65);
    for (const { key, profile } of profiles) {
      const series = profile[scenario];
      const ageMap = new Map(profile.ages.map((a, i) => [a, series[i]]));
      perType[key] = ages.map(a => ageMap.has(a) ? ageMap.get(a) : null);
    }
    return { types, ages, perType, transitionIdx, leAge };
  }

  // Multi-series age-axis arrays from the nmtr block (net_tax or marginal IRR).
  // `field` ∈ { 'values', 'marginal_irr' }, `scenario` ∈ { 'scheduled', 'payable' }.
  function getWorkerCompareNmtrSeries(spouseType, birthYear, field, scenario) {
    const types = payload.dimensions.worker_types.map(t => t.key);
    let ages = null;
    const perType = {};
    for (const w of types) {
      const cfg = payload.configs[configKey(w, spouseType, birthYear)];
      if (!cfg || !hasNmtr(cfg)) { perType[w] = null; continue; }
      if (!ages) ages = cfg.nmtr.ages;
      const arr = cfg.nmtr[scenario]?.[field];
      perType[w] = Array.isArray(arr) && arr.length > 0 ? arr : null;
    }
    return { types, ages: ages || [], perType };
  }

  // Constant-earner cohort series. The bundler stores parallel arrays
  // (indexed by birth_years) under payload.constant_earner. PV taxes is
  // scenario-invariant; the two arrays are identical for that field.
  function getConstantEarnerSeries(summaryField) {
    const ce    = payload.constant_earner;
    const years = ce.birth_years;
    if (summaryField === 'pv_taxes') {
      return { years, scheduled: ce.pv_taxes, payable: ce.pv_taxes };
    }
    return {
      years,
      scheduled: ce.scheduled[summaryField] || [],
      payable:   ce.payable[summaryField]   || []
    };
  }

  function getConstantEarnerMeta() {
    return payload.constant_earner;
  }

  function getConstantEarnerLifetime() {
    return payload.constant_earner.lifetime;
  }

  function getConstantEarnerIncomeShare() {
    return payload.constant_earner.income_share;
  }

  // Per-cohort cohort-tab series. Each metric gets parallel scheduled/payable
  // arrays of equal length. PV taxes is scenario-invariant; for that field the
  // two arrays are identical and the caller can render a single line.
  function getCohortSeries(workerType, spouseType, summaryField) {
    const years = payload.dimensions.birth_years;
    const scheduled = [];
    const payable   = [];
    for (const y of years) {
      const cfg = payload.configs[configKey(workerType, spouseType, y)];
      if (!cfg) { scheduled.push(null); payable.push(null); continue; }
      if (summaryField === 'pv_taxes') {
        const v = cfg.summary.pv_taxes;
        scheduled.push(v ?? null);
        payable.push(v ?? null);
      } else {
        scheduled.push(cfg.summary.scheduled?.[summaryField] ?? null);
        payable.push(cfg.summary.payable?.[summaryField]     ?? null);
      }
    }
    return { years, scheduled, payable };
  }

  // Lifetime profile pieces. Returns work-year ages + earnings (scenario-
  // invariant) plus parallel retirement-year arrays for scheduled and payable.
  // The chart can plot both retirement series and overlay them on the same
  // earnings prefix. When NMTR is missing, the work segment is empty and
  // earnings_available is false.
  function getLifetimeProfile(workerType, spouseType, birthYear, real = true, view = 'primary') {
    const cfg = getConfig(workerType, spouseType, birthYear);
    const leAge = cfg.summary.death_age;
    const household = view === 'household';

    const retAgesAll = cfg.annual.ages;
    const pickRet = scenarioObj => household
      ? (real ? scenarioObj.household_real    : scenarioObj.household_nominal)
      : (real ? scenarioObj.real              : scenarioObj.nominal);

    const retSchedAll = pickRet(cfg.annual.scheduled);
    const retPayAll   = pickRet(cfg.annual.payable);

    const keepIdx = retAgesAll.reduce((acc, a, i) => (a <= leAge ? acc.concat(i) : acc), []);
    const retAges       = keepIdx.map(i => retAgesAll[i]);
    const retScheduled  = keepIdx.map(i => retSchedAll[i]);
    const retPayable    = keepIdx.map(i => retPayAll[i]);

    if (!hasNmtr(cfg)) {
      return {
        ages:           retAges,
        scheduled:      retScheduled,
        payable:        retPayable,
        transitionIdx:  0,
        leAge,
        earnings_available: false
      };
    }

    const workAges = cfg.nmtr.ages;
    const workVals = household
      ? (real ? cfg.nmtr.household_earnings_real    : cfg.nmtr.household_earnings_nominal)
      : (real ? cfg.nmtr.earnings_real              : cfg.nmtr.earnings_nominal);

    const ages       = [...workAges, ...retAges];
    const scheduled  = [...workVals, ...retScheduled];
    const payable    = [...workVals, ...retPayable];
    const transitionIdx = workAges.length;

    if (ages[transitionIdx] !== 65) {
      throw new Error(`Lifetime profile transition mis-aligned: ages[${transitionIdx}]=${ages[transitionIdx]}, expected 65`);
    }
    return { ages, scheduled, payable, transitionIdx, leAge, earnings_available: true };
  }

  return {
    init, ready, meta, dimensions, getConfig, getCohortSeries, getLifetimeProfile,
    getWorkerCompareSeries, getWorkerCompareLifetimeProfiles, getWorkerCompareNmtrSeries,
    getConstantEarnerSeries, getConstantEarnerMeta,
    getConstantEarnerLifetime, getConstantEarnerIncomeShare,
    hasNmtr, nmtrMissingBirthYears, nmtrValuesPending, pbNmtrPending
  };
})();
