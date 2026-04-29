// =============================================================================
// DataLoader — Fetches site_data.json and exposes config lookups.
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

  function getCohortSeries(workerType, spouseType, summaryField) {
    const years = payload.dimensions.birth_years;
    const values = years.map(y => {
      const cfg = payload.configs[configKey(workerType, spouseType, y)];
      if (!cfg) return null;
      const v = cfg.summary[summaryField];
      return (v == null) ? null : v;
    });
    return { years, values };
  }

  // Lifetime profile: working-year earnings (21–64) followed by retirement
  // benefits (65 → life expectancy). Honours the real/nominal toggle and the
  // primary/household view toggle. When NMTR data is missing for a cohort,
  // returns retirement-only with earnings_available=false so the caller can
  // adjust labelling.
  function getLifetimeProfile(workerType, spouseType, birthYear, real = true, view = 'primary') {
    const cfg = getConfig(workerType, spouseType, birthYear);
    const leAge = cfg.summary.death_age;
    const household = view === 'household';

    const retAgesAll = cfg.annual.ages;
    const retValsAll = household
      ? (real ? cfg.annual.household_real    : cfg.annual.household_nominal)
      : (real ? cfg.annual.real              : cfg.annual.nominal);
    const retAges = retAgesAll.filter(a => a <= leAge);
    const retVals = retValsAll.slice(0, retAges.length);

    if (!hasNmtr(cfg)) {
      return {
        ages: retAges,
        values: retVals,
        transitionIdx: 0,
        leAge,
        earnings_available: false
      };
    }

    const workAges = cfg.nmtr.ages;
    const workVals = household
      ? (real ? cfg.nmtr.household_earnings_real    : cfg.nmtr.household_earnings_nominal)
      : (real ? cfg.nmtr.earnings_real              : cfg.nmtr.earnings_nominal);

    const ages   = [...workAges, ...retAges];
    const values = [...workVals, ...retVals];
    const transitionIdx = workAges.length;

    if (ages[transitionIdx] !== 65) {
      throw new Error(`Lifetime profile transition mis-aligned: ages[${transitionIdx}]=${ages[transitionIdx]}, expected 65`);
    }
    return { ages, values, transitionIdx, leAge, earnings_available: true };
  }

  return {
    init, ready, meta, dimensions, getConfig, getCohortSeries, getLifetimeProfile,
    hasNmtr, nmtrMissingBirthYears
  };
})();
