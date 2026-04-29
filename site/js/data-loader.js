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

  return { init, ready, meta, dimensions, getConfig, getCohortSeries };
})();
