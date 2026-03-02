// =============================================================================
// DataLoader — Fetch, cache, and serve pre-computed JSON data
// =============================================================================

class DataLoader {
  constructor() {
    this.cache = new Map();
    this.pending = new Map();
    this.manifest = null;
    this.maxCacheSize = 50;
    this.basePath = 'data';
  }

  async init() {
    try {
      const resp = await fetch(`${this.basePath}/manifest.json`);
      if (!resp.ok) throw new Error(`manifest.json: ${resp.status}`);
      this.manifest = await resp.json();
      return this.manifest;
    } catch (err) {
      console.error('DataLoader init failed:', err);
      this.manifest = { dimensions: {}, reform_labels: {}, reform_categories: {} };
      return this.manifest;
    }
  }

  async _fetch(path) {
    if (this.cache.has(path)) return this.cache.get(path);
    if (this.pending.has(path)) return this.pending.get(path);

    const url = `${this.basePath}/${path}`;
    const promise = fetch(url)
      .then(resp => {
        if (!resp.ok) throw new Error(`HTTP ${resp.status} for ${url}`);
        return resp.json();
      })
      .then(data => {
        this.pending.delete(path);
        this._cacheSet(path, data);
        return data;
      })
      .catch(err => {
        this.pending.delete(path);
        console.error(`DataLoader: failed to fetch ${url}:`, err);
        return null;
      });
    this.pending.set(path, promise);
    return promise;
  }

  _cacheSet(key, value) {
    if (this.cache.size >= this.maxCacheSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }
    this.cache.set(key, value);
  }

  // =========================================================================
  // Build dimension key: "{sex}_{marital}"
  // =========================================================================

  dimKey(sex, marital) {
    return `${sex}_${marital}`;
  }

  // =========================================================================
  // Public API — file fetchers
  // =========================================================================

  async getCohortData(type) {
    return this._fetch(`cohort/${type}.json`);
  }

  async getIndividualBenefits(type) {
    return this._fetch(`individual/${type}_benefits.json`);
  }

  async getIndividualNMTR(type) {
    return this._fetch(`individual/${type}_nmtr.json`);
  }

  // =========================================================================
  // Data extraction helpers
  // =========================================================================

  /**
   * Extract metrics for a single birth year from cohort data
   */
  getMetricsForBirthYear(cohortData, sex, marital, birthYear) {
    const key = this.dimKey(sex, marital);
    const d = cohortData?.data?.[key];
    if (!d) return null;
    const idx = d.birth_years.indexOf(birthYear);
    if (idx === -1) return null;
    return {
      monthly_benefit: d.monthly_benefit[idx],
      pv_benefits: d.pv_benefits[idx],
      pv_taxes: d.pv_taxes[idx],
      ratio: d.ratio[idx],
      irr: d.irr[idx],
      repl_rate: d.repl_rate[idx],
      death_age: d.death_age?.[idx],
      initial_real_benefit: d.initial_real_benefit?.[idx],
      couple_pv_benefits: d.couple_pv_benefits?.[idx],
      couple_pv_taxes: d.couple_pv_taxes?.[idx],
      couple_ratio: d.couple_ratio?.[idx]
    };
  }

  /**
   * Extract benefit series for a specific birth year
   */
  getBenefitSeries(benefitsData, sex, marital, birthYear) {
    const key = this.dimKey(sex, marital);
    return benefitsData?.data?.[key]?.[String(birthYear)] || null;
  }

  /**
   * Extract NMTR series for a specific birth year
   */
  getNMTRSeries(nmtrData, sex, marital, birthYear) {
    const key = this.dimKey(sex, marital);
    return nmtrData?.data?.[key]?.[String(birthYear)] || null;
  }

  /**
   * Extract cohort-level arrays (all birth years) for charts
   */
  getCohortSeries(cohortData, sex, marital) {
    const key = this.dimKey(sex, marital);
    return cohortData?.data?.[key] || null;
  }

  /**
   * Get human-readable reform label
   */
  getReformLabel(comboKey) {
    if (!comboKey || comboKey === 'baseline') return 'Current Law';
    const labels = this.manifest?.reform_labels || {};
    const parts = comboKey.split('+');
    return parts.map(p => labels[p] || p).join(' + ');
  }
}

const dataLoader = new DataLoader();
