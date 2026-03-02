// =============================================================================
// DataLoader — Fetch, cache, and serve pre-computed JSON data
// =============================================================================

class DataLoader {
  constructor() {
    this.cache = new Map();
    this.pending = new Map();  // Request deduplication
    this.manifest = null;
    this.maxCacheSize = 50;
    this.basePath = 'data';
  }

  /**
   * Initialize: fetch manifest.json
   */
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

  /**
   * Fetch a JSON file with caching and request deduplication
   */
  async _fetch(path) {
    if (this.cache.has(path)) {
      return this.cache.get(path);
    }
    if (this.pending.has(path)) {
      return this.pending.get(path);
    }
    const promise = fetch(`${this.basePath}/${path}`)
      .then(resp => {
        if (!resp.ok) throw new Error(`${path}: ${resp.status}`);
        return resp.json();
      })
      .then(data => {
        this.pending.delete(path);
        this._cacheSet(path, data);
        return data;
      })
      .catch(err => {
        this.pending.delete(path);
        console.warn(`DataLoader: failed to fetch ${path}:`, err.message);
        return null;
      });
    this.pending.set(path, promise);
    return promise;
  }

  /**
   * Set cache entry with LRU eviction
   */
  _cacheSet(key, value) {
    if (this.cache.size >= this.maxCacheSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }
    this.cache.set(key, value);
  }

  // =========================================================================
  // Public API
  // =========================================================================

  /**
   * Get cohort data (also serves individual metrics)
   */
  async getCohortData(type) {
    return this._fetch(`cohort/${type}_65.json`);
  }

  /**
   * Get individual benefit series (yearly benefits per birth year)
   */
  async getIndividualBenefits(type) {
    return this._fetch(`individual/${type}_65_benefits.json`);
  }

  /**
   * Get individual marginal analysis data (stub — returns null until NMTR data exists)
   */
  async getIndividualMarginal(type) {
    return null;
  }

  // =========================================================================
  // Data extraction helpers
  // =========================================================================

  /**
   * Extract a single birth year's metrics from cohort data
   */
  getMetricsForBirthYear(cohortData, comboKey, birthYear) {
    if (!cohortData?.data?.[comboKey]) return null;
    const d = cohortData.data[comboKey];
    const idx = d.birth_years.indexOf(birthYear);
    if (idx === -1) return null;
    return {
      monthly_benefit: d.monthly_benefit[idx],
      pv_benefits: d.pv_benefits[idx],
      pv_taxes: d.pv_taxes[idx],
      ratio: d.ratio[idx],
      irr: d.irr[idx],
      repl_rate: d.repl_rate_real_all?.[idx],
      death_age: d.death_age?.[idx]
    };
  }

  /**
   * Extract benefit series for a specific birth year
   */
  getBenefitSeries(benefitsData, comboKey, birthYear) {
    if (!benefitsData?.data?.[comboKey]) return null;
    return benefitsData.data[comboKey][String(birthYear)] || null;
  }

  /**
   * Extract marginal data for a specific birth year (stub)
   */
  getMarginalData(marginalData, comboKey, birthYear) {
    return null;
  }

  /**
   * Extract cohort arrays for a combo key
   */
  getCohortSeries(cohortData, comboKey) {
    if (!cohortData?.data?.[comboKey]) return null;
    return cohortData.data[comboKey];
  }

  /**
   * Get available combo keys from loaded data
   */
  getScenarios(data) {
    if (!data?.data) return [];
    return Object.keys(data.data);
  }

  /**
   * Get human-readable label for a combo key
   */
  getReformLabel(comboKey) {
    if (!comboKey || comboKey === 'baseline') return 'Current Law';

    const labels = this.manifest?.reform_labels || {};
    const parts = comboKey.split('+');

    // Map each reform to its label, fall back to the key itself
    const partLabels = parts.map(p => labels[p] || p);
    return partLabels.join(' + ');
  }
}

// Global instance
const dataLoader = new DataLoader();
