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
      // Return a minimal manifest so the app can still render UI
      this.manifest = { dimensions: {}, reform_scenarios: {}, reform_categories: {} };
      return this.manifest;
    }
  }

  /**
   * Build config key from worker type and custom earnings
   */
  configKey(workerType, customEarnings) {
    if (workerType === 'custom' && customEarnings) {
      return `custom_${customEarnings / 1000}k`;
    }
    return workerType;
  }

  /**
   * Build file key for cache lookup
   */
  _fileKey(category, type, sex, claimAge, suffix = '') {
    return `${category}/${type}_${sex}_${claimAge}${suffix}`;
  }

  /**
   * Fetch a JSON file with caching and request deduplication
   */
  async _fetch(path) {
    // Check cache
    if (this.cache.has(path)) {
      return this.cache.get(path);
    }

    // Check if already fetching (dedup)
    if (this.pending.has(path)) {
      return this.pending.get(path);
    }

    // Fetch
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
      // Evict oldest entry
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }
    this.cache.set(key, value);
  }

  // =========================================================================
  // Public API
  // =========================================================================

  /**
   * Get cohort data for a config key
   */
  async getCohortData(type, sex, claimAge) {
    const path = `cohort/${type}_${sex}_${claimAge}.json`;
    return this._fetch(path);
  }

  /**
   * Get individual metrics (scalar summaries per birth year)
   */
  async getIndividualMetrics(type, sex, claimAge) {
    const path = `individual/${type}_${sex}_${claimAge}_metrics.json`;
    return this._fetch(path);
  }

  /**
   * Get individual benefit series (yearly benefits per birth year)
   */
  async getIndividualBenefits(type, sex, claimAge) {
    const path = `individual/${type}_${sex}_${claimAge}_benefits.json`;
    return this._fetch(path);
  }

  /**
   * Get individual marginal analysis data
   */
  async getIndividualMarginal(type, sex, claimAge) {
    const path = `individual/${type}_${sex}_${claimAge}_marginal.json`;
    return this._fetch(path);
  }

  // =========================================================================
  // Data extraction helpers
  // =========================================================================

  /**
   * Extract a single birth year's data from an individual metrics file
   */
  getMetricsForBirthYear(metricsData, scenario, birthYear) {
    if (!metricsData?.data?.[scenario]) return null;
    const d = metricsData.data[scenario];
    const idx = d.birth_years.indexOf(birthYear);
    if (idx === -1) return null;
    return {
      monthly_benefit: d.monthly_benefit[idx],
      pv_benefits: d.pv_benefits[idx],
      pv_taxes: d.pv_taxes[idx],
      ratio: d.ratio[idx],
      irr: d.irr[idx]
    };
  }

  /**
   * Extract benefit series for a specific birth year
   */
  getBenefitSeries(benefitsData, scenario, birthYear) {
    if (!benefitsData?.data?.[scenario]) return null;
    return benefitsData.data[scenario][String(birthYear)] || null;
  }

  /**
   * Extract marginal data for a specific birth year
   */
  getMarginalData(marginalData, scenario, birthYear) {
    if (!marginalData?.data?.[scenario]) return null;
    return marginalData.data[scenario][String(birthYear)] || null;
  }

  /**
   * Extract cohort arrays for a scenario, optionally filtered by birth year range
   */
  getCohortSeries(cohortData, scenario, minYear = null, maxYear = null) {
    if (!cohortData?.data?.[scenario]) return null;
    const d = cohortData.data[scenario];

    if (minYear == null && maxYear == null) return d;

    // Filter by range
    const startIdx = minYear ? d.birth_years.indexOf(minYear) : 0;
    const endIdx = maxYear ? d.birth_years.indexOf(maxYear) + 1 : d.birth_years.length;

    if (startIdx === -1 || endIdx === 0) return d;

    const result = {};
    for (const key of Object.keys(d)) {
      if (Array.isArray(d[key])) {
        result[key] = d[key].slice(startIdx, endIdx);
      } else {
        result[key] = d[key];
      }
    }
    return result;
  }

  /**
   * Get available scenarios from loaded data
   */
  getScenarios(data) {
    if (!data?.data) return [];
    return Object.keys(data.data);
  }

  /**
   * Get reform label from manifest
   */
  getReformLabel(scenarioName) {
    if (!this.manifest?.reform_scenarios?.[scenarioName]) return scenarioName;
    return this.manifest.reform_scenarios[scenarioName].label;
  }
}

// Global instance
const dataLoader = new DataLoader();
