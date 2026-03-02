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
  // Build dimension key
  //   Single:  "{sex}_single"
  //   Married: "{sex}_married_{spouseType}"
  // =========================================================================

  dimKey(sex, marital, spouseType) {
    if (marital === 'married' && spouseType) {
      return `${sex}_married_${spouseType}`;
    }
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

  getMetricsForBirthYear(cohortData, sex, marital, birthYear, spouseType) {
    const key = this.dimKey(sex, marital, spouseType);
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

  getBenefitSeries(benefitsData, sex, marital, birthYear, spouseType) {
    const key = this.dimKey(sex, marital, spouseType);
    return benefitsData?.data?.[key]?.[String(birthYear)] || null;
  }

  getNMTRSeries(nmtrData, sex, marital, birthYear, spouseType) {
    const key = this.dimKey(sex, marital, spouseType);
    return nmtrData?.data?.[key]?.[String(birthYear)] || null;
  }

  getCohortSeries(cohortData, sex, marital, spouseType) {
    const key = this.dimKey(sex, marital, spouseType);
    return cohortData?.data?.[key] || null;
  }

  // =========================================================================
  // Unisex computation — average male and female values
  // =========================================================================

  getUnisexMetrics(cohortData, marital, birthYear, spouseType) {
    const maleData = this.getMetricsForBirthYear(cohortData, 'male', marital, birthYear, spouseType);
    const femaleData = this.getMetricsForBirthYear(cohortData, 'female', marital, birthYear, spouseType);
    if (!maleData || !femaleData) return maleData || femaleData;

    return {
      monthly_benefit: maleData.monthly_benefit,
      initial_real_benefit: maleData.initial_real_benefit,
      repl_rate: maleData.repl_rate,
      pv_benefits: (maleData.pv_benefits + femaleData.pv_benefits) / 2,
      pv_taxes: maleData.pv_taxes,
      ratio: (maleData.ratio + femaleData.ratio) / 2,
      irr: (maleData.irr + femaleData.irr) / 2,
      death_age: (maleData.death_age + femaleData.death_age) / 2,
      couple_pv_benefits: maleData.couple_pv_benefits,
      couple_pv_taxes: maleData.couple_pv_taxes,
      couple_ratio: maleData.couple_ratio
    };
  }

  getUnisexCohortSeries(cohortData, marital, spouseType) {
    const male = this.getCohortSeries(cohortData, 'male', marital, spouseType);
    const female = this.getCohortSeries(cohortData, 'female', marital, spouseType);
    if (!male || !female) return male || female;

    return {
      birth_years: male.birth_years,
      monthly_benefit: male.monthly_benefit,
      initial_real_benefit: male.initial_real_benefit,
      repl_rate: male.repl_rate,
      pv_benefits: male.pv_benefits.map((v, i) => (v + female.pv_benefits[i]) / 2),
      pv_taxes: male.pv_taxes,
      ratio: male.ratio.map((v, i) => (v + female.ratio[i]) / 2),
      irr: male.irr.map((v, i) => (v + female.irr[i]) / 2),
      death_age: male.death_age.map((v, i) => (v + female.death_age[i]) / 2)
    };
  }

  getReformLabel(comboKey) {
    if (!comboKey || comboKey === 'baseline') return 'Current Law';
    const labels = this.manifest?.reform_labels || {};
    const parts = comboKey.split('+');
    return parts.map(p => labels[p] || p).join(' + ');
  }
}

const dataLoader = new DataLoader();
