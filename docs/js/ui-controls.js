// =============================================================================
// UI Controls — Hero controls, tab switching, collapsible sections
// =============================================================================

// =========================================================================
// Hero state getters
// =========================================================================

function getHeroWorkerType() {
  return document.getElementById('heroWorkerType')?.value || 'medium';
}

function getHeroBirthYear() {
  return parseInt(document.getElementById('heroBirthYear')?.value) || 1960;
}

function getHeroSex() {
  return document.getElementById('heroSex')?.value || 'unisex';
}

function getHeroMarital() {
  return document.getElementById('heroMarital')?.value || 'single';
}

function getHeroSpouseType() {
  return document.getElementById('heroSpouseType')?.value || 'medium';
}

// =========================================================================
// Sex/Marital constraint logic
// =========================================================================

function updateHeroConstraints() {
  const sex = getHeroSex();
  const maritalEl = document.getElementById('heroMarital');
  const maritalGroup = document.getElementById('maritalGroup');
  const spouseGroup = document.getElementById('spouseTypeGroup');

  if (sex === 'unisex') {
    // Hide marital entirely when unisex — married requires a specific sex
    if (maritalEl) maritalEl.value = 'single';
    if (maritalGroup) maritalGroup.style.display = 'none';
    if (spouseGroup) spouseGroup.style.display = 'none';
  } else {
    // Show marital option for male/female
    if (maritalGroup) maritalGroup.style.display = '';

    // Show spouse type row when married
    const marital = getHeroMarital();
    if (spouseGroup) {
      spouseGroup.style.display = marital === 'married' ? 'block' : 'none';
    }
  }
}

// =========================================================================
// Tab switching
// =========================================================================

function switchTab(tabName) {
  document.querySelectorAll('#mainTabs .hero-tab').forEach(btn => {
    btn.classList.toggle('active', btn.dataset.tab === tabName);
  });
  document.getElementById('individualPane').classList.toggle('active', tabName === 'individual');
  document.getElementById('cohortPane').classList.toggle('active', tabName === 'cohort');
  window.location.hash = tabName;
  onHeroChange();
}

// =========================================================================
// Collapsible sections
// =========================================================================

function toggleSection(sectionId) {
  const body = document.getElementById(sectionId);
  const chevron = document.getElementById(sectionId + 'Chevron');
  body.classList.toggle('collapsed');
  if (chevron) {
    chevron.classList.toggle('bi-chevron-down');
    chevron.classList.toggle('bi-chevron-up');
  }
}

// =========================================================================
// Benefit view toggle (nominal/real) — default to real
// =========================================================================

let currentBenefitView = 'real';

function toggleBenefitView(mode, btnEl) {
  currentBenefitView = mode;
  btnEl.closest('.toggle-group').querySelectorAll('button').forEach(b => {
    b.classList.toggle('active', b.dataset.toggle === mode);
  });
  renderIndividualBenefitsChart();
}

// =========================================================================
// Cohort metric switcher
// =========================================================================

let currentCohortMetric = 'ratio';

const COHORT_NARRATIVES = {
  ratio: {
    title: "Each generation's return on Social Security taxes",
    subtitle: 'A ratio above 1.0 means you receive more in benefits than you paid in taxes'
  },
  initial_real_benefit: {
    title: 'How monthly benefits have grown across generations',
    subtitle: 'Initial monthly benefit at age 65, in 2025 dollars'
  },
  repl_rate: {
    title: 'What share of earnings Social Security replaces',
    subtitle: 'Higher replacement rates mean Social Security covers more of your pre-retirement income'
  },
  pv_benefits: {
    title: 'Total lifetime Social Security benefits by generation',
    subtitle: 'Present value of all benefits from age 65 to expected death, discounted to age 65'
  },
  irr: {
    title: 'The effective return on Social Security taxes',
    subtitle: 'Internal rate of return treating SS taxes as contributions and benefits as payouts'
  }
};

function switchCohortMetric(metric, btnEl) {
  currentCohortMetric = metric;

  // Update active pill
  document.querySelectorAll('#cohortMetricSelector .metric-btn').forEach(b => {
    b.classList.toggle('active', b.dataset.metric === metric);
  });

  // Update narrative text
  const narrative = COHORT_NARRATIVES[metric] || COHORT_NARRATIVES.ratio;
  const titleEl = document.getElementById('cohortTitle');
  const subEl = document.getElementById('cohortSubtitle');
  if (titleEl) titleEl.textContent = narrative.title;
  if (subEl) subEl.textContent = narrative.subtitle;

  // Re-render chart
  renderCohortChart();
}

// =========================================================================
// Reform sidebar
// =========================================================================

// Active reform state: { pia: 'reduce_fact3', nra: null, cola: null, ... }
const activeReforms = {};

function toggleSidebar() {
  const sidebar = document.getElementById('reformSidebar');
  sidebar.classList.toggle('collapsed');
}

function toggleReformCategory(category) {
  const body = document.getElementById(category + '-body');
  const chevron = document.getElementById(category + '-chevron');
  if (!body) return;
  body.classList.toggle('open');
  if (chevron) chevron.classList.toggle('open');
}

function selectReform(category, reformName, btn) {
  // Click-to-deselect: if already active, clear it
  if (activeReforms[category] === reformName) {
    activeReforms[category] = null;
    btn.classList.remove('active');
  } else {
    // Deselect previous in this category
    const catBody = document.getElementById(category + '-body');
    if (catBody) {
      catBody.querySelectorAll('.reform-option').forEach(b => b.classList.remove('active'));
    }
    activeReforms[category] = reformName;
    btn.classList.add('active');
  }
  onHeroChange();
}

function getActiveComboKey() {
  const parts = [];
  // Category order: pia, nra, cola, taxmax, other
  for (const cat of ['pia', 'nra', 'cola', 'taxmax', 'other']) {
    if (activeReforms[cat]) parts.push(activeReforms[cat]);
  }
  return parts.length > 0 ? parts.join('+') : null;
}

function hasActiveReform() {
  return getActiveComboKey() !== null;
}

function getActiveReformLabel() {
  const comboKey = getActiveComboKey();
  if (!comboKey) return null;
  return dataLoader.getReformLabel(comboKey);
}

// =========================================================================
// CSV Export
// =========================================================================

function exportCSV(tableId) {
  const now = new Date().toISOString().slice(0, 10);
  tableManager.exportCSV(tableId, `ss_explorer_${tableId}_${now}.csv`);
}
