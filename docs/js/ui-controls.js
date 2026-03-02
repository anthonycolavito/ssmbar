// =============================================================================
// UI Controls — Dropdown population, sidebar, toggle, and event wiring
// =============================================================================

/**
 * Populate a <select> element with options
 */
function populateSelect(id, options, defaultValue) {
  const el = document.getElementById(id);
  if (!el) return;
  el.innerHTML = '';
  for (const opt of options) {
    const option = document.createElement('option');
    option.value = opt.value;
    option.textContent = opt.label;
    if (opt.value === defaultValue || opt.value === String(defaultValue)) {
      option.selected = true;
    }
    el.appendChild(option);
  }
}

/**
 * Populate worker type dropdowns
 */
function populateWorkerTypes(selectId, defaultValue = 'medium') {
  const types = [
    { value: 'very_low', label: 'Very Low' },
    { value: 'low', label: 'Low' },
    { value: 'medium', label: 'Medium' },
    { value: 'high', label: 'High' },
    { value: 'max', label: 'Maximum' },
    { value: 'custom_50k', label: '$50K Earner' }
  ];
  populateSelect(selectId, types, defaultValue);
}

/**
 * Populate claim age dropdown
 */
function populateClaimAges(selectId, defaultValue = 65) {
  const ages = [];
  for (let a = 62; a <= 70; a++) {
    ages.push({ value: String(a), label: String(a) });
  }
  populateSelect(selectId, ages, String(defaultValue));
}

/**
 * Populate birth year dropdown (every 10th year)
 */
function populateBirthYears(selectId, min = 1940, max = 2010, defaultValue = 1960) {
  const years = [];
  for (let y = min; y <= max; y += 10) {
    years.push({ value: String(y), label: String(y) });
  }
  populateSelect(selectId, years, String(defaultValue));
}

// =============================================================================
// Sidebar controls
// =============================================================================

/**
 * Toggle sidebar visibility (mobile)
 */
function toggleSidebar() {
  const sidebar = document.getElementById('sidebar');
  const overlay = document.getElementById('sidebarOverlay');
  sidebar.classList.toggle('open');
  overlay.classList.toggle('open');
}

/**
 * Toggle a reform section's collapsed state
 */
function toggleReformSection(headerEl) {
  const section = headerEl.closest('.reform-section');
  const options = section.querySelector('.reform-options');
  section.classList.toggle('collapsed');
  if (options.classList.contains('collapsed')) {
    options.classList.remove('collapsed');
  } else {
    options.classList.add('collapsed');
  }
}

// =============================================================================
// Reform combo key builder
// =============================================================================

// Category order must match the data generation script
const REFORM_RADIO_GROUPS = [
  'reform_pia',
  'reform_nra',
  'reform_cola',
  'reform_taxmax',
  'reform_other'
];

/**
 * Build combo key from the 5 radio button groups.
 * Returns "baseline" if none selected, otherwise joins active reform names with "+".
 */
function getComboKey() {
  const active = [];
  for (const groupName of REFORM_RADIO_GROUPS) {
    const checked = document.querySelector(`input[name="${groupName}"]:checked`);
    if (checked) {
      active.push(checked.value);
    }
  }
  return active.length === 0 ? 'baseline' : active.join('+');
}

/**
 * Check if any reform is selected (non-baseline)
 */
function hasReformSelected() {
  return getComboKey() !== 'baseline';
}

/**
 * Clear all reform selections (deselect all radios)
 */
function clearReform() {
  for (const groupName of REFORM_RADIO_GROUPS) {
    document.querySelectorAll(`input[name="${groupName}"]`).forEach(r => r.checked = false);
  }
  updateReformBadge();
  onInputChange();
}

/**
 * Enable click-to-deselect on radio buttons (no "None" option needed)
 */
function initRadioDeselect() {
  for (const groupName of REFORM_RADIO_GROUPS) {
    document.querySelectorAll(`input[name="${groupName}"]`).forEach(radio => {
      radio.addEventListener('click', function () {
        if (this._wasChecked) {
          this.checked = false;
          this._wasChecked = false;
        } else {
          // Unmark siblings
          document.querySelectorAll(`input[name="${groupName}"]`).forEach(r => r._wasChecked = false);
          this._wasChecked = true;
        }
        updateReformBadge();
        onInputChange();
      });
    });
  }
}

/**
 * Update the reform badge display
 */
function updateReformBadge() {
  const badge = document.getElementById('selectedReformBadge');
  const noBadge = document.getElementById('noReformBadge');
  const nameEl = document.getElementById('selectedReformName');

  const comboKey = getComboKey();

  if (comboKey !== 'baseline') {
    badge.style.display = 'block';
    noBadge.style.display = 'none';
    nameEl.textContent = dataLoader.getReformLabel(comboKey);
  } else {
    badge.style.display = 'none';
    noBadge.style.display = 'block';
  }
}

// =============================================================================
// Tab switching
// =============================================================================

/**
 * Switch between Individual and Cohort tabs
 */
function switchTab(tabName) {
  document.querySelectorAll('#mainTabs .nav-link').forEach(btn => {
    btn.classList.toggle('active', btn.dataset.tab === tabName);
  });
  document.getElementById('individualPane').classList.toggle('active', tabName === 'individual');
  document.getElementById('cohortPane').classList.toggle('active', tabName === 'cohort');
  window.location.hash = tabName;
  onInputChange();
}

// =============================================================================
// Collapsible sections
// =============================================================================

function toggleSection(sectionId) {
  const body = document.getElementById(sectionId);
  const chevron = document.getElementById(sectionId + 'Chevron');
  body.classList.toggle('collapsed');
  if (chevron) {
    chevron.classList.toggle('bi-chevron-down');
    chevron.classList.toggle('bi-chevron-up');
  }
}

// =============================================================================
// Benefit view toggle (nominal/real)
// =============================================================================

let currentBenefitView = 'nominal';

function toggleBenefitView(mode, btnEl) {
  currentBenefitView = mode;
  btnEl.closest('.toggle-group').querySelectorAll('button').forEach(b => {
    b.classList.toggle('active', b.dataset.toggle === mode);
  });
  renderIndividualBenefitsChart();
}

// =============================================================================
// CSV Export
// =============================================================================

function exportCSV(tableId) {
  const now = new Date().toISOString().slice(0, 10);
  tableManager.exportCSV(tableId, `ss_explorer_${tableId}_${now}.csv`);
}
