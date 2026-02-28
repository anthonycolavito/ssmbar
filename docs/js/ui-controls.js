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
 * Populate worker type dropdowns (shared between individual and cohort)
 */
function populateWorkerTypes(selectId, defaultValue = 'medium') {
  const types = [
    { value: 'very_low', label: 'Very Low' },
    { value: 'low', label: 'Low' },
    { value: 'medium', label: 'Medium' },
    { value: 'high', label: 'High' },
    { value: 'max', label: 'Maximum' },
    { value: 'custom_25k', label: '$25K Earner' },
    { value: 'custom_50k', label: '$50K Earner' },
    { value: 'custom_75k', label: '$75K Earner' },
    { value: 'custom_100k', label: '$100K Earner' },
    { value: 'custom_125k', label: '$125K Earner' },
    { value: 'custom_150k', label: '$150K Earner' }
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
 * Populate birth year dropdown
 */
function populateBirthYears(selectId, min = 1939, max = 2010, defaultValue = 1960) {
  const years = [];
  for (let y = min; y <= max; y++) {
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

/**
 * Get currently selected reform (or null for baseline)
 */
function getSelectedReform() {
  const checked = document.querySelector('input[name="reform"]:checked');
  return checked ? checked.value : null;
}

/**
 * Clear reform selection
 */
function clearReform() {
  const checked = document.querySelector('input[name="reform"]:checked');
  if (checked) checked.checked = false;
  updateReformBadge(null);
  onInputChange();
}

/**
 * Update the reform badge display
 */
function updateReformBadge(reformName) {
  const badge = document.getElementById('selectedReformBadge');
  const noBadge = document.getElementById('noReformBadge');
  const nameEl = document.getElementById('selectedReformName');

  if (reformName) {
    badge.style.display = 'block';
    noBadge.style.display = 'none';
    // Get human-readable label
    const label = dataLoader.getReformLabel(reformName);
    nameEl.textContent = label;
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
  // Update tab buttons
  document.querySelectorAll('#mainTabs .nav-link').forEach(btn => {
    btn.classList.toggle('active', btn.dataset.tab === tabName);
  });

  // Update tab panes
  document.getElementById('individualPane').classList.toggle('active', tabName === 'individual');
  document.getElementById('cohortPane').classList.toggle('active', tabName === 'cohort');

  // Update URL hash
  window.location.hash = tabName;

  // Trigger data load for the active tab
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
  // Update button state
  btnEl.closest('.toggle-group').querySelectorAll('button').forEach(b => {
    b.classList.toggle('active', b.dataset.toggle === mode);
  });
  // Re-render benefits chart
  renderIndividualBenefitsChart();
}

// =============================================================================
// Range slider (cohort birth year range)
// =============================================================================

function updateRangeSlider() {
  const minSlider = document.getElementById('cohBirthYearMin');
  const maxSlider = document.getElementById('cohBirthYearMax');
  let minVal = parseInt(minSlider.value);
  let maxVal = parseInt(maxSlider.value);

  // Ensure min <= max
  if (minVal > maxVal) {
    minSlider.value = maxVal;
    minVal = maxVal;
  }

  document.getElementById('cohBirthYearMinLabel').textContent = minVal;
  document.getElementById('cohBirthYearMaxLabel').textContent = maxVal;

  // Debounce chart update
  clearTimeout(updateRangeSlider._timer);
  updateRangeSlider._timer = setTimeout(() => onInputChange(), 200);
}

// =============================================================================
// CSV Export
// =============================================================================

function exportCSV(tableId) {
  const now = new Date().toISOString().slice(0, 10);
  tableManager.exportCSV(tableId, `ss_explorer_${tableId}_${now}.csv`);
}
