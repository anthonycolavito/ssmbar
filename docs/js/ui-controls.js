// =============================================================================
// UI Controls — Dropdown population, sidebar, toggle, and event wiring
// =============================================================================

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

function toggleSidebar() {
  const sidebar = document.getElementById('sidebar');
  const overlay = document.getElementById('sidebarOverlay');
  sidebar.classList.toggle('open');
  overlay.classList.toggle('open');
}

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
// Reform controls (all disabled for current-law mode)
// =============================================================================

function disableAllReforms() {
  document.querySelectorAll('.reform-section input[type="radio"]').forEach(radio => {
    radio.disabled = true;
  });
  document.querySelectorAll('.reform-section').forEach(section => {
    section.classList.add('disabled');
  });
}

function getComboKey() {
  return 'baseline';
}

function hasReformSelected() {
  return false;
}

function clearReform() {
  // No-op in current-law mode
}

function updateReformBadge() {
  const badge = document.getElementById('selectedReformBadge');
  const noBadge = document.getElementById('noReformBadge');
  if (badge) badge.style.display = 'none';
  if (noBadge) noBadge.style.display = 'block';
}

// =============================================================================
// Worker dimension state
// =============================================================================

function getIndSex() {
  return document.querySelector('input[name="indSex"]:checked')?.value || 'male';
}

function getIndMarital() {
  return document.querySelector('input[name="indMarital"]:checked')?.value || 'single';
}

function getCohSex() {
  return document.querySelector('input[name="cohSex"]:checked')?.value || 'male';
}

function getCohMarital() {
  return document.querySelector('input[name="cohMarital"]:checked')?.value || 'single';
}

// =============================================================================
// Tab switching
// =============================================================================

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
// Benefit view toggle (nominal/real) — default to real
// =============================================================================

let currentBenefitView = 'real';

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
