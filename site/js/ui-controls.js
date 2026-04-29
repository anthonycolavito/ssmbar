// =============================================================================
// UIControls — Populates worker/spouse/cohort/real controls and emits state.
// =============================================================================

const uiControls = (() => {
  const state = {
    workerType:   'medium',
    spouseType:   'none',
    birthYear:    1980,
    real:         true,
    lifetimeView: 'primary'
  };

  const listeners = new Set();
  function onChange(fn) { listeners.add(fn); }
  function emit() { listeners.forEach(fn => fn({ ...state })); }

  function buildWorkerTypeControl(types) {
    const host = document.getElementById('workerTypeControl');
    host.innerHTML = '';
    types.forEach(t => {
      const btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'btn btn-outline-primary';
      btn.dataset.value = t.key;
      btn.textContent = t.label.replace(' Earner', '');
      if (t.key === state.workerType) btn.classList.add('active');
      btn.addEventListener('click', () => {
        if (state.workerType === t.key) return;
        state.workerType = t.key;
        host.querySelectorAll('button').forEach(b => b.classList.toggle('active', b.dataset.value === t.key));
        emit();
      });
      host.appendChild(btn);
    });
  }

  function buildSpouseControl(spouses) {
    const sel = document.getElementById('spouseControl');
    sel.innerHTML = '';
    spouses.forEach(s => {
      const opt = document.createElement('option');
      opt.value = s.key;
      opt.textContent = s.label;
      if (s.key === state.spouseType) opt.selected = true;
      sel.appendChild(opt);
    });
    sel.addEventListener('change', () => {
      state.spouseType = sel.value;
      emit();
    });
  }

  function buildCohortControl(years) {
    const sel = document.getElementById('cohortControl');
    sel.innerHTML = '';
    years.forEach(y => {
      const opt = document.createElement('option');
      opt.value = String(y);
      opt.textContent = String(y);
      if (y === state.birthYear) opt.selected = true;
      sel.appendChild(opt);
    });
    sel.addEventListener('change', () => {
      state.birthYear = parseInt(sel.value, 10);
      emit();
    });
  }

  function buildRealToggle() {
    const host = document.getElementById('realToggleControl');
    host.querySelectorAll('button').forEach(btn => {
      btn.addEventListener('click', () => {
        const isReal = btn.dataset.mode === 'real';
        if (isReal === state.real) return;
        state.real = isReal;
        host.querySelectorAll('button').forEach(b => b.classList.toggle('active', b.dataset.mode === btn.dataset.mode));
        emit();
      });
    });
  }

  function buildLifetimeViewToggle() {
    const host = document.getElementById('lifetimeViewControl');
    if (!host) return;
    host.querySelectorAll('button').forEach(btn => {
      btn.addEventListener('click', () => {
        const view = btn.dataset.view;
        if (view === state.lifetimeView) return;
        state.lifetimeView = view;
        host.querySelectorAll('button').forEach(b => b.classList.toggle('active', b.dataset.view === view));
        emit();
      });
    });
  }

  function buildTabs(onTab) {
    document.querySelectorAll('.nav-tabs .nav-link').forEach(btn => {
      btn.addEventListener('click', () => {
        const tab = btn.dataset.tab;
        document.querySelectorAll('.nav-tabs .nav-link').forEach(b => b.classList.toggle('active', b === btn));
        document.querySelectorAll('.tab-panel').forEach(p => {
          const isTarget = p.id === `panel-${tab}`;
          p.hidden = !isTarget;
        });
        onTab(tab);
      });
    });
  }

  function init({ dimensions, onTab }) {
    buildWorkerTypeControl(dimensions.worker_types);
    buildSpouseControl(dimensions.spouse_types);
    buildCohortControl(dimensions.birth_years);
    buildRealToggle();
    buildLifetimeViewToggle();
    buildTabs(onTab);
  }

  function getState() { return { ...state }; }

  return { init, onChange, getState };
})();
