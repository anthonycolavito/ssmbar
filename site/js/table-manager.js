// =============================================================================
// TableManager — Renders the year-by-year detail table and CSV export.
// Defaults to ages 65 through cohort life-expectancy at 65; expander reveals
// the full life table to age 119. Each retirement row shows scheduled and
// payable benefits side by side.
// =============================================================================

const tableManager = (() => {
  let allRows = [];
  let leAge   = null;
  let workingRows = [];   // earnings ages 21–64 (used for CSV export only)
  let showFull = false;

  function render(cfg) {
    leAge = cfg.summary.death_age;
    const a = cfg.annual;
    allRows = a.ages.map((age, i) => ({
      age,
      year:           a.years[i],
      earnings:       a.earnings[i],
      nominal_sched:  a.scheduled.nominal[i],
      real_sched:     a.scheduled.real[i],
      nominal_pay:    a.payable.nominal[i],
      real_pay:       a.payable.real[i]
    }));

    workingRows = cfg.nmtr.ages.map((age, i) => ({
      age,
      year:             cfg.nmtr.years ? cfg.nmtr.years[i] : null,
      earnings_nominal: cfg.nmtr.earnings_nominal ? cfg.nmtr.earnings_nominal[i] : null,
      earnings_real:    cfg.nmtr.earnings_real    ? cfg.nmtr.earnings_real[i]    : null
    }));

    showFull = false;
    renderTable();

    document.getElementById('downloadCsvBtn').onclick = () => downloadCsv();
    document.getElementById('toggleFullTableBtn').onclick = () => {
      showFull = !showFull;
      renderTable();
    };
  }

  function renderTable() {
    const visible = showFull ? allRows
      : (leAge != null ? allRows.filter(r => r.age <= leAge) : allRows);

    const tbl = document.getElementById('detailTable');
    tbl.innerHTML = `
      <thead>
        <tr>
          <th rowspan="2">Age</th>
          <th rowspan="2">Year</th>
          <th class="text-end" colspan="2">Nominal Benefit</th>
          <th class="text-end" colspan="2">Real Benefit</th>
        </tr>
        <tr>
          <th class="text-end th-sub">Scheduled</th>
          <th class="text-end th-sub">Payable</th>
          <th class="text-end th-sub">Scheduled</th>
          <th class="text-end th-sub">Payable</th>
        </tr>
      </thead>
      <tbody>
        ${visible.map(r => `
          <tr>
            <td>${r.age}</td>
            <td>${r.year}</td>
            <td class="text-end">${Fmt.currency(r.nominal_sched)}</td>
            <td class="text-end td-secondary">${Fmt.currency(r.nominal_pay)}</td>
            <td class="text-end">${Fmt.currency(r.real_sched)}</td>
            <td class="text-end td-secondary">${Fmt.currency(r.real_pay)}</td>
          </tr>
        `).join('')}
      </tbody>
    `;

    const btn = document.getElementById('toggleFullTableBtn');
    if (btn) {
      btn.innerHTML = showFull
        ? '<i class="bi bi-arrows-collapse"></i> Collapse to life expectancy'
        : '<i class="bi bi-arrows-expand"></i> Show full life table';
    }
  }

  function downloadCsv() {
    const header = [
      'phase', 'age', 'year',
      'earnings_nominal', 'earnings_real',
      'nominal_ben_scheduled', 'nominal_ben_payable',
      'real_ben_scheduled', 'real_ben_payable'
    ];
    const lines = [header.join(',')];

    for (const r of workingRows) {
      lines.push([
        'working', r.age, r.year ?? '',
        r.earnings_nominal ?? '', r.earnings_real ?? '',
        '', '', '', ''
      ].join(','));
    }
    for (const r of allRows) {
      lines.push([
        'retired', r.age, r.year ?? '',
        '', '',
        r.nominal_sched ?? '', r.nominal_pay ?? '',
        r.real_sched ?? '',    r.real_pay ?? ''
      ].join(','));
    }

    const blob = new Blob([lines.join('\n')], { type: 'text/csv;charset=utf-8' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'ssmbar_lifetime_detail.csv';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  }

  return { render };
})();
