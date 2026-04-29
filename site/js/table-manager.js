// =============================================================================
// TableManager — Renders the year-by-year detail table and CSV export.
// Defaults to ages 65 through cohort life-expectancy at 65; expander reveals
// the full life table to age 119.
// =============================================================================

const tableManager = (() => {
  let allRows = [];
  let leAge   = null;
  let workingRows = [];   // earnings ages 21–64 (used for CSV export only)
  let showFull = false;

  function render(cfg) {
    leAge = cfg.summary.death_age;
    allRows = cfg.annual.ages.map((age, i) => ({
      age,
      year:        cfg.annual.years[i],
      earnings:    cfg.annual.earnings[i],
      nominal_ben: cfg.annual.nominal[i],
      real_ben:    cfg.annual.real[i]
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
          <th>Age</th>
          <th>Year</th>
          <th class="text-end">Nominal Benefit</th>
          <th class="text-end">Real Benefit</th>
        </tr>
      </thead>
      <tbody>
        ${visible.map(r => `
          <tr>
            <td>${r.age}</td>
            <td>${r.year}</td>
            <td class="text-end">${Fmt.currency(r.nominal_ben)}</td>
            <td class="text-end">${Fmt.currency(r.real_ben)}</td>
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
    // CSV export includes BOTH working-year earnings (ages 21–64) and
    // retirement-year benefits (65 → 119), with a `phase` column.
    const header = ['phase', 'age', 'year', 'earnings_nominal', 'earnings_real', 'nominal_ben', 'real_ben'];
    const lines = [header.join(',')];

    for (const r of workingRows) {
      lines.push([
        'working', r.age, r.year ?? '',
        r.earnings_nominal ?? '', r.earnings_real ?? '',
        '', ''
      ].join(','));
    }
    for (const r of allRows) {
      lines.push([
        'retired', r.age, r.year ?? '',
        '', '',
        r.nominal_ben ?? '', r.real_ben ?? ''
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
