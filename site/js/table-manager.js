// =============================================================================
// TableManager — Renders the year-by-year detail table and CSV export.
// =============================================================================

const tableManager = (() => {
  function render(cfg) {
    const rows = cfg.annual.ages.map((age, i) => ({
      age,
      year:        cfg.annual.years[i],
      earnings:    cfg.annual.earnings[i],
      nominal_ben: cfg.annual.nominal[i],
      real_ben:    cfg.annual.real[i]
    }));

    const tbl = document.getElementById('detailTable');
    tbl.innerHTML = `
      <thead>
        <tr>
          <th>Age</th>
          <th>Year</th>
          <th class="text-end">Earnings</th>
          <th class="text-end">Nominal Benefit</th>
          <th class="text-end">Real Benefit</th>
        </tr>
      </thead>
      <tbody>
        ${rows.map(r => `
          <tr>
            <td>${r.age}</td>
            <td>${r.year}</td>
            <td class="text-end">${Fmt.currency(r.earnings)}</td>
            <td class="text-end">${Fmt.currency(r.nominal_ben)}</td>
            <td class="text-end">${Fmt.currency(r.real_ben)}</td>
          </tr>
        `).join('')}
      </tbody>
    `;

    document.getElementById('downloadCsvBtn').onclick = () => downloadCsv(rows);
  }

  function downloadCsv(rows) {
    const header = ['age', 'year', 'earnings', 'nominal_ben', 'real_ben'];
    const lines = [header.join(',')].concat(
      rows.map(r => [r.age, r.year, r.earnings, r.nominal_ben, r.real_ben].join(','))
    );
    const blob = new Blob([lines.join('\n')], { type: 'text/csv;charset=utf-8' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'ssmbar_annual_detail.csv';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  }

  return { render };
})();
