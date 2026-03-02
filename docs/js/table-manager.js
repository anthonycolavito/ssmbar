// =============================================================================
// TableManager — Data table rendering, pagination, and CSV export
// =============================================================================

class TableManager {
  constructor() {
    this.tables = {};
    this.pageSize = 20;
  }

  render(wrapperId, { columns, rows, id }) {
    const wrapper = document.getElementById(wrapperId);
    if (!wrapper) return;

    this.tables[id] = { columns, rows };

    if (!rows || rows.length === 0) {
      wrapper.innerHTML = '<div class="text-muted-custom text-center p-3" style="font-size:0.8rem">No data available</div>';
      return;
    }

    this._renderPage(wrapperId, id, 0);
  }

  _renderPage(wrapperId, id, page) {
    const wrapper = document.getElementById(wrapperId);
    if (!wrapper) return;

    const { columns, rows } = this.tables[id];
    const totalPages = Math.ceil(rows.length / this.pageSize);
    const start = page * this.pageSize;
    const end = Math.min(start + this.pageSize, rows.length);
    const pageRows = rows.slice(start, end);

    let html = '<table class="data-table"><thead><tr>';
    for (const col of columns) html += `<th>${col.label}</th>`;
    html += '</tr></thead><tbody>';

    for (const row of pageRows) {
      html += '<tr>';
      for (const col of columns) {
        const val = row[col.key];
        const formatted = col.format ? col.format(val) : (val ?? '--');
        html += `<td>${formatted}</td>`;
      }
      html += '</tr>';
    }
    html += '</tbody></table>';

    if (totalPages > 1) {
      html += '<div class="table-controls">';
      html += `<span>Showing ${start + 1}-${end} of ${rows.length}</span>`;
      html += '<div class="pagination">';
      if (page > 0) html += `<button onclick="tableManager.navigatePage('${wrapperId}','${id}',${page - 1})">Prev</button>`;
      for (let p = 0; p < totalPages; p++) {
        html += `<button class="${p === page ? 'active' : ''}" onclick="tableManager.navigatePage('${wrapperId}','${id}',${p})">${p + 1}</button>`;
      }
      if (page < totalPages - 1) html += `<button onclick="tableManager.navigatePage('${wrapperId}','${id}',${page + 1})">Next</button>`;
      html += '</div></div>';
    }

    wrapper.innerHTML = html;
  }

  navigatePage(wrapperId, id, page) {
    this._renderPage(wrapperId, id, page);
  }

  exportCSV(id, filename) {
    const tableData = this.tables[id];
    if (!tableData) return;

    const { columns, rows } = tableData;
    const headers = columns.map(c => c.label).join(',');
    const csvRows = rows.map(row =>
      columns.map(col => {
        let val = row[col.key];
        if (val == null) return '';
        val = String(val);
        if (val.includes(',') || val.includes('"') || val.includes('\n')) {
          val = '"' + val.replace(/"/g, '""') + '"';
        }
        return val;
      }).join(',')
    );

    const csv = headers + '\n' + csvRows.join('\n');
    const blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });
    const link = document.createElement('a');
    link.href = URL.createObjectURL(blob);
    link.download = filename || `${id}_export.csv`;
    link.click();
    URL.revokeObjectURL(link.href);
  }

  // =========================================================================
  // Table builders
  // =========================================================================

  renderBenefitsTable(series) {
    if (!series) return;

    const columns = [
      { key: 'age', label: 'Age' },
      { key: 'nominal', label: 'Nominal ($)', format: v => Fmt.currency(v) },
      { key: 'real', label: 'Real ($)', format: v => Fmt.currency(v) }
    ];

    const rows = series.ages.map((age, i) => ({
      age,
      nominal: series.nominal[i],
      real: series.real[i]
    }));

    this.render('benefitsTableWrapper', { columns, rows, id: 'benefits' });
  }

  renderCohortTable(data, reformData, reformLabel, replField) {
    if (!data) return;

    const columns = [
      { key: 'birth_year', label: 'Birth Year' },
      { key: 'repl', label: 'Repl Rate', format: v => Fmt.percent(v) },
      { key: 'pv', label: 'PV Benefits', format: v => Fmt.currency(v, { compact: true }) },
      { key: 'ratio', label: 'Ratio', format: v => Fmt.number(v) },
      { key: 'irr', label: 'IRR', format: v => Fmt.percent(v) },
      { key: 'death_age', label: 'Life Exp.', format: v => v != null ? v.toFixed(1) : '--' }
    ];

    const rows = data.birth_years.map((by, i) => ({
      birth_year: by,
      repl: data[replField]?.[i],
      pv: data.pv_benefits[i],
      ratio: data.ratio[i],
      irr: data.irr[i],
      death_age: data.death_age?.[i]
    }));

    this.render('cohortTableWrapper', { columns, rows, id: 'cohort' });
  }
}

const tableManager = new TableManager();
