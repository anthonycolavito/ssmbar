// =============================================================================
// TableManager — Data table rendering, pagination, and CSV export
// =============================================================================

class TableManager {
  constructor() {
    this.tables = {};  // Store table data for export
    this.pageSize = 20;
  }

  /**
   * Render an HTML table into a wrapper element
   * @param {string} wrapperId - DOM id of wrapper div
   * @param {object} config - { columns, rows, id }
   */
  render(wrapperId, { columns, rows, id }) {
    const wrapper = document.getElementById(wrapperId);
    if (!wrapper) return;

    this.tables[id] = { columns, rows };

    if (!rows || rows.length === 0) {
      wrapper.innerHTML = '<div class="text-muted-custom text-center p-3" style="font-size:0.8rem">No data available</div>';
      return;
    }

    const totalPages = Math.ceil(rows.length / this.pageSize);
    let currentPage = 0;

    const renderPage = (page) => {
      currentPage = page;
      const start = page * this.pageSize;
      const end = Math.min(start + this.pageSize, rows.length);
      const pageRows = rows.slice(start, end);

      let html = '<table class="data-table"><thead><tr>';
      for (const col of columns) {
        html += `<th>${col.label}</th>`;
      }
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

      // Pagination
      if (totalPages > 1) {
        html += '<div class="table-controls">';
        html += `<span>Showing ${start + 1}-${end} of ${rows.length}</span>`;
        html += '<div class="pagination">';
        if (currentPage > 0) {
          html += `<button onclick="tableManager.navigatePage('${wrapperId}', '${id}', ${page - 1})">Prev</button>`;
        }
        for (let p = 0; p < totalPages; p++) {
          const active = p === currentPage ? ' active' : '';
          html += `<button class="${active}" onclick="tableManager.navigatePage('${wrapperId}', '${id}', ${p})">${p + 1}</button>`;
        }
        if (currentPage < totalPages - 1) {
          html += `<button onclick="tableManager.navigatePage('${wrapperId}', '${id}', ${page + 1})">Next</button>`;
        }
        html += '</div></div>';
      }

      wrapper.innerHTML = html;
    };

    renderPage(0);
  }

  /**
   * Navigate to a specific page
   */
  navigatePage(wrapperId, id, page) {
    const tableData = this.tables[id];
    if (!tableData) return;
    this.render(wrapperId, { ...tableData, id });
    // Re-render starts at page 0; we need to build a stateful approach
    // Simpler: re-render with offset
    const wrapper = document.getElementById(wrapperId);
    if (!wrapper) return;

    const { columns, rows } = tableData;
    const totalPages = Math.ceil(rows.length / this.pageSize);
    const start = page * this.pageSize;
    const end = Math.min(start + this.pageSize, rows.length);
    const pageRows = rows.slice(start, end);

    let html = '<table class="data-table"><thead><tr>';
    for (const col of columns) {
      html += `<th>${col.label}</th>`;
    }
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
      if (page > 0) {
        html += `<button onclick="tableManager.navigatePage('${wrapperId}', '${id}', ${page - 1})">Prev</button>`;
      }
      for (let p = 0; p < totalPages; p++) {
        const active = p === page ? ' active' : '';
        html += `<button class="${active}" onclick="tableManager.navigatePage('${wrapperId}', '${id}', ${p})">${p + 1}</button>`;
      }
      if (page < totalPages - 1) {
        html += `<button onclick="tableManager.navigatePage('${wrapperId}', '${id}', ${page + 1})">Next</button>`;
      }
      html += '</div></div>';
    }

    wrapper.innerHTML = html;
  }

  /**
   * Export table data as CSV
   */
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
        // Escape commas and quotes
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
  // Table builders for specific views
  // =========================================================================

  /**
   * Build benefits data table
   */
  renderBenefitsTable(baselineSeries, reformSeries, reformLabel) {
    if (!baselineSeries) return;

    const columns = [
      { key: 'age', label: 'Age' },
      { key: 'nominal_base', label: 'Nominal (Base)', format: v => Fmt.currency(v) },
      { key: 'real_base', label: 'Real (Base)', format: v => Fmt.currency(v) },
      { key: 'bc', label: 'Class' }
    ];

    if (reformSeries) {
      columns.splice(3, 0,
        { key: 'nominal_reform', label: `Nominal (${reformLabel})`, format: v => Fmt.currency(v) },
        { key: 'real_reform', label: `Real (${reformLabel})`, format: v => Fmt.currency(v) }
      );
    }

    const rows = baselineSeries.ages.map((age, i) => {
      const row = {
        age,
        nominal_base: baselineSeries.nominal[i],
        real_base: baselineSeries.real[i],
        bc: baselineSeries.bc[i]
      };
      if (reformSeries) {
        row.nominal_reform = reformSeries.nominal?.[i];
        row.real_reform = reformSeries.real?.[i];
      }
      return row;
    });

    this.render('benefitsTableWrapper', { columns, rows, id: 'benefits' });
  }

  /**
   * Build marginal analysis data table
   */
  renderMarginalTable(baselineMarginal, reformMarginal, reformLabel) {
    if (!baselineMarginal) return;

    const columns = [
      { key: 'age', label: 'Age' },
      { key: 'earnings', label: 'Earnings', format: v => Fmt.currency(v) },
      { key: 'nmtr_base', label: 'NMTR (Base)', format: v => Fmt.percent(v) },
      { key: 'delta_pv_base', label: 'Delta PV (Base)', format: v => Fmt.currency(v) }
    ];

    if (reformMarginal) {
      columns.push(
        { key: 'nmtr_reform', label: `NMTR (${reformLabel})`, format: v => Fmt.percent(v) },
        { key: 'delta_pv_reform', label: `Delta PV (${reformLabel})`, format: v => Fmt.currency(v) }
      );
    }

    const rows = baselineMarginal.ages.map((age, i) => {
      const row = {
        age,
        earnings: baselineMarginal.earnings[i],
        nmtr_base: baselineMarginal.nmtr[i],
        delta_pv_base: baselineMarginal.delta_pv?.[i]
      };
      if (reformMarginal) {
        row.nmtr_reform = reformMarginal.nmtr?.[i];
        row.delta_pv_reform = reformMarginal.delta_pv?.[i];
      }
      return row;
    });

    this.render('marginalTableWrapper', { columns, rows, id: 'marginal' });
  }

  /**
   * Build cohort data table
   */
  renderCohortTable(baselineData, reformData, reformLabel, replField) {
    if (!baselineData) return;

    const columns = [
      { key: 'birth_year', label: 'Birth Year' },
      { key: 'repl_base', label: 'Repl% (Base)', format: v => Fmt.percent(v) },
      { key: 'pv_base', label: 'PV Ben (Base)', format: v => Fmt.currency(v, { compact: true }) },
      { key: 'ratio_base', label: 'Ratio (Base)', format: v => Fmt.number(v) },
      { key: 'irr_base', label: 'IRR% (Base)', format: v => Fmt.percent(v) }
    ];

    if (reformData) {
      columns.push(
        { key: 'repl_reform', label: `Repl% (${reformLabel})`, format: v => Fmt.percent(v) },
        { key: 'pv_reform', label: `PV Ben (${reformLabel})`, format: v => Fmt.currency(v, { compact: true }) },
        { key: 'ratio_reform', label: `Ratio (${reformLabel})`, format: v => Fmt.number(v) },
        { key: 'irr_reform', label: `IRR% (${reformLabel})`, format: v => Fmt.percent(v) }
      );
    }

    const rows = baselineData.birth_years.map((by, i) => {
      const row = {
        birth_year: by,
        repl_base: baselineData[replField]?.[i],
        pv_base: baselineData.pv_benefits[i],
        ratio_base: baselineData.ratio[i],
        irr_base: baselineData.irr[i]
      };
      if (reformData) {
        row.repl_reform = reformData[replField]?.[i];
        row.pv_reform = reformData.pv_benefits[i];
        row.ratio_reform = reformData.ratio[i];
        row.irr_reform = reformData.irr[i];
      }
      return row;
    });

    this.render('cohortTableWrapper', { columns, rows, id: 'cohort' });
  }
}

// Global instance
const tableManager = new TableManager();
