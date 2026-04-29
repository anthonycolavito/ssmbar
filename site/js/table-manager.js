// =============================================================================
// TableManager — Renders the year-by-year detail table and CSV exports.
//
// The detail table unifies working-year and retirement-year data: working
// rows (ages 21–64) carry earnings + net tax rate; retirement rows (age 65 to
// life expectancy) carry scheduled and payable annual benefits. When NMTR
// data is unavailable for the selected cohort (or globally pending), the
// working rows are omitted.
// =============================================================================

const tableManager = (() => {
  let allRows    = [];   // unified rows, ordered by age
  let cohortRows = [];   // most recent cohort-tab rows, used by Download CSV

  function render(cfg) {
    const a = cfg.annual;
    const n = cfg.nmtr;
    const includeWorking = dataLoader.hasNmtr(cfg) && !dataLoader.nmtrValuesPending();

    const includePbNmtr = !dataLoader.pbNmtrPending();
    const workingRows = includeWorking ? n.ages.map((age, i) => ({
      phase:            'working',
      age,
      year:             n.years[i],
      earnings_nominal: n.earnings_nominal[i],
      earnings_real:    n.earnings_real[i],
      net_tax_sched:    n.scheduled.values[i],
      net_tax_pay:      includePbNmtr ? n.payable.values[i] : null
    })) : [];

    const retirementRows = a.ages.map((age, i) => ({
      phase:         'retired',
      age,
      year:          a.years[i],
      nominal_sched: a.scheduled.nominal[i],
      nominal_pay:   a.payable.nominal[i],
      real_sched:    a.scheduled.real[i],
      real_pay:      a.payable.real[i]
    }));

    allRows = [...workingRows, ...retirementRows];
    renderTable(includeWorking);

    document.getElementById('downloadCsvBtn').onclick = () => downloadIndividualCsv();
  }

  function renderTable(includeWorking) {
    const tbl = document.getElementById('detailTable');
    const includePbNmtr = !dataLoader.pbNmtrPending();
    const headers = `
      <thead>
        <tr>
          <th rowspan="2">Age</th>
          <th rowspan="2">Year</th>
          <th class="text-end" colspan="2">Working-age Earnings</th>
          <th class="text-end" colspan="${includePbNmtr ? 2 : 1}">Net Tax Rate</th>
          <th class="text-end" colspan="2">Nominal Benefit</th>
          <th class="text-end" colspan="2">Real Benefit</th>
        </tr>
        <tr>
          <th class="text-end th-sub">Nominal</th>
          <th class="text-end th-sub">Real</th>
          <th class="text-end th-sub">Sched</th>
          ${includePbNmtr ? '<th class="text-end th-sub">Pay</th>' : ''}
          <th class="text-end th-sub">Sched</th>
          <th class="text-end th-sub">Pay</th>
          <th class="text-end th-sub">Sched</th>
          <th class="text-end th-sub">Pay</th>
        </tr>
      </thead>`;

    const blankNtrCell = includePbNmtr ? '<td class="text-end td-blank">—</td>' : '';
    const body = allRows.map(r => {
      if (r.phase === 'working') {
        const ntrPayCell = includePbNmtr
          ? `<td class="text-end td-secondary">${Fmt.percent(r.net_tax_pay)}</td>`
          : '';
        return `
          <tr>
            <td>${r.age}</td>
            <td>${r.year}</td>
            <td class="text-end">${Fmt.currency(r.earnings_nominal)}</td>
            <td class="text-end">${Fmt.currency(r.earnings_real)}</td>
            <td class="text-end">${Fmt.percent(r.net_tax_sched)}</td>
            ${ntrPayCell}
            <td class="text-end td-blank">—</td>
            <td class="text-end td-blank">—</td>
            <td class="text-end td-blank">—</td>
            <td class="text-end td-blank">—</td>
          </tr>`;
      }
      const blank = includeWorking ? '<td class="text-end td-blank">—</td>' : '';
      return `
        <tr>
          <td>${r.age}</td>
          <td>${r.year}</td>
          ${blank}
          ${blank}
          ${blank}
          ${includeWorking ? blankNtrCell : ''}
          <td class="text-end">${Fmt.currency(r.nominal_sched)}</td>
          <td class="text-end td-secondary">${Fmt.currency(r.nominal_pay)}</td>
          <td class="text-end">${Fmt.currency(r.real_sched)}</td>
          <td class="text-end td-secondary">${Fmt.currency(r.real_pay)}</td>
        </tr>`;
    }).join('');

    // When working rows are absent, drop the working-age column groups from
    // the header too so the column count matches the body.
    const narrowHeaders = `
      <thead>
        <tr>
          <th rowspan="2">Age</th>
          <th rowspan="2">Year</th>
          <th class="text-end" colspan="2">Nominal Benefit</th>
          <th class="text-end" colspan="2">Real Benefit</th>
        </tr>
        <tr>
          <th class="text-end th-sub">Sched</th>
          <th class="text-end th-sub">Pay</th>
          <th class="text-end th-sub">Sched</th>
          <th class="text-end th-sub">Pay</th>
        </tr>
      </thead>`;

    const narrowBody = allRows.map(r => `
      <tr>
        <td>${r.age}</td>
        <td>${r.year}</td>
        <td class="text-end">${Fmt.currency(r.nominal_sched)}</td>
        <td class="text-end td-secondary">${Fmt.currency(r.nominal_pay)}</td>
        <td class="text-end">${Fmt.currency(r.real_sched)}</td>
        <td class="text-end td-secondary">${Fmt.currency(r.real_pay)}</td>
      </tr>
    `).join('');

    tbl.innerHTML = includeWorking
      ? `${headers}<tbody>${body}</tbody>`
      : `${narrowHeaders}<tbody>${narrowBody}</tbody>`;
  }

  function downloadIndividualCsv() {
    const header = [
      'phase', 'age', 'year',
      'earnings_nominal', 'earnings_real',
      'net_tax_rate_scheduled', 'net_tax_rate_payable',
      'nominal_ben_scheduled', 'nominal_ben_payable',
      'real_ben_scheduled',    'real_ben_payable'
    ];
    const lines = [header.join(',')];
    for (const r of allRows) {
      if (r.phase === 'working') {
        lines.push([
          'working', r.age, r.year ?? '',
          r.earnings_nominal ?? '', r.earnings_real ?? '',
          r.net_tax_sched ?? '', r.net_tax_pay ?? '',
          '', '', '', ''
        ].join(','));
      } else {
        lines.push([
          'retired', r.age, r.year ?? '',
          '', '', '', '',
          r.nominal_sched ?? '', r.nominal_pay ?? '',
          r.real_sched ?? '',    r.real_pay ?? ''
        ].join(','));
      }
    }
    triggerDownload(lines.join('\n'), 'ssmbar_lifetime_detail.csv');
  }

  // Cohort-tab CSV: one row per birth year for the currently-selected
  // (worker, spouse) — every metric in both scenarios where applicable.
  function downloadCohortCsv(state) {
    const w = state.workerType, s = state.spouseType;
    const years = dataLoader.dimensions().birth_years;
    const monthly  = dataLoader.getCohortSeries(w, s, 'monthly_real_at_65');
    const rrCareer = dataLoader.getCohortSeries(w, s, 'rep_rate_career');
    const rrAwi    = dataLoader.getCohortSeries(w, s, 'rep_rate_awi');
    const pvBen    = dataLoader.getCohortSeries(w, s, 'pv_benefits');
    const pvTax    = dataLoader.getCohortSeries(w, s, 'pv_taxes');
    const ratio    = dataLoader.getCohortSeries(w, s, 'ben_tax_ratio');

    const irr = dataLoader.getCohortSeries(w, s, 'irr');

    const header = [
      'birth_year', 'worker_type', 'spouse_type',
      'monthly_real_at_65_scheduled', 'monthly_real_at_65_payable',
      'ben_tax_ratio_scheduled',      'ben_tax_ratio_payable',
      'irr_scheduled',                'irr_payable',
      'pv_benefits_scheduled',        'pv_benefits_payable',
      'pv_taxes',
      'rep_rate_career_scheduled',    'rep_rate_career_payable',
      'rep_rate_awi_scheduled',       'rep_rate_awi_payable'
    ];
    const lines = [header.join(',')];
    years.forEach((y, i) => {
      lines.push([
        y, w, s,
        monthly.scheduled[i] ?? '',  monthly.payable[i] ?? '',
        ratio.scheduled[i] ?? '',    ratio.payable[i] ?? '',
        irr.scheduled[i] ?? '',      irr.payable[i] ?? '',
        pvBen.scheduled[i] ?? '',    pvBen.payable[i] ?? '',
        pvTax.scheduled[i] ?? '',
        rrCareer.scheduled[i] ?? '', rrCareer.payable[i] ?? '',
        rrAwi.scheduled[i] ?? '',    rrAwi.payable[i] ?? ''
      ].join(','));
    });

    triggerDownload(lines.join('\n'), `ssmbar_cohort_${w}_${s}.csv`);
  }

  function triggerDownload(csv, filename) {
    const blob = new Blob([csv], { type: 'text/csv;charset=utf-8' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  }

  return { render, downloadCohortCsv };
})();
