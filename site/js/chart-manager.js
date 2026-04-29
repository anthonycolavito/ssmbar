// =============================================================================
// ChartManager — Chart.js helpers (light theme, current-law only).
// Requires Chart.js v4 + chartjs-plugin-annotation v3 (loaded via CDN).
//
// Most line/cohort helpers accept a primary dataset plus an optional secondary
// dataset (used for the payable scenario shown alongside scheduled). When the
// secondary is present it renders dashed in a lighter shade of the primary
// colour, and tooltip labels disambiguate "Scheduled" vs "Payable".
// =============================================================================

const CHART_COLORS = {
  line:               '#2563EB',
  lineSecondary:      'rgba(37, 99, 235, 0.55)',
  fill:               'rgba(37, 99, 235, 0.05)',
  earnings:           '#0D9488',
  earningsSecondary:  'rgba(13, 148, 136, 0.55)',
  benefits:           '#2563EB',
  benefitsSecondary:  'rgba(37, 99, 235, 0.55)',
  taxAccent:          '#D97706',
  taxAccentSecondary: 'rgba(217, 119, 6, 0.55)',
  muted:              '#9ca3af',
  grid:               'rgba(0, 0, 0, 0.06)',
  axis:               '#6b7280'
};

const SECONDARY_DASH = [6, 4];

// Convert an age (possibly fractional, e.g., 66.8333 for "66y10m") to its
// index along a category x-axis whose categories are integer ages. Returns
// -1 if the floor age is not in the labels array.
function ageToIndex(ages, age) {
  if (age == null) return -1;
  const lo  = Math.floor(age);
  const idx = ages.indexOf(lo);
  return idx < 0 ? -1 : idx + (age - lo);
}

// Annotation for Normal Retirement Age: dashed navy vertical with a label
// at the bottom, distinct from the existing claim-age (top, dark) and LE
// (top-right, amber) annotations.
function nraAnnotation(nraIdx, nraAge) {
  return {
    type: 'line',
    xMin: nraIdx, xMax: nraIdx,
    borderColor: 'rgba(15, 23, 65, 0.45)',
    borderWidth: 1,
    borderDash: [5, 3],
    label: {
      display: true,
      content: `NRA ${Fmt.yearsMonths(nraAge)}`,
      position: 'end',
      backgroundColor: 'rgba(15, 23, 65, 0.85)',
      color: '#fff',
      font: { family: 'Inter', size: 10 },
      padding: 3
    }
  };
}

function makeDefaults() {
  return {
    responsive: true,
    maintainAspectRatio: false,
    interaction: { mode: 'index', intersect: false },
    animation: { duration: 350, easing: 'easeOutQuart' },
    plugins: {
      legend: { display: false },
      tooltip: {
        backgroundColor: '#0f1741',
        titleColor: '#e8e8e8',
        bodyColor:  '#e8e8e8',
        borderColor: '#2a3f5f',
        borderWidth: 1,
        cornerRadius: 6,
        padding: 10,
        bodyFont:  { family: 'Inter', size: 12 },
        titleFont: { family: 'Inter', size: 12, weight: 'bold' }
      },
      subtitle: {
        display: false,
        color: CHART_COLORS.axis,
        font: { family: 'Inter', size: 11, style: 'italic' },
        padding: { bottom: 8 }
      }
    },
    scales: {
      x: {
        ticks: {
          color: CHART_COLORS.axis,
          font: { family: 'Inter', size: 11 },
          maxTicksLimit: 8
        },
        grid: { color: CHART_COLORS.grid }
      },
      y: {
        ticks: {
          color: CHART_COLORS.axis,
          font: { family: 'Inter', size: 11 },
          maxTicksLimit: 6
        },
        grid: { color: CHART_COLORS.grid }
      }
    }
  };
}

const chartManager = (() => {
  const charts = {};

  function destroyExisting(canvasId) {
    if (charts[canvasId]) {
      charts[canvasId].destroy();
      delete charts[canvasId];
    }
  }

  // Generic line chart used for the secondary annual-benefits chart.
  // Pass `dataSecondary` to render a second (payable) line alongside the first.
  function lineChart(canvasId, opts) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    destroyExisting(canvasId);

    const {
      labels, data, dataSecondary = null,
      yFormat = 'currency', yMin = null, yMax = null,
      subtitle = null, leMarker = null, nraAge = null, fadeAfterIdx = null
    } = opts;

    const o = makeDefaults();
    if (yMin != null) o.scales.y.min = yMin;
    if (yMax != null) o.scales.y.max = yMax;
    o.scales.y.ticks.callback = (v) => formatYTick(v, yFormat);
    o.plugins.tooltip.callbacks = {
      title: (items) => `Age ${items[0].label}`,
      label: (item)  => {
        const prefix = dataSecondary ? `${item.dataset.label}: ` : '';
        return `${prefix}${formatYTick(item.parsed.y, yFormat)}`;
      }
    };
    if (subtitle) {
      o.plugins.subtitle.display = true;
      o.plugins.subtitle.text = subtitle;
    }
    const annotations = {};
    if (leMarker != null) {
      const leIdx = labels.indexOf(leMarker);
      if (leIdx >= 0) {
        annotations.le = {
          type: 'line',
          xMin: leIdx, xMax: leIdx,
          borderColor: 'rgba(217, 119, 6, 0.6)',
          borderWidth: 1,
          borderDash: [3, 3],
          label: {
            display: true,
            content: `Life Expectancy ≈ ${leMarker}`,
            position: 'start',
            backgroundColor: 'rgba(217, 119, 6, 0.9)',
            color: '#fff',
            font: { family: 'Inter', size: 10 },
            padding: 3
          }
        };
      }
    }
    if (nraAge != null) {
      const nraIdx = ageToIndex(labels, nraAge);
      if (nraIdx >= 0) annotations.nra = nraAnnotation(nraIdx, nraAge);
    }
    if (Object.keys(annotations).length > 0) {
      o.plugins.annotation = { annotations };
    }

    const primary = {
      label: dataSecondary ? 'Scheduled' : '',
      data,
      borderColor: CHART_COLORS.line,
      backgroundColor: CHART_COLORS.fill,
      fill: false,
      tension: 0.2,
      pointRadius: 0,
      pointHoverRadius: 4,
      borderWidth: 2,
      spanGaps: true
    };

    if (fadeAfterIdx != null) {
      primary.segment = {
        borderColor: c => c.p1DataIndex > fadeAfterIdx ? 'rgba(37, 99, 235, 0.35)' : CHART_COLORS.line,
        borderDash:  c => c.p1DataIndex > fadeAfterIdx ? [4, 4] : undefined
      };
    }

    const datasets = [primary];
    if (dataSecondary) {
      datasets.push({
        label: 'Payable',
        data: dataSecondary,
        borderColor: CHART_COLORS.lineSecondary,
        backgroundColor: 'transparent',
        fill: false,
        tension: 0.2,
        pointRadius: 0,
        pointHoverRadius: 4,
        borderWidth: 2,
        borderDash: SECONDARY_DASH,
        spanGaps: true
      });
    }

    charts[canvasId] = new Chart(ctx, {
      type: 'line',
      data: { labels, datasets },
      options: o
    });
  }

  // Lifetime profile. Always plots the scheduled line with the existing
  // earnings/benefit two-color segment encoding. `valuesSecondary` (payable)
  // overlays a dashed line; in working years it duplicates the scheduled
  // earnings (same data) and in retirement diverges below.
  function lifetimeProfileChart(canvasId, { ages, values, valuesSecondary = null, transitionIdx, leAge, nraAge = null, subtitle = null }) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    destroyExisting(canvasId);

    const o = makeDefaults();
    o.scales.y.min = 0;
    o.scales.y.ticks.callback = (v) => formatYTick(v, 'currency');
    o.plugins.tooltip.callbacks = {
      title: (items) => `Age ${items[0].label}`,
      label: (item)  => {
        const inWorkYears = item.dataIndex < transitionIdx;
        if (valuesSecondary && !inWorkYears) {
          return `${item.dataset.label}: ${formatYTick(item.parsed.y, 'currency')}`;
        }
        const phase = inWorkYears ? 'Earnings' : 'Benefit';
        return `${phase}: ${formatYTick(item.parsed.y, 'currency')}`;
      }
    };
    if (subtitle) {
      o.plugins.subtitle.display = true;
      o.plugins.subtitle.text = subtitle;
    }
    const claimIdx = transitionIdx;
    const leIdx    = ages.indexOf(leAge);
    const nraIdx   = ageToIndex(ages, nraAge);
    const annotations = {
      claim: {
        type: 'line', xMin: claimIdx, xMax: claimIdx,
        borderColor: 'rgba(0, 0, 0, 0.25)',
        borderWidth: 1, borderDash: [2, 4],
        label: {
          display: true, content: 'Claim age 65', position: 'start',
          backgroundColor: 'rgba(0, 0, 0, 0.65)', color: '#fff',
          font: { family: 'Inter', size: 10 }, padding: 3
        }
      }
    };
    if (leIdx >= 0) {
      annotations.le = {
        type: 'line', xMin: leIdx, xMax: leIdx,
        borderColor: 'rgba(217, 119, 6, 0.6)',
        borderWidth: 1, borderDash: [3, 3],
        label: {
          display: true, content: `Life Expectancy ≈ ${leAge}`, position: 'end',
          backgroundColor: 'rgba(217, 119, 6, 0.9)', color: '#fff',
          font: { family: 'Inter', size: 10 }, padding: 3
        }
      };
    }
    if (nraIdx >= 0) {
      annotations.nra = nraAnnotation(nraIdx, nraAge);
    }
    o.plugins.annotation = { annotations };

    const primary = {
      label: valuesSecondary ? 'Scheduled' : '',
      data: values,
      borderColor: CHART_COLORS.benefits,
      fill: false,
      tension: 0.2,
      pointRadius: 0,
      pointHoverRadius: 4,
      borderWidth: 2.5,
      segment: {
        borderColor: c => c.p1DataIndex < transitionIdx ? CHART_COLORS.earnings : CHART_COLORS.benefits
      }
    };

    const datasets = [primary];
    if (valuesSecondary) {
      datasets.push({
        label: 'Payable',
        data: valuesSecondary,
        borderColor: CHART_COLORS.benefitsSecondary,
        fill: false,
        tension: 0.2,
        pointRadius: 0,
        pointHoverRadius: 4,
        borderWidth: 2,
        borderDash: SECONDARY_DASH,
        segment: {
          borderColor: c => c.p1DataIndex < transitionIdx ? CHART_COLORS.earningsSecondary : CHART_COLORS.benefitsSecondary
        }
      });
    }

    charts[canvasId] = new Chart(ctx, {
      type: 'line',
      data: { labels: ages, datasets },
      options: o
    });
  }

  // Marginal IRR by working age. Two lines (scheduled / payable) with a
  // boxed annotation covering the leading uninsured ages — that's the
  // "clever" treatment for the first 10 years where return is undefined
  // (post-hoc-zeroed at age 21, then zero through pre-insurance years).
  function marginalIrrChart(canvasId, { ages, valuesScheduled, valuesPayable, subtitle = null }) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    destroyExisting(canvasId);

    // First age where either scenario produces a real value.
    let firstWithData = ages.length;
    for (let i = 0; i < ages.length; i++) {
      if (valuesScheduled[i] != null || valuesPayable[i] != null) { firstWithData = i; break; }
    }

    const o = makeDefaults();
    o.scales.y.ticks.callback = (v) => Fmt.percent(v);
    o.plugins.tooltip.callbacks = {
      title: (items) => `Age ${items[0].label}`,
      label: (item)  => {
        if (item.parsed.y == null) return null;
        return `${item.dataset.label}: ${Fmt.percent(item.parsed.y)}`;
      }
    };
    if (subtitle) {
      o.plugins.subtitle.display = true;
      o.plugins.subtitle.text = subtitle;
    }

    const annotations = {
      zero: {
        type: 'line', yMin: 0, yMax: 0,
        borderColor: 'rgba(0, 0, 0, 0.25)',
        borderWidth: 1, borderDash: [2, 4]
      }
    };
    if (firstWithData > 0) {
      annotations.uninsured = {
        type: 'box',
        xMin: -0.5,
        xMax: firstWithData - 0.5,
        backgroundColor: 'rgba(217, 119, 6, 0.08)',
        borderColor:    'rgba(217, 119, 6, 0.30)',
        borderWidth: 1,
        borderDash: [4, 4],
        label: {
          display: true,
          content: ['Uninsured', 'return = −100%'],
          position: 'center',
          backgroundColor: 'rgba(217, 119, 6, 0.85)',
          color: '#fff',
          font: { family: 'Inter', size: 10 },
          padding: 4
        }
      };
    }
    o.plugins.annotation = { annotations };

    const shared = {
      fill: false, tension: 0.15, pointRadius: 0, pointHoverRadius: 4,
      borderWidth: 2, spanGaps: false
    };

    charts[canvasId] = new Chart(ctx, {
      type: 'line',
      data: {
        labels: ages,
        datasets: [
          { label: 'Scheduled', data: valuesScheduled, borderColor: CHART_COLORS.line,           ...shared },
          { label: 'Payable',   data: valuesPayable,   borderColor: CHART_COLORS.lineSecondary,
            borderDash: SECONDARY_DASH, ...shared }
        ]
      },
      options: o
    });
  }

  function netTaxRateChart(canvasId, { ages, values, valuesSecondary = null, subtitle = null,
                                       yMin = -0.50, yMax = 0.20 }) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    destroyExisting(canvasId);

    const dual = valuesSecondary != null;

    const o = makeDefaults();
    o.scales.y.min = yMin;
    o.scales.y.max = yMax;
    o.scales.y.ticks.callback = (v) => Fmt.percent(v);
    o.plugins.tooltip.callbacks = {
      title: (items) => `Age ${items[0].label}`,
      label: (item)  => {
        const prefix = dual ? `${item.dataset.label}: ` : '';
        return `${prefix}${Fmt.percent(item.parsed.y)}`;
      }
    };
    if (subtitle) {
      o.plugins.subtitle.display = true;
      o.plugins.subtitle.text = subtitle;
    }
    o.plugins.annotation = {
      annotations: {
        zero: {
          type: 'line', yMin: 0, yMax: 0,
          borderColor: 'rgba(0, 0, 0, 0.25)',
          borderWidth: 1, borderDash: [2, 4]
        }
      }
    };

    const primary = {
      label: dual ? 'Scheduled' : '',
      data: values,
      borderColor: CHART_COLORS.taxAccent,
      fill: false,
      tension: 0.15,
      pointRadius: 0,
      pointHoverRadius: 4,
      borderWidth: 2,
      segment: {
        borderColor: c => (c.p1.parsed.y >= 0 ? CHART_COLORS.taxAccent : CHART_COLORS.earnings)
      }
    };
    const datasets = [primary];
    if (dual) {
      datasets.push({
        label: 'Payable',
        data: valuesSecondary,
        borderColor: CHART_COLORS.taxAccentSecondary,
        fill: false,
        tension: 0.15,
        pointRadius: 0,
        pointHoverRadius: 4,
        borderWidth: 2,
        borderDash: SECONDARY_DASH,
        segment: {
          borderColor: c => (c.p1.parsed.y >= 0 ? CHART_COLORS.taxAccentSecondary : CHART_COLORS.earningsSecondary)
        }
      });
    }

    charts[canvasId] = new Chart(ctx, {
      type: 'line',
      data: { labels: ages, datasets },
      options: o
    });
  }

  // Cohort line chart. Pass `dataSecondary` to add the payable line alongside
  // scheduled. When secondary is present, two-color threshold encoding is
  // suppressed (the dashed vs solid contrast carries enough information).
  function cohortLineChart(canvasId, opts) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    destroyExisting(canvasId);

    const {
      labels, data, dataSecondary = null,
      yFormat = 'currency',
      yMin = null, yMax = null,
      twoColorThreshold = null,
      referenceY = null,
      referenceLabel = null,
      subtitle = null
    } = opts;

    const dual = dataSecondary != null;

    const o = makeDefaults();
    o.scales.y.ticks.callback = (v) => formatYTick(v, yFormat);
    o.scales.y.beginAtZero = (yFormat === 'currency');
    if (yMin != null) o.scales.y.min = yMin;
    if (yMax != null) o.scales.y.max = yMax;
    o.plugins.tooltip.callbacks = {
      title: (items) => `Born ${items[0].label}`,
      label: (item)  => {
        const prefix = dual ? `${item.dataset.label}: ` : '';
        return `${prefix}${formatYTick(item.parsed.y, yFormat)}`;
      }
    };
    if (subtitle) {
      o.plugins.subtitle.display = true;
      o.plugins.subtitle.text = subtitle;
    }
    if (referenceY != null) {
      o.plugins.annotation = {
        annotations: {
          ref: {
            type: 'line',
            yMin: referenceY, yMax: referenceY,
            borderColor: 'rgba(0, 0, 0, 0.28)',
            borderWidth: 1,
            borderDash: [4, 4],
            label: referenceLabel ? {
              display: true,
              content: referenceLabel,
              position: 'end',
              backgroundColor: 'rgba(255, 255, 255, 0.9)',
              color: CHART_COLORS.axis,
              borderColor: 'rgba(0, 0, 0, 0.15)',
              borderWidth: 1,
              font: { family: 'Inter', size: 10 },
              padding: 3,
              yAdjust: -10
            } : { display: false }
          }
        }
      };
    }

    // Threshold encoding flips line colour at twoColorThreshold and is applied
    // independently to each dataset when present, so both scheduled and
    // payable lines can highlight where the metric crosses the threshold.
    const useThreshold = twoColorThreshold != null;
    const primaryPointColors = useThreshold
      ? (data || []).map(v => (v != null && v < twoColorThreshold) ? CHART_COLORS.taxAccent : CHART_COLORS.line)
      : CHART_COLORS.line;

    const primary = {
      label: dual ? 'Scheduled' : '',
      data,
      borderColor: CHART_COLORS.line,
      backgroundColor: 'rgba(37, 99, 235, 0.06)',
      fill: false,
      tension: 0.3,
      pointRadius: 3.5,
      pointHoverRadius: 6,
      pointBackgroundColor: primaryPointColors,
      pointBorderColor: '#fff',
      pointBorderWidth: 1.5,
      borderWidth: 2.25
    };

    if (useThreshold) {
      primary.segment = {
        borderColor: c => (c.p1.parsed.y >= twoColorThreshold ? CHART_COLORS.line : CHART_COLORS.taxAccent)
      };
    }

    const datasets = [primary];
    if (dual) {
      const secondaryPointColors = useThreshold
        ? (dataSecondary || []).map(v => (v != null && v < twoColorThreshold) ? CHART_COLORS.taxAccentSecondary : CHART_COLORS.lineSecondary)
        : CHART_COLORS.lineSecondary;
      const secondary = {
        label: 'Payable',
        data: dataSecondary,
        borderColor: CHART_COLORS.lineSecondary,
        backgroundColor: 'transparent',
        fill: false,
        tension: 0.3,
        pointRadius: 3,
        pointHoverRadius: 5,
        pointBackgroundColor: secondaryPointColors,
        pointBorderColor: '#fff',
        pointBorderWidth: 1.5,
        borderWidth: 2,
        borderDash: SECONDARY_DASH
      };
      if (useThreshold) {
        secondary.segment = {
          borderColor: c => (c.p1.parsed.y >= twoColorThreshold ? CHART_COLORS.lineSecondary : CHART_COLORS.taxAccentSecondary)
        };
      }
      datasets.push(secondary);
    }

    charts[canvasId] = new Chart(ctx, {
      type: 'line',
      data: { labels, datasets },
      options: o
    });
  }

  function barChart(canvasId, { labels, data, yFormat = 'currency', twoColorThreshold = null, subtitle = null }) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    destroyExisting(canvasId);

    const o = makeDefaults();
    o.scales.y.ticks.callback = (v) => formatYTick(v, yFormat);
    o.plugins.tooltip.callbacks = {
      label: (item) => formatYTick(item.parsed.y, yFormat)
    };
    if (subtitle) {
      o.plugins.subtitle.display = true;
      o.plugins.subtitle.text = subtitle;
    }

    const colorFor = (v) => {
      if (twoColorThreshold == null || v == null) return CHART_COLORS.line;
      return v >= twoColorThreshold ? CHART_COLORS.line : CHART_COLORS.taxAccent;
    };

    charts[canvasId] = new Chart(ctx, {
      type: 'bar',
      data: {
        labels,
        datasets: [{
          data,
          backgroundColor: data.map(colorFor),
          borderRadius: 3
        }]
      },
      options: o
    });
  }

  function formatYTick(v, fmt) {
    if (v == null) return '';
    if (fmt === 'currency') return Fmt.currency(v, { compact: true });
    if (fmt === 'percent')  return Fmt.percent(v);
    if (fmt === 'number')   return v.toFixed(2);
    return String(v);
  }

  function destroyAll() {
    Object.values(charts).forEach(c => c.destroy());
    Object.keys(charts).forEach(k => delete charts[k]);
  }

  function destroyChart(canvasId) {
    destroyExisting(canvasId);
  }

  return { lineChart, lifetimeProfileChart, netTaxRateChart, marginalIrrChart, barChart, cohortLineChart, destroyAll, destroyChart };
})();
