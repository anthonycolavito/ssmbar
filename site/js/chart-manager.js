// =============================================================================
// ChartManager — Chart.js helpers (light theme, current-law only).
// Requires Chart.js v4 + chartjs-plugin-annotation v3 (loaded via CDN).
// =============================================================================

const CHART_COLORS = {
  line:      '#2563EB',
  fill:      'rgba(37, 99, 235, 0.05)',
  earnings:  '#0D9488',  // working-year segment / negative NMTR (--nmtr-accrual)
  benefits:  '#2563EB',  // retirement segment
  taxAccent: '#D97706',  // positive NMTR (--nmtr-tax)
  muted:     '#9ca3af',
  grid:      'rgba(0, 0, 0, 0.06)',
  axis:      '#6b7280'
};

function makeDefaults() {
  return {
    responsive: true,
    maintainAspectRatio: false,
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

  // Generic line chart used for cohort and the secondary annual-benefits chart.
  function lineChart(canvasId, opts) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    destroyExisting(canvasId);

    const {
      labels, data,
      yFormat = 'currency', yMin = null, yMax = null,
      subtitle = null, leMarker = null, fadeAfterIdx = null
    } = opts;

    const o = makeDefaults();
    if (yMin != null) o.scales.y.min = yMin;
    if (yMax != null) o.scales.y.max = yMax;
    o.scales.y.ticks.callback = (v) => formatYTick(v, yFormat);
    o.plugins.tooltip.callbacks = {
      title: (items) => `Age ${items[0].label}`,
      label: (item)  => formatYTick(item.parsed.y, yFormat)
    };
    if (subtitle) {
      o.plugins.subtitle.display = true;
      o.plugins.subtitle.text = subtitle;
    }
    if (leMarker != null) {
      o.plugins.annotation = {
        annotations: {
          le: {
            type: 'line',
            xMin: leMarker, xMax: leMarker,
            borderColor: 'rgba(217, 119, 6, 0.6)',
            borderWidth: 1,
            borderDash: [3, 3],
            label: {
              display: true,
              content: `LE ≈ ${leMarker}`,
              position: 'start',
              backgroundColor: 'rgba(217, 119, 6, 0.9)',
              color: '#fff',
              font: { family: 'Inter', size: 10 },
              padding: 3
            }
          }
        }
      };
    }

    const dataset = {
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
      dataset.segment = {
        borderColor: c => c.p1DataIndex > fadeAfterIdx ? 'rgba(37, 99, 235, 0.35)' : CHART_COLORS.line,
        borderDash:  c => c.p1DataIndex > fadeAfterIdx ? [4, 4] : undefined
      };
    }

    charts[canvasId] = new Chart(ctx, {
      type: 'line',
      data: { labels, datasets: [dataset] },
      options: o
    });
  }

  // Lifetime profile: combined earnings → benefits, two-color segments, no fill.
  // Linear y-axis. Vertical reference at age 65 and at LE.
  function lifetimeProfileChart(canvasId, { ages, values, transitionIdx, leAge, subtitle = null }) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    destroyExisting(canvasId);

    const o = makeDefaults();
    o.scales.y.ticks.callback = (v) => formatYTick(v, 'currency');
    o.plugins.tooltip.callbacks = {
      title: (items) => `Age ${items[0].label}`,
      label: (item)  => {
        const phase = item.dataIndex < transitionIdx ? 'Earnings' : 'Benefit';
        return `${phase}: ${formatYTick(item.parsed.y, 'currency')}`;
      }
    };
    if (subtitle) {
      o.plugins.subtitle.display = true;
      o.plugins.subtitle.text = subtitle;
    }
    o.plugins.annotation = {
      annotations: {
        claim: {
          type: 'line', xMin: 65, xMax: 65,
          borderColor: 'rgba(0, 0, 0, 0.25)',
          borderWidth: 1, borderDash: [2, 4],
          label: {
            display: true, content: 'Claim age 65', position: 'start',
            backgroundColor: 'rgba(0, 0, 0, 0.65)', color: '#fff',
            font: { family: 'Inter', size: 10 }, padding: 3
          }
        },
        le: {
          type: 'line', xMin: leAge, xMax: leAge,
          borderColor: 'rgba(217, 119, 6, 0.6)',
          borderWidth: 1, borderDash: [3, 3],
          label: {
            display: true, content: `LE ≈ ${leAge}`, position: 'end',
            backgroundColor: 'rgba(217, 119, 6, 0.9)', color: '#fff',
            font: { family: 'Inter', size: 10 }, padding: 3
          }
        }
      }
    };

    const dataset = {
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

    charts[canvasId] = new Chart(ctx, {
      type: 'line',
      data: { labels: ages, datasets: [dataset] },
      options: o
    });
  }

  // Net Tax Rate by Age — two-color segments + annotated age-30 spike.
  function netTaxRateChart(canvasId, { ages, values, subtitle = null }) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    destroyExisting(canvasId);

    const o = makeDefaults();
    o.scales.y.ticks.callback = (v) => Fmt.percent(v);
    o.plugins.tooltip.callbacks = {
      title: (items) => `Age ${items[0].label}`,
      label: (item)  => Fmt.percent(item.parsed.y)
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
        },
        spike: {
          type: 'label',
          xValue: 33, yValue: -1.5,
          content: [
            'Spike at age 30: model artifact.',
            'The full PV of accrued benefits is booked the moment',
            'the worker first becomes fully insured.',
            'Values from age 31 onward are the meaningful ones.'
          ],
          backgroundColor: 'rgba(255, 255, 255, 0.95)',
          borderColor: 'rgba(0, 0, 0, 0.15)',
          borderWidth: 1,
          font: { family: 'Inter', size: 10 },
          color: CHART_COLORS.axis,
          padding: 6,
          textAlign: 'left'
        }
      }
    };

    const dataset = {
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

    charts[canvasId] = new Chart(ctx, {
      type: 'line',
      data: { labels: ages, datasets: [dataset] },
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

  return { lineChart, lifetimeProfileChart, netTaxRateChart, barChart, destroyAll };
})();
