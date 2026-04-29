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
    // Use index mode + intersect:false so hover tooltips fire when the cursor
    // is anywhere over the chart, not just exactly on a (possibly invisible)
    // data point. This is what makes the Individual tab line charts respond
    // to hover the same way the Cohort tab bar charts do.
    interaction: {
      mode: 'index',
      intersect: false
    },
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
      // chartjs-plugin-annotation interprets numeric xMin/xMax as category
      // indices when the x-axis is category — convert label → index.
      const leIdx = labels.indexOf(leMarker);
      if (leIdx >= 0) {
        o.plugins.annotation = {
          annotations: {
            le: {
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
            }
          }
        };
      }
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
    // chartjs-plugin-annotation treats numeric xMin/xMax on a category scale
    // as indices, not label values. Convert the age values to indices.
    const claimIdx = transitionIdx;            // index of age 65 in the combined array
    const leIdx    = ages.indexOf(leAge);
    o.plugins.annotation = {
      annotations: {
        claim: {
          type: 'line', xMin: claimIdx, xMax: claimIdx,
          borderColor: 'rgba(0, 0, 0, 0.25)',
          borderWidth: 1, borderDash: [2, 4],
          label: {
            display: true, content: 'Claim age 65', position: 'start',
            backgroundColor: 'rgba(0, 0, 0, 0.65)', color: '#fff',
            font: { family: 'Inter', size: 10 }, padding: 3
          }
        },
        ...(leIdx >= 0 ? {
          le: {
            type: 'line', xMin: leIdx, xMax: leIdx,
            borderColor: 'rgba(217, 119, 6, 0.6)',
            borderWidth: 1, borderDash: [3, 3],
            label: {
              display: true, content: `Life Expectancy ≈ ${leAge}`, position: 'end',
              backgroundColor: 'rgba(217, 119, 6, 0.9)', color: '#fff',
              font: { family: 'Inter', size: 10 }, padding: 3
            }
          }
        } : {})
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

  // Net Tax Rate by Age — two-color segments. Y-axis is bounded so the
  // model-artifact spike at age 30 (which can dive to ~-400%) doesn't
  // compress the rest of the chart into a flat line.
  function netTaxRateChart(canvasId, { ages, values, subtitle = null,
                                       yMin = -0.50, yMax = 0.20 }) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    destroyExisting(canvasId);

    const o = makeDefaults();
    o.scales.y.min = yMin;
    o.scales.y.max = yMax;
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

  // Cohort line chart — clean line with visible round points, optional
  // two-color encoding around a threshold, optional horizontal reference line.
  function cohortLineChart(canvasId, opts) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    destroyExisting(canvasId);

    const {
      labels, data,
      yFormat = 'currency',
      twoColorThreshold = null,
      referenceY = null,
      referenceLabel = null,
      subtitle = null
    } = opts;

    const o = makeDefaults();
    o.scales.y.ticks.callback = (v) => formatYTick(v, yFormat);
    o.scales.y.beginAtZero = (yFormat === 'currency');
    o.plugins.tooltip.callbacks = {
      title: (items) => `Born ${items[0].label}`,
      label: (item)  => formatYTick(item.parsed.y, yFormat)
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

    const pointColors = (twoColorThreshold == null)
      ? CHART_COLORS.line
      : (data || []).map(v => (v != null && v < twoColorThreshold) ? CHART_COLORS.taxAccent : CHART_COLORS.line);

    const dataset = {
      data,
      borderColor: CHART_COLORS.line,
      backgroundColor: 'rgba(37, 99, 235, 0.06)',
      fill: false,
      tension: 0.3,
      pointRadius: 3.5,
      pointHoverRadius: 6,
      pointBackgroundColor: pointColors,
      pointBorderColor: '#fff',
      pointBorderWidth: 1.5,
      borderWidth: 2.25
    };

    if (twoColorThreshold != null) {
      dataset.segment = {
        borderColor: c => (c.p1.parsed.y >= twoColorThreshold ? CHART_COLORS.line : CHART_COLORS.taxAccent)
      };
    }

    charts[canvasId] = new Chart(ctx, {
      type: 'line',
      data: { labels, datasets: [dataset] },
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

  return { lineChart, lifetimeProfileChart, netTaxRateChart, barChart, cohortLineChart, destroyAll };
})();
