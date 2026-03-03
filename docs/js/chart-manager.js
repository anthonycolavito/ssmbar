// =============================================================================
// ChartManager — Chart.js chart creation and updates (light theme)
// =============================================================================

const CHART_COLORS = {
  line: '#2563EB',
  fill: 'rgba(37, 99, 235, 0.08)',
  reform: '#F36107',
  muted: '#9ca3af',
  grid: 'rgba(0, 0, 0, 0.06)',
  nmtrAccrual: '#0D9488',
  nmtrTax: '#D97706'
};

const CHART_DEFAULTS = {
  responsive: true,
  maintainAspectRatio: false,
  animation: { duration: 350, easing: 'easeOutQuart' },
  plugins: {
    legend: {
      display: false,
      labels: { color: '#6b7280', font: { family: 'Inter', size: 11 }, boxWidth: 12 }
    },
    tooltip: {
      backgroundColor: '#0f1741',
      titleColor: '#e8e8e8',
      bodyColor: '#e8e8e8',
      borderColor: '#2a3f5f',
      borderWidth: 1,
      cornerRadius: 6,
      padding: 10,
      bodyFont: { family: 'Inter', size: 12 },
      titleFont: { family: 'Inter', size: 12, weight: 'bold' }
    }
  },
  scales: {
    x: {
      ticks: { color: '#6b7280', font: { family: 'Inter', size: 11 } },
      grid: { color: CHART_COLORS.grid }
    },
    y: {
      ticks: { color: '#6b7280', font: { family: 'Inter', size: 11 } },
      grid: { color: CHART_COLORS.grid }
    }
  }
};

class ChartManager {
  constructor() {
    this.charts = {};
  }

  destroy(id) {
    if (this.charts[id]) {
      this.charts[id].destroy();
      delete this.charts[id];
    }
  }

  lineChart(canvasId, { labels, datasets, yFormat, yLabel, xLabel, annotation }) {
    this.destroy(canvasId);
    const ctx = document.getElementById(canvasId);
    if (!ctx) return null;

    const config = {
      type: 'line',
      data: { labels, datasets },
      options: {
        ...CHART_DEFAULTS,
        interaction: { mode: 'index', intersect: false },
        plugins: {
          ...CHART_DEFAULTS.plugins,
          legend: {
            display: datasets.length > 1,
            labels: CHART_DEFAULTS.plugins.legend.labels
          },
          annotation: annotation ? { annotations: annotation } : undefined
        },
        scales: {
          x: {
            ...CHART_DEFAULTS.scales.x,
            title: xLabel ? { display: true, text: xLabel, color: '#6b7280', font: { family: 'Inter', size: 12 } } : undefined
          },
          y: {
            ...CHART_DEFAULTS.scales.y,
            title: yLabel ? { display: true, text: yLabel, color: '#6b7280', font: { family: 'Inter', size: 12 } } : undefined,
            ticks: {
              ...CHART_DEFAULTS.scales.y.ticks,
              callback: yFormat || undefined
            }
          }
        }
      }
    };

    this.charts[canvasId] = new Chart(ctx, config);
    return this.charts[canvasId];
  }

  barChart(canvasId, { labels, datasets, yFormat, yLabel, xLabel, annotation, yMin, yMax }) {
    this.destroy(canvasId);
    const ctx = document.getElementById(canvasId);
    if (!ctx) return null;

    const yScale = {
      ...CHART_DEFAULTS.scales.y,
      title: yLabel ? { display: true, text: yLabel, color: '#6b7280', font: { family: 'Inter', size: 12 } } : undefined,
      ticks: {
        ...CHART_DEFAULTS.scales.y.ticks,
        callback: yFormat || undefined
      }
    };
    if (yMin != null) yScale.min = yMin;
    if (yMax != null) yScale.max = yMax;

    const config = {
      type: 'bar',
      data: { labels, datasets },
      options: {
        ...CHART_DEFAULTS,
        interaction: { mode: 'index', intersect: false },
        plugins: {
          ...CHART_DEFAULTS.plugins,
          legend: {
            display: datasets.length > 1,
            labels: CHART_DEFAULTS.plugins.legend.labels
          },
          annotation: annotation ? { annotations: annotation } : undefined
        },
        scales: {
          x: {
            ...CHART_DEFAULTS.scales.x,
            title: xLabel ? { display: true, text: xLabel, color: '#6b7280', font: { family: 'Inter', size: 12 } } : undefined
          },
          y: yScale
        }
      }
    };

    this.charts[canvasId] = new Chart(ctx, config);
    return this.charts[canvasId];
  }

  // =========================================================================
  // Individual benefits-by-age chart (clips at death age)
  // =========================================================================

  renderBenefitsChart(baselineSeries, viewMode = 'real', deathAge = null, reformSeries = null) {
    if (!baselineSeries) return;

    const field = viewMode === 'real' ? 'real' : 'nominal';
    let ages = baselineSeries.ages;
    let baseData = baselineSeries[field];

    if (deathAge != null) {
      const maxAge = Math.floor(deathAge);
      const clipIdx = ages.findIndex(a => a > maxAge);
      if (clipIdx > 0) {
        ages = ages.slice(0, clipIdx);
        baseData = baseData.slice(0, clipIdx);
      }
    }

    const datasets = [{
      label: 'Current Law',
      data: baseData,
      borderColor: CHART_COLORS.line,
      backgroundColor: CHART_COLORS.fill,
      borderWidth: 2.5,
      pointRadius: 0,
      pointHoverRadius: 6,
      fill: true,
      tension: 0.2
    }];

    // Add reform overlay if present
    if (reformSeries) {
      let reformAges = reformSeries.ages;
      let reformData = reformSeries[field];

      // Clip reform data to same age range
      if (deathAge != null) {
        const maxAge = Math.floor(deathAge);
        const clipIdx = reformAges.findIndex(a => a > maxAge);
        if (clipIdx > 0) {
          reformAges = reformAges.slice(0, clipIdx);
          reformData = reformData.slice(0, clipIdx);
        }
      }

      // Align reform data to baseline ages
      const alignedReformData = ages.map(age => {
        const idx = reformAges.indexOf(age);
        return idx >= 0 ? reformData[idx] : null;
      });

      datasets.push({
        label: 'With Reform',
        data: alignedReformData,
        borderColor: CHART_COLORS.reform,
        backgroundColor: 'transparent',
        borderWidth: 2.5,
        borderDash: [6, 4],
        pointRadius: 0,
        pointHoverRadius: 6,
        fill: false,
        tension: 0.2
      });
    }

    const annotation = {};
    if (deathAge != null) {
      const lastAge = ages[ages.length - 1];
      annotation.deathAge = {
        type: 'line',
        xMin: lastAge,
        xMax: lastAge,
        borderColor: CHART_COLORS.muted,
        borderWidth: 1.5,
        borderDash: [6, 4],
        label: {
          display: true,
          content: `Expected lifespan: age ${deathAge.toFixed(0)}`,
          position: 'start',
          color: '#6b7280',
          font: { size: 11, family: 'Inter' },
          backgroundColor: 'rgba(250, 251, 252, 0.9)',
          padding: 4
        }
      };
    }

    this.lineChart('benefitsChart', {
      labels: ages,
      datasets,
      yFormat: (v) => Fmt.currency(v, { compact: true }),
      yLabel: `Annual Benefit (${viewMode === 'real' ? "Today's $" : 'Nominal $'})`,
      xLabel: 'Age',
      annotation: Object.keys(annotation).length > 0 ? annotation : undefined
    });
  }

  // =========================================================================
  // NMTR bar chart with dual colors
  // =========================================================================

  renderNMTRChart(nmtrSeries, reformNmtrSeries = null) {
    if (!nmtrSeries) return;

    const nmtrPct = nmtrSeries.nmtr.map(v => v != null ? v * 100 : null);

    const datasets = [{
      label: 'Current Law',
      data: nmtrPct,
      backgroundColor: (ctx) => {
        const val = ctx.dataset.data[ctx.dataIndex];
        return val != null && val < 0 ? '#0D948899' : '#D9770699';
      },
      borderColor: (ctx) => {
        const val = ctx.dataset.data[ctx.dataIndex];
        return val != null && val < 0 ? '#0D9488' : '#D97706';
      },
      borderWidth: 1
    }];

    // Add reform NMTR overlay as a line
    if (reformNmtrSeries) {
      const alignedReformPct = nmtrSeries.ages.map(age => {
        const idx = reformNmtrSeries.ages.indexOf(age);
        return idx >= 0 && reformNmtrSeries.nmtr[idx] != null
          ? reformNmtrSeries.nmtr[idx] * 100 : null;
      });

      datasets.push({
        label: 'With Reform',
        type: 'line',
        data: alignedReformPct,
        borderColor: CHART_COLORS.reform,
        backgroundColor: 'transparent',
        borderWidth: 2.5,
        borderDash: [6, 4],
        pointRadius: 0,
        pointHoverRadius: 5,
        fill: false,
        tension: 0.2,
        order: 0
      });
    }

    const annotation = {
      zeroLine: {
        type: 'line',
        yMin: 0,
        yMax: 0,
        borderColor: CHART_COLORS.muted,
        borderWidth: 1.5
      }
    };

    this.barChart('nmtrChart', {
      labels: nmtrSeries.ages,
      datasets,
      yFormat: (v) => v.toFixed(0) + '%',
      yLabel: 'Net Tax Rate (%)',
      xLabel: 'Age',
      annotation,
      yMin: -50,
      yMax: 20
    });
  }

  // =========================================================================
  // Cohort single hero chart
  // =========================================================================

  renderCohortHeroChart(series, metric, reformSeries = null) {
    if (!series) return;

    const configs = {
      ratio: {
        yFormat: (v) => v.toFixed(2),
        yLabel: 'Benefits per $1 of Taxes',
        annotation: {
          breakeven: {
            type: 'line', yMin: 1.0, yMax: 1.0,
            borderColor: CHART_COLORS.muted, borderWidth: 1.5, borderDash: [6, 4],
            label: {
              display: true, content: 'Break-even', position: 'end',
              color: '#6b7280', font: { size: 12, family: 'Inter' },
              backgroundColor: 'rgba(250, 251, 252, 0.9)', padding: 4
            }
          }
        }
      },
      initial_real_benefit: {
        yFormat: (v) => Fmt.currency(v),
        yLabel: 'Monthly Benefit (2025 $)'
      },
      repl_rate: {
        yFormat: (v) => (v * 100).toFixed(0) + '%',
        yLabel: '% of Earnings Replaced'
      },
      pv_benefits: {
        yFormat: (v) => Fmt.currency(v, { compact: true }),
        yLabel: 'Lifetime Benefits (2025 $)'
      },
      irr: {
        yFormat: (v) => (v * 100).toFixed(1) + '%',
        yLabel: 'Annual Return (%)',
        annotation: {
          zeroLine: {
            type: 'line', yMin: 0, yMax: 0,
            borderColor: CHART_COLORS.muted, borderWidth: 1.5, borderDash: [6, 4],
            label: {
              display: true, content: 'Zero return', position: 'end',
              color: '#6b7280', font: { size: 12, family: 'Inter' },
              backgroundColor: 'rgba(250, 251, 252, 0.9)', padding: 4
            }
          }
        }
      }
    };

    const cfg = configs[metric] || configs.ratio;

    const datasets = [{
      label: 'Current Law',
      data: series[metric],
      borderColor: CHART_COLORS.line,
      backgroundColor: 'transparent',
      borderWidth: 2.5,
      pointRadius: 4,
      pointHoverRadius: 7,
      pointBackgroundColor: CHART_COLORS.line,
      tension: 0.3
    }];

    // Add reform overlay if present
    if (reformSeries && reformSeries[metric]) {
      // Align reform data to baseline birth years; for birth years where
      // the reform was not yet in effect, use the baseline value so the
      // line spans the full chart (reform == baseline for those cohorts)
      const alignedData = series.birth_years.map((by, i) => {
        const idx = reformSeries.birth_years.indexOf(by);
        return idx >= 0 ? reformSeries[metric][idx] : series[metric][i];
      });

      datasets.push({
        label: 'With Reform',
        data: alignedData,
        borderColor: CHART_COLORS.reform,
        backgroundColor: 'transparent',
        borderWidth: 2.5,
        borderDash: [6, 4],
        pointRadius: 4,
        pointHoverRadius: 7,
        pointBackgroundColor: CHART_COLORS.reform,
        tension: 0.3
      });
    }

    this.lineChart('cohortHeroChart', {
      labels: series.birth_years,
      datasets,
      yFormat: cfg.yFormat,
      yLabel: cfg.yLabel,
      xLabel: 'Birth Year',
      annotation: cfg.annotation
    });
  }
}

// Global instance
const chartManager = new ChartManager();
