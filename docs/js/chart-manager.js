// =============================================================================
// ChartManager — Chart.js chart creation and updates
// =============================================================================

const CHART_COLORS = {
  baseline: '#9ACDFF',
  reform: '#F36107',
  teal: '#0A81A8',
  red: '#EE3224',
  purple: '#2A368A',
  muted: '#a0a0a0',
  grid: '#2a3f5f',
  bg: '#16213e'
};

const CHART_DEFAULTS = {
  responsive: true,
  maintainAspectRatio: false,
  animation: { duration: 300 },
  plugins: {
    legend: {
      display: false,
      labels: { color: '#e8e8e8', font: { size: 11 }, boxWidth: 12 }
    },
    tooltip: {
      backgroundColor: '#0f1741',
      titleColor: '#e8e8e8',
      bodyColor: '#e8e8e8',
      borderColor: '#2a3f5f',
      borderWidth: 1,
      cornerRadius: 4,
      padding: 8,
      bodyFont: { size: 12 },
      titleFont: { size: 12, weight: 'bold' }
    }
  },
  scales: {
    x: {
      ticks: { color: '#a0a0a0', font: { size: 11 } },
      grid: { color: 'rgba(42, 63, 95, 0.3)' }
    },
    y: {
      ticks: { color: '#a0a0a0', font: { size: 11 } },
      grid: { color: 'rgba(42, 63, 95, 0.5)' }
    }
  }
};

class ChartManager {
  constructor() {
    this.charts = {};
  }

  /**
   * Destroy a chart if it exists
   */
  destroy(id) {
    if (this.charts[id]) {
      this.charts[id].destroy();
      delete this.charts[id];
    }
  }

  /**
   * Get or create a line chart
   */
  lineChart(canvasId, { labels, datasets, yFormat, yLabel, xLabel, annotation }) {
    this.destroy(canvasId);
    const ctx = document.getElementById(canvasId);
    if (!ctx) return null;

    const config = {
      type: 'line',
      data: { labels, datasets },
      options: {
        ...CHART_DEFAULTS,
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
            title: xLabel ? { display: true, text: xLabel, color: '#a0a0a0', font: { size: 12 } } : undefined
          },
          y: {
            ...CHART_DEFAULTS.scales.y,
            title: yLabel ? { display: true, text: yLabel, color: '#a0a0a0', font: { size: 12 } } : undefined,
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

  /**
   * Create a bar chart (for NMTR)
   */
  barChart(canvasId, { labels, datasets, yFormat, yLabel, xLabel, annotation }) {
    this.destroy(canvasId);
    const ctx = document.getElementById(canvasId);
    if (!ctx) return null;

    const config = {
      type: 'bar',
      data: { labels, datasets },
      options: {
        ...CHART_DEFAULTS,
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
            title: xLabel ? { display: true, text: xLabel, color: '#a0a0a0', font: { size: 12 } } : undefined
          },
          y: {
            ...CHART_DEFAULTS.scales.y,
            title: yLabel ? { display: true, text: yLabel, color: '#a0a0a0', font: { size: 12 } } : undefined,
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

  // =========================================================================
  // Convenience builders for specific chart types
  // =========================================================================

  /**
   * Render individual benefits-by-age chart
   */
  renderBenefitsChart(baselineSeries, reformSeries, reformLabel, viewMode = 'nominal') {
    if (!baselineSeries) return;

    const field = viewMode === 'real' ? 'real' : 'nominal';
    const datasets = [{
      label: 'Baseline',
      data: baselineSeries[field],
      borderColor: CHART_COLORS.baseline,
      backgroundColor: 'rgba(154, 205, 255, 0.1)',
      borderWidth: 2,
      pointRadius: 1,
      fill: true,
      tension: 0.1
    }];

    if (reformSeries) {
      datasets.push({
        label: reformLabel || 'Reform',
        data: reformSeries[field],
        borderColor: CHART_COLORS.reform,
        backgroundColor: 'rgba(243, 97, 7, 0.1)',
        borderWidth: 2,
        pointRadius: 1,
        fill: true,
        tension: 0.1
      });
    }

    this.lineChart('benefitsChart', {
      labels: baselineSeries.ages,
      datasets,
      yFormat: (v) => Fmt.currency(v, { compact: true }),
      yLabel: `Annual Benefit (${viewMode === 'real' ? 'Real' : 'Nominal'} $)`,
      xLabel: 'Age'
    });
  }

  /**
   * Render NMTR bar chart
   */
  renderNMTRChart(baselineMarginal, reformMarginal, reformLabel) {
    if (!baselineMarginal) return;

    const datasets = [{
      label: 'Baseline NMTR',
      data: baselineMarginal.nmtr.map(v => v != null ? v * 100 : null),
      backgroundColor: CHART_COLORS.teal + '99',
      borderColor: CHART_COLORS.teal,
      borderWidth: 1
    }];

    if (reformMarginal) {
      datasets.push({
        label: (reformLabel || 'Reform') + ' NMTR',
        data: reformMarginal.nmtr.map(v => v != null ? v * 100 : null),
        backgroundColor: CHART_COLORS.reform + '99',
        borderColor: CHART_COLORS.reform,
        borderWidth: 1
      });
    }

    this.barChart('nmtrChart', {
      labels: baselineMarginal.ages,
      datasets,
      yFormat: (v) => v.toFixed(0) + '%',
      yLabel: 'Net Marginal Tax Rate (%)',
      xLabel: 'Age',
      annotation: {
        zeroLine: {
          type: 'line',
          yMin: 0,
          yMax: 0,
          borderColor: CHART_COLORS.muted,
          borderWidth: 1,
          borderDash: [4, 4]
        }
      }
    });
  }

  /**
   * Render a cohort line chart (replacement rate, PV benefits, ratio, or IRR)
   */
  renderCohortChart(canvasId, { baselineData, reformData, reformLabel, field, yFormat, yLabel, annotation }) {
    if (!baselineData) return;

    const datasets = [{
      label: 'Baseline',
      data: baselineData[field],
      borderColor: CHART_COLORS.baseline,
      backgroundColor: 'transparent',
      borderWidth: 2,
      pointRadius: 2,
      pointHoverRadius: 5,
      tension: 0.2
    }];

    if (reformData) {
      datasets.push({
        label: reformLabel || 'Reform',
        data: reformData[field],
        borderColor: CHART_COLORS.reform,
        backgroundColor: 'transparent',
        borderWidth: 2,
        pointRadius: 2,
        pointHoverRadius: 5,
        tension: 0.2
      });
    }

    this.lineChart(canvasId, {
      labels: baselineData.birth_years,
      datasets,
      yFormat,
      yLabel,
      xLabel: 'Birth Year',
      annotation
    });
  }

  /**
   * Render all 4 cohort charts
   */
  renderCohortCharts(baselineData, reformData, reformLabel, replRateField = 'repl_rate_pv') {
    // Replacement Rate
    this.renderCohortChart('cohReplChart', {
      baselineData, reformData, reformLabel,
      field: replRateField,
      yFormat: (v) => Fmt.percent(v, { isRatio: true }),
      yLabel: 'Replacement Rate (%)'
    });

    // PV Benefits
    this.renderCohortChart('cohPVChart', {
      baselineData, reformData, reformLabel,
      field: 'pv_benefits',
      yFormat: (v) => Fmt.currency(v, { compact: true }),
      yLabel: 'PV Lifetime Benefits ($)'
    });

    // Benefit-Tax Ratio
    this.renderCohortChart('cohRatioChart', {
      baselineData, reformData, reformLabel,
      field: 'ratio',
      yFormat: (v) => v.toFixed(2),
      yLabel: 'Benefit-Tax Ratio',
      annotation: {
        breakeven: {
          type: 'line',
          yMin: 1.0,
          yMax: 1.0,
          borderColor: CHART_COLORS.muted,
          borderWidth: 1,
          borderDash: [4, 4],
          label: {
            display: true,
            content: 'Break-even',
            position: 'end',
            color: CHART_COLORS.muted,
            font: { size: 10 }
          }
        }
      }
    });

    // IRR
    this.renderCohortChart('cohIRRChart', {
      baselineData, reformData, reformLabel,
      field: 'irr',
      yFormat: (v) => Fmt.percent(v, { isRatio: true }),
      yLabel: 'Internal Rate of Return (%)',
      annotation: {
        zeroLine: {
          type: 'line',
          yMin: 0,
          yMax: 0,
          borderColor: CHART_COLORS.muted,
          borderWidth: 1,
          borderDash: [4, 4]
        }
      }
    });
  }
}

// Global instance
const chartManager = new ChartManager();
