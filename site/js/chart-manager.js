// =============================================================================
// ChartManager — Chart.js helpers (light theme, current-law only)
// =============================================================================

const CHART_COLORS = {
  line:   '#2563EB',
  fill:   'rgba(37, 99, 235, 0.08)',
  muted:  '#9ca3af',
  grid:   'rgba(0, 0, 0, 0.06)',
  axis:   '#6b7280'
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
      }
    },
    scales: {
      x: {
        ticks: { color: CHART_COLORS.axis, font: { family: 'Inter', size: 11 } },
        grid:  { color: CHART_COLORS.grid }
      },
      y: {
        ticks: { color: CHART_COLORS.axis, font: { family: 'Inter', size: 11 } },
        grid:  { color: CHART_COLORS.grid }
      }
    }
  };
}

const chartManager = (() => {
  const charts = {};

  function lineChart(canvasId, { labels, data, yFormat = 'currency', yMin = null, yMax = null }) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    if (charts[canvasId]) charts[canvasId].destroy();

    const opts = makeDefaults();
    if (yMin != null) opts.scales.y.min = yMin;
    if (yMax != null) opts.scales.y.max = yMax;
    opts.scales.y.ticks.callback = (v) => formatYTick(v, yFormat);
    opts.plugins.tooltip.callbacks = {
      label: (item) => formatYTick(item.parsed.y, yFormat)
    };

    charts[canvasId] = new Chart(ctx, {
      type: 'line',
      data: {
        labels,
        datasets: [{
          data,
          borderColor: CHART_COLORS.line,
          backgroundColor: CHART_COLORS.fill,
          fill: true,
          tension: 0.25,
          pointRadius: 0,
          pointHoverRadius: 4,
          borderWidth: 2,
          spanGaps: true
        }]
      },
      options: opts
    });
  }

  function barChart(canvasId, { labels, data, yFormat = 'currency' }) {
    const ctx = document.getElementById(canvasId);
    if (!ctx) return;
    if (charts[canvasId]) charts[canvasId].destroy();

    const opts = makeDefaults();
    opts.scales.y.ticks.callback = (v) => formatYTick(v, yFormat);
    opts.plugins.tooltip.callbacks = {
      label: (item) => formatYTick(item.parsed.y, yFormat)
    };

    charts[canvasId] = new Chart(ctx, {
      type: 'bar',
      data: {
        labels,
        datasets: [{
          data,
          backgroundColor: CHART_COLORS.line,
          borderRadius: 2
        }]
      },
      options: opts
    });
  }

  function formatYTick(v, fmt) {
    if (v == null) return '';
    if (fmt === 'currency') return Fmt.currency(v, { compact: true });
    if (fmt === 'percent')  return Fmt.percent(v);
    if (fmt === 'number')   return String(v);
    return String(v);
  }

  function destroyAll() {
    Object.values(charts).forEach(c => c.destroy());
    Object.keys(charts).forEach(k => delete charts[k]);
  }

  return { lineChart, barChart, destroyAll };
})();
