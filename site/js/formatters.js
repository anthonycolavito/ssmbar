// =============================================================================
// Formatters — Currency, percent, and number formatting utilities
// =============================================================================

const Fmt = {
  /**
   * Format a number as currency (e.g., "$1,234" or "$1,234K")
   */
  currency(value, { decimals = 0, compact = false, prefix = '$' } = {}) {
    if (value == null || isNaN(value)) return '--';
    let v = Number(value);
    let suffix = '';
    if (compact) {
      if (Math.abs(v) >= 1e6) { v /= 1e6; suffix = 'M'; decimals = decimals || 1; }
      else if (Math.abs(v) >= 1e3) { v /= 1e3; suffix = 'K'; decimals = decimals || 0; }
    }
    const formatted = v.toLocaleString('en-US', {
      minimumFractionDigits: decimals,
      maximumFractionDigits: decimals
    });
    return `${prefix}${formatted}${suffix}`;
  },

  /**
   * Format as percentage (e.g., "42.5%")
   * @param {number} value - Raw ratio (0.425) or already percentage (42.5) depending on isRatio
   * @param {boolean} isRatio - If true, multiplies by 100
   */
  percent(value, { decimals = 1, isRatio = true } = {}) {
    if (value == null || isNaN(value)) return '--';
    const pct = isRatio ? value * 100 : value;
    return pct.toFixed(decimals) + '%';
  },

  /**
   * Format a number with specified decimal places
   */
  number(value, { decimals = 2 } = {}) {
    if (value == null || isNaN(value)) return '--';
    return Number(value).toLocaleString('en-US', {
      minimumFractionDigits: decimals,
      maximumFractionDigits: decimals
    });
  },

  /**
   * Format a signed change (e.g., "+3.2%" or "-1.5%")
   */
  change(value, { decimals = 1, suffix = '%' } = {}) {
    if (value == null || isNaN(value)) return '--';
    const sign = value >= 0 ? '+' : '';
    return sign + value.toFixed(decimals) + suffix;
  },

  /**
   * Determine CSS class based on value direction
   */
  directionClass(value, { positiveIsGood = true } = {}) {
    if (value == null || isNaN(value)) return '';
    if (value > 0) return positiveIsGood ? 'positive' : 'negative';
    if (value < 0) return positiveIsGood ? 'negative' : 'positive';
    return '';
  }
};
