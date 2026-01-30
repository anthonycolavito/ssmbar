# App Design: Reform Comparison Integration

## Overview

This document outlines the design for integrating Social Security reform comparisons into the Benefit Explorer Shiny app.

## Current App Architecture

The app uses a modular structure with:
- **Sidebar**: Worker configuration (mod_worker_input.R)
- **Nav Panels**: Benefits, Replacement Rates, Lifetime Value, Ratios, Marginal
- **Data Flow**: worker_input_server() returns reactive data passed to all visualization modules
- **Comparison Infrastructure**: Already exists - modules handle `data$comparisons` for multiple scenarios

## Proposed Design

### 1. Add Reform Selection to Sidebar

Add a collapsible "Policy Reforms" section to the sidebar below the spouse configuration:

```
[ ] Enable Reform Comparison

Reform Options:
[✓] 5% Benefit Reduction
[ ] Raise NRA to 68
[ ] Index NRA to Longevity
[✓] Chained CPI
[ ] Cap COLAs at Median
...
```

**Implementation:**
- Add `checkboxInput("enable_reforms", "Enable Reform Comparison")`
- Add `checkboxGroupInput("selected_reforms", ...)` showing all available reforms
- Group reforms by category (Benefit Changes, NRA, COLA, Tax Max, Other)
- Show warnings when mutually exclusive reforms are selected

### 2. Update worker_input Module

Modify `worker_input_server()` to:

1. Calculate baseline benefits (current behavior)
2. When reforms enabled, also calculate benefits under each selected reform
3. Return data structure with:
   - `primary`: Baseline benefits (scenario = "Baseline")
   - `comparisons`: Reform benefits (scenario = reform name)
   - `assumptions`: Baseline assumptions
   - `reform_assumptions`: List of reformed assumptions by name

**New return structure:**
```r
list(
  primary = baseline_worker,      # scenario = "Baseline"
  comparisons = reform_workers,   # scenarios bound together
  assumptions = tr2025,
  comparison_summary = summary_df # From compare_reform_scenarios()
)
```

### 3. Add "Reform Summary" Nav Panel

New panel showing:

**Top Row: Key Metrics Comparison Table**
| Metric | Baseline | Reform 1 | Reform 2 | ... |
|--------|----------|----------|----------|-----|
| Monthly Benefit (Claim) | $3,729 | $3,542 | $3,708 | ... |
| PV Lifetime Benefits | $520,532 | $494,452 | $517,621 | ... |
| Benefit-Tax Ratio | 2.70 | 2.56 | 2.68 | ... |
| % Change from Baseline | - | -5.0% | -0.6% | ... |

**Middle Row: Comparison Charts**
- Side-by-side bar charts for key metrics
- Waterfall chart showing incremental impact of each reform

**Bottom Row: Detail Selection**
- Dropdown to select specific reform for detailed view
- Links to other tabs with that reform selected

### 4. Update Existing Visualization Modules

All existing modules already handle multiple scenarios via `data$comparisons`. Minor updates needed:

**mod_benefits.R**:
- Already works ✓
- Add reform name to legend when reforms enabled

**mod_replacement.R**:
- Update to show multiple replacement rate series
- Add comparison table

**mod_lifetime.R**:
- Add comparison bars for PV benefits/taxes
- Show percent difference

**mod_ratios.R**:
- Add comparison chart for benefit-tax ratios across scenarios

**mod_marginal.R**:
- Show marginal analysis for selected scenario only (too complex to overlay)
- Add scenario selector dropdown

### 5. Reform Configuration Constants

Add to `global.R`:

```r
# Available reforms for comparison
AVAILABLE_REFORMS <- list(
  "Benefit Changes" = list(
    "5% Benefit Cut" = function() reform_reduce_benefits(0.95, 2030),
    "10% Benefit Cut" = function() reform_reduce_benefits(0.90, 2030)
  ),
  "NRA Changes" = list(
    "Raise NRA to 68" = function() reform_nra_to_68(2030),
    "Index NRA to Longevity" = function() reform_index_nra(2030),
    "NRA to 69, then Index" = function() reform_nra_to_69_index(2030)
  ),
  "COLA Indexing" = list(
    "Chained CPI" = function() reform_chained_cpi(2030),
    "Cap COLAs at Median" = function() reform_cola_cap(2030),
    "CPI-E (Higher)" = function() reform_cpi_e(2030)
  ),
  "Tax Max" = list(
    "90% Coverage + 5% Credit" = function() reform_taxmax_90_pct(2030),
    "Eliminate + 15% Credit" = function() reform_eliminate_taxmax(2030),
    "Eliminate, No Credit" = function() reform_eliminate_taxmax_no_credit(2030)
  ),
  "Other" = list(
    "40-Year Averaging" = function() reform_40_year_averaging(2030),
    "Repeal RET" = function() reform_repeal_ret(2030),
    "Phase Out Spousal" = function() reform_phase_out_spousal(2030),
    "Basic Minimum Benefit" = function() reform_basic_minimum(900, 1342, 2030)
  )
)

# Mutual exclusivity groups for UI validation
REFORM_EXCLUSIVE_GROUPS <- list(
  "nra" = c("Raise NRA to 68", "Index NRA to Longevity", "NRA to 69, then Index"),
  "cola" = c("Chained CPI", "Cap COLAs at Median", "CPI-E (Higher)"),
  "taxmax" = c("90% Coverage + 5% Credit", "Eliminate + 15% Credit", "Eliminate, No Credit")
)
```

### 6. New Module: mod_reform_summary.R

```r
# Reform Summary Module UI
reform_summary_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(12, 6, 6),

    # Top: Comparison table
    card(
      card_header(class = "bg-primary text-white", "Reform Comparison Summary"),
      card_body(DTOutput(ns("summary_table")))
    ),

    # Bottom left: Metric comparison bar chart
    card(
      card_header("Key Metrics by Scenario"),
      card_body(
        selectInput(ns("metric_select"), NULL,
                    choices = c("Monthly Benefit", "PV Benefits", "Benefit-Tax Ratio")),
        plotOutput(ns("metric_chart"), height = "300px")
      )
    ),

    # Bottom right: Percent change chart
    card(
      card_header("Percent Change from Baseline"),
      card_body(plotOutput(ns("change_chart"), height = "300px"))
    )
  )
}
```

## Implementation Phases

### Phase 1: Backend Infrastructure (Completed)
- [x] Create reform templates for all 21 reforms
- [x] Implement mutual exclusivity checking
- [x] Create compare_reform_scenarios() function
- [x] Create calculate_all_measures() function
- [x] Test all reform combinations

### Phase 2: App Integration - Core
- [ ] Add reform selection UI to sidebar
- [ ] Update worker_input_server() to calculate reform scenarios
- [ ] Update data flow to include comparisons
- [ ] Test with existing modules

### Phase 3: App Integration - New Module
- [ ] Create mod_reform_summary.R
- [ ] Add "Reform Comparison" nav panel
- [ ] Connect to comparison data

### Phase 4: Polish & Testing
- [ ] Add mutual exclusivity warnings in UI
- [ ] Update all visualization modules for better comparison display
- [ ] Add tooltips explaining each reform
- [ ] User testing and refinement

## File Changes Summary

| File | Changes |
|------|---------|
| global.R | Add AVAILABLE_REFORMS, REFORM_EXCLUSIVE_GROUPS |
| app.R | Add nav_panel for Reform Summary |
| mod_worker_input.R | Add reform selection UI, calculate reform scenarios |
| mod_reform_summary.R | NEW - Reform comparison summary module |
| mod_benefits.R | Minor - improve multi-scenario legend |
| mod_replacement.R | Add comparison table |
| mod_lifetime.R | Add comparison bars |
| mod_ratios.R | Add comparison chart |
| mod_marginal.R | Add scenario selector |

## Performance Considerations

- Reform calculations are computationally similar to baseline
- With 4-5 reforms selected: ~5x calculation time
- Consider caching reformed assumptions
- Progressive loading: calculate reforms on demand

## User Experience

1. **Discovery**: Clear "Compare Reforms" option in sidebar
2. **Selection**: Grouped checkboxes with reform descriptions
3. **Feedback**: Warning when selecting mutually exclusive reforms
4. **Visualization**: All charts show overlay comparison
5. **Export**: Download comparison data as CSV

## Future Enhancements

- Custom reform parameters (e.g., adjust cut percentage)
- Reform effective year slider
- Cohort comparison (same reform, different birth years)
- Package of reforms (pre-defined combinations)
