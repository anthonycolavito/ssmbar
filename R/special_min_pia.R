
years_of_coverage <- function(worker, debugg=FALSE) {
  
  # Count years where earnings >= yoc_threshold
  # Only count years from age 21 through eligibility age - 1 (working years)
  dataset <- worker %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
      # Flag each year as a coverage year (1) or not (0)
      is_coverage_year = if_else(
        age >= 21 & age < elig_age & !is.na(yoc_threshold) & earnings >= yoc_threshold,
        1L,
        0L
      ),
      # Cumulative years of coverage through each age
      # Capped at 30 per 42 USC 415(a)(1)(C)(ii): special minimum PIA
      # maxes out at 30 years of coverage (30 - 10 = 20 increments)
      years_of_coverage = pmin(cumsum(is_coverage_year), 30L)
    ) %>%
    ungroup()
  
  if(debugg) {
    worker <- worker %>% left_join(
      dataset %>% select(id, age, years_of_coverage, is_coverage_year),
      by = c("id", "age")
    )
  } else {
    worker <- worker %>% left_join(
      dataset %>% select(id, age, years_of_coverage),
      by = c("id", "age")
    )
  }
  
  return(worker)
  
}