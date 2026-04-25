
spousal_pia <- function(worker, spouse = NULL, assumptions debugg = FALSE) {
  # The spousal insurance benefit is described in Section 320 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.03/handbook-0320.html
  #
  # Spousal benefits can begin at elig_age_retired (currently 62).
  # They cannot be claimed before the spouse whose record they are based on claims retired worker benefits.
  # Spousal PIA = (s_pia_share * spouse's PIA) - own PIA (if positive)
  
  # Check if we have spouse_spec in the worker data
  has_spouse_spec <- "spouse_spec" %in% names(worker) && any(!is.na(worker$spouse_spec))
  
  # If no spouse_data provided but workers have spouse_spec, generate it on-the-fly
  if (is.null(spouse_data) && has_spouse_spec) {
    if (is.null(factors)) {
      stop("factors parameter is required when spouse_data is NULL and workers have spouse_spec")
    }
    # Get unique spouse_specs (excluding NA)
    unique_specs <- unique(worker$spouse_spec[!is.na(worker$spouse_spec)])
    # Generate spouse data for each unique spec and cache it
    spouse_data <- lapply(unique_specs, function(spec) {
      generate_spouse(spec, factors, assumptions)
    })
    names(spouse_data) <- unique_specs
  }
  
  # Process each worker group based on their spouse_spec
  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("s_pia_share", "elig_age_retired")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]
  
  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>%
      left_join(assumptions %>% select(all_of(cols_to_join)), by = "year")
  } else {
    dataset <- worker
  }
  
  dataset <- dataset %>%
    group_by(id) %>%
    group_modify(~ {
      spec <- .x$spouse_spec[1]  # spouse_spec is constant within a worker
      elig_age_ret <- .x$elig_age_retired[1]
      
      if (is.na(spec) || is.null(spouse_data) || is.null(spouse_data[[spec]])) {
        # No spouse - set spouse_pia to 0
        .x$s_pia <- NA_real_
        .x$s_elig_age <- NA_real_
        .x$s_age <- NA_real_
        .x$spouse_pia <- 0
      } else {
        # Get spouse data from cache
        spouse_df <- spouse_data[[spec]]
        
        # Join spouse PIA and eligibility age by year
        # s_elig_age is needed to determine BC code (BD vs BR) in final_benefit()
        .x <- .x %>%
          left_join(spouse_df %>% select(year, s_age, s_claim_age, s_elig_age, s_pia), by = "year")
        
        # Calculate spousal PIA
        s_pia_share_ind <- .x$s_pia_share[which(.x$age == .x$elig_age[1])]
        yr_s_claim <- .x$year[which(.x$s_age == .x$s_claim_age)]
        
        .x$spouse_pia <- if_else(
          .x$age >= elig_age_ret & .x$year >= yr_s_claim,
          pmax((s_pia_share_ind * .x$s_pia) - pmax(.x$cola_basic_pia, 0, na.rm = TRUE), 0, na.rm = TRUE),
          0
        )
      }
      .x %>% select(-elig_age_retired)
    }) %>%
    ungroup()
  
  if (debugg) {
    cols_to_add <- c("s_pia", "s_elig_age", "s_age", "spouse_pia")
    cols_new <- cols_to_add[!cols_to_add %in% names(worker)]
    if (length(cols_new) > 0) {
      worker <- worker %>% left_join(dataset %>% select(id, year, all_of(cols_new)),
                                     by = c("id", "year"))
    }
  } else {
    # s_elig_age and s_age are needed by final_benefit() to determine BC code (BD vs BR)
    cols_to_add <- c("s_elig_age", "s_age", "spouse_pia")
    cols_new <- cols_to_add[!cols_to_add %in% names(worker)]
    if (length(cols_new) > 0) {
      worker <- worker %>% left_join(dataset %>% select(id, year, all_of(cols_new)),
                                     by = c("id", "year"))
    }
  }
  
  return(worker)
}