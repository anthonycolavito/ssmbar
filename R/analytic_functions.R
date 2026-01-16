# =============================================================================
# ANALYTIC FUNCTIONS
# =============================================================================
#
# This file contains the core benefit calculation functions for the ssmbar package.
# Functions are organized in the order they are called in the benefit calculation
# pipeline (see calculate_benefits() in CL_benefit_calculator.R):
#
# =============================================================================

rep_rates <- function(worker, assumptions) {

  #Error Prevention
  worker_cols_needed <- c("year","age","earnings","annual_ind")
  if(!worker_cols_needed %in% names(worker)){
      stop(paste("worker file must contain:", paste(worker_cols_needed, collapse = ", ")))
    }
  }

  assumption_cols_needed <- c("year","gdp_pi","awi","df")
  if(!assumption_cols_needed %in% names(assumptions)) {
    stop(paste("assumptions file must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  dataset <- worker %>% left_join(assumptions %>% select(year, gdp_pi, awi, df),
                                  by = "year") %>%
    group_by(id) %>%
    mutate(
           #Initial benefit (numerator in the replacement rate)
           init_ben = annual_ind[which(age == 65)],

           #Scalar bases
           awi_age65 = awi[which(age == 65)],
           gdp_pi_age65 = awi[which(age == 65)],
           df_age65 = df[which(age == 65)],

           #Scalars
           wage_scalar = awi_age65 / awi,
           price_scalar = gdp_pi_age65 / gdp_pi,
           df_scalar = df_age65 / df,

           #Indexed Earnings
           wage_earnings = earnings * wage_scalar,
           real_earnings = earnings * price_scalar

           ) %>% ungroup() %>%
    group_by(id) %>% arrange(id, age) %>%
    group_modify(
      ~ {

      }
    )

  if (measure %in% c("real","wage")) {

    real_earnings <-

  }


}
