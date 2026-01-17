# =============================================================================
# ANALYTIC FUNCTIONS
# =============================================================================
#
# This file contains the core analytical functions for the ssmbar package.
#
# =============================================================================

rep_rates <- function(worker, assumptions) {

  #Error Prevention
  worker_cols_needed <- c("year","age","earnings","annual_ind")
  if(!worker_cols_needed %in% names(worker)){
      stop(paste("worker file must contain:", paste(worker_cols_needed, collapse = ", ")))
    }

  assumption_cols_needed <- c("year","gdp_pi","awi","df")
  if(!assumption_cols_needed %in% names(assumptions)) {
    stop(paste("assumptions file must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  dataset <- worker %>% left_join(assumptions %>% select(year, gdp_pi, awi),
                                  by = "year") %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
           #Initial benefit (numerator in the replacement rate)
           init_ben = annual_ind[which(age == 65)],

           #Scalar bases
           awi_age65 = awi[which(age == 65)],
           gdp_pi_age65 = gdp_pi[which(age == 65)],


           #Scalars
           wage_scalar = awi_age65 / awi,
           price_scalar = gdp_pi_age65 / gdp_pi,

           #Indexed Earnings
           wage_earnings = earnings * wage_scalar,
           real_earnings = earnings * price_scalar,

          #PV Payment Calcs
          real_df_base = real_df[which(age == 21)],
          real_df_wrk = real_df / real_df_base,
          pv_real_earn = real_earnings / real_df_wrk,
          real_pv_payment = sum(pv_real_earn) / real_df_wrk[which(age==64)]

           ) %>% ungroup()

  if (measure %in% c("real","wage")) {

  }

}


