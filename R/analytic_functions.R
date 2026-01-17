# =============================================================================
# ANALYTIC FUNCTIONS
# =============================================================================
#
# This file contains the core analytical functions for the ssmbar package.
#
# =============================================================================

rep_rates <- function(worker, assumptions) {

  #Error Prevention
  worker_cols_needed <- c("id","year","age","earnings","annual_ind")
  if(!all(worker_cols_needed %in% names(worker))){
      stop(paste("worker file must contain:", paste(worker_cols_needed, collapse = ", ")))
    }

  assumption_cols_needed <- c("year","gdp_pi","awi","real_df")
  if(!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions file must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  dataset <- worker %>% left_join(assumptions %>% select(year, gdp_pi, awi, real_df),
                                  by = "year") %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
           #Initial benefit (numerator in the replacement rate)
           init_ben = annual_ind[which(age == 65)],

           #Scalars
           wage_scalar = awi[which(age == 65)] / awi,
           price_scalar = gdp_pi[which(age == 65)] / gdp_pi,

           #Indexed Earnings
           wage_earnings = earnings * wage_scalar,
           real_earnings = earnings * price_scalar,

          #Discount factors normalized to age 21
          real_df_norm = real_df / real_df[which(age == 21)],

          #Present value of real earnings at age 21
          pv_real_earn = real_earnings / real_df_norm

           ) %>% filter(age <= 64) %>%
          summarise(
            #Initial benefit
            init_ben = first(init_ben),

            #PV of earnings at age 21 (in real dollars)
            pv_real_earnings = sum(pv_real_earn),

            #Constant real payment with same PV as career earnings
            real_annuity = pv_real_earnings / sum(1 / real_df_norm),

            #Replacement Rates -- All
            pv_rr = init_ben / real_annuity,
            real_all = init_ben / mean(real_earnings),
            wage_all = init_ben / mean(wage_earnings),

             #High-N Year Replacement Rates
            real_sorted = list(sort(real_earnings, decreasing = TRUE)),
            wage_sorted = list(sort(wage_earnings, decreasing = TRUE)),

            real_h35 = init_ben / mean(real_sorted[[1]][1:35]),
            wage_h35 = init_ben / mean(wage_sorted[[1]][1:35]),
            real_h10 = init_ben / mean(real_sorted[[1]][1:10]),
            wage_h10 = init_ben / mean(wage_sorted[[1]][1:10]),
            real_h5 = init_ben / mean(real_sorted[[1]][1:5]),
            wage_h5 = init_ben / mean(wage_sorted[[1]][1:5]),

            #Last-N Years Replacement Rates
            n_years = n(),

            real_l35 = init_ben / mean(real_earnings[(n_years - 34): n_years]),
            wage_l35 = init_ben / mean(wage_earnings[(n_years - 34): n_years]),
            real_l10 = init_ben / mean(real_earnings[(n_years - 9): n_years]),
            wage_l10 = init_ben / mean(wage_earnings[(n_years - 9): n_years]),
            real_l5 = init_ben / mean(real_earnings[(n_years - 4): n_years]),
            wage_l5 = init_ben / mean(wage_earnings[(n_years - 4): n_years])

           ) %>% select(id, pv_rr, real_all, wage_all,
                        real_h35, wage_h35, real_h10, wage_h10, real_h5, wage_h5,
                        real_l35, wage_l35, real_l10, wage_l10, real_l5, wage_l5) %>%
    pivot_longer(cols = !"id",
                 names_to = "type",
                 values_to = "rep_rate")

  return(dataset)


}

