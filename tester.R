setwd("C:/Users/AnthonyColavito/OneDrive - Committee for a Responsible Federal Budget/Documents/Other Projects/Benefit Calculator/ssmbar")


tr2025 <- read.csv("2025TR Assumptions.csv")
sef <- read.csv("scaled_earnings_factors.csv")

tr2025 <- prep_assumptions(tr2025)

med_worker <- earnings_generator(birth_yr = 1960, type = "custom", age_claim = 65, factors = sef, assumptions = tr2025, custom_avg_earnings = 50000)

med_worker <- earnings_generator(birth_yr = 1960, type = "medium", age_stop = 65, age_claim = 65, age_elig = 62, factors = sef, assumptions = tr2025)
low_worker <- earnings_generator(birth_yr = 1960, type = "low", age_stop = 65, age_claim = 65, age_elig = 62, factors = sef, assumptions = tr2025)

workers <- rbind(med_worker, low_worker)

workers <- retired_worker(workers, tr2025, debugg = TRUE)

first_yr <- 1960 + 21 #First earnings year
last_yr <- 1960 + 119 #Last possible year alive (used for benefit amounts)

years <- seq(first_yr, last_yr, 1)
ages <- seq(21,119,1)

worker <- data.frame(year = years, age = ages)

real <- worker$real_earn
avg <- sum(sort(real, decreasing = TRUE)[1:35]) / 35
scalar <- 50000 / avg


worker <- worker %>% left_join(sef %>% filter(worker == "raw") %>% select(age, factor),
                               by = "age") %>%
  mutate(
    pi_age65 = gdp_pi[which(age==65)],
    index = pi_age65 / gdp_pi,
    nom_earn = factor * awi,
    real_earn = nom_earn * index)

med_worker <- med_worker %>% mutate(
  test_earn = earnings * pi_age65 / gdp_pi
)

earn_vect <- med_worker$test_earn

avg_test <- sum(sort(earn_vect, decreasing = TRUE)[1:35]) / 35
