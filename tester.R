setwd("C:/Users/AnthonyColavito/OneDrive - Committee for a Responsible Federal Budget/Documents/Other Projects/Benefit Calculator/ssmbar")


tr2025 <- read.csv("2025TR Assumptions.csv")
sef <- read.csv("scaled_earnings_factors.csv")

tr2025 <- prep_assumptions(tr2025)

med_worker <- earnings_generator(birth_yr = 1960, type = "medium", age_claim = 65, factors = sef, assumptions = tr2025)

med_worker <- earnings_generator(birth_yr = 1960, type = "medium", age_stop = 65, age_claim = 65, age_elig = 62, factors = sef, assumptions = tr2025)
low_worker <- earnings_generator(birth_yr = 1960, type = "low", age_stop = 65, age_claim = 65, age_elig = 62, factors = sef, assumptions = tr2025)

workers <- rbind(med_worker, low_worker)

workers <- retired_worker(workers, tr2025, debugg = TRUE)
