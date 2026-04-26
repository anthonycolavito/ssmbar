library(tidyverse)
library(slider)

load("./data/tr2025.rda")
load("./data/sef2025.rda")

source("./R/earnings.R")
source("./R/eligibility.R")
source("./R/aime.R")
source("./R/general_helpers.R")
source("./R/pia.R")
source("./R/special_min_pia.R")
source("./R/worker_ben.R")

test <- generate_single_worker(sef2025, tr2025, birth_yr = 1960, type = "high") %>%
  mutate(dis_age = NA, yr_62 = year[which(age == 62)], claim_age = 64)

test <- join_all_assumptions(test, tr2025)

test_elig <- eligibility(test, debugg = TRUE)

test_aime <- aime(test_elig, debugg = TRUE)

test_pia <- test_aime %>% basic_pia(debugg = TRUE) %>% cola(debugg=TRUE)

test_smp <- test_pia %>% special_min_pia(debugg = TRUE)

test_wrk_ben <- test_smp %>% worker_benefit(debugg = TRUE)


test_di <- generate_single_worker(sef2025, tr2025, birth_yr = 1960, type = "high", career_length = 9) %>%
  mutate(dis_age = 30, claim_age = 30, birth_yr = 1960) %>%
  join_all_assumptions(tr2025)

test_di_elig <- test_di %>% eligibility(debugg = TRUE)

test_di_aime <- test_di_elig %>% aime(debugg = TRUE)

test_di_pia <- test_di_aime %>% basic_pia(debugg = TRUE) %>% cola(debugg=TRUE)

test_di_smp <- test_di_pia %>% special_min_pia(debugg = TRUE)

test_di_ben <- test_di_smp %>% worker_benefit(debugg = TRUE)
