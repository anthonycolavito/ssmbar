library(tidyverse)
library(slider)

load("./data/tr2025.rda")
load("./data/sef2025.rda")

source("./R/earnings.R")
source("./R/eligibility.R")
source("./R/aime.R")
source("./R/general_helpers.R")
source("./R/pia.R")

test <- generate_single_worker(sef2025, tr2025, birth_yr = 1960, type = "medium") %>%
  mutate(dis_age = NA, yr_62 = year[which(age == 62)])

test <- join_all_assumptions(test, tr2025)

test_elig <- eligibility(test, debugg = TRUE)

test_aime <- aime(test_elig, debugg = TRUE)

test_pia <- test_aime %>% basic_pia(debugg = TRUE) %>% cola(debugg=TRUE)
