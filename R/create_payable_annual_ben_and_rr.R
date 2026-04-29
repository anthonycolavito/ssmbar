library(tidyverse)
library(slider)
library(checkmate)

load("./data/tr2025.rda")

source("./R/general_helpers.R")
source("./R/generate_worker.R")
source("./R/earnings.R")
source("./R/eligibility.R")
source("./R/aime.R")
source("./R/pia.R")
source("./R/special_min_pia.R")
source("./R/worker_ben.R")
source("./R/spousal.R")
source("./R/calc_ben.R")
source("./R/calc_tax.R")
source("./R/payable_ben.R")

all_ben <- read.csv("./output/benefits_by_worker_age.csv")
irr <- read.csv("./output/initial_replacement_rates.csv")

pb_all_ben <- all_ben %>% left_join(tr2025 %>% select(year, payable), by="year") %>%
  mutate(
    nominal_ben_pb = nominal_ben * payable,
    real_ben_pb = real_ben * payable
  ) %>% select(-payable, -nominal_ben, -real_ben)

saveRDS(pb_all_ben,  "./output/pb_benefits_by_worker_age.rds")
write_csv(pb_all_ben, "./output/pb_benefits_by_worker_age.csv")

pb_rr <- irr %>% left_join(tr2025 %>% select(year, payable), by=c("year_at_65"="year")) %>%
  mutate(
    ben_at_65_pb = ben_at_65 * payable,
    real_ben_at_65_pb = real_ben_at_65 * payable, 
    rep_rate_awi_pb = ben_at_65_pb / awi_at_65,
    rep_rate_career_pb = real_ben_at_65_pb / real_career_avg_earn
  ) %>% select(-payable, -ben_at_65, -real_ben_at_65, -rep_rate_awi, -rep_rate_career)

saveRDS(pb_rr,  "./output/pb_initial_replacement_rates.rds")
write_csv(pb_rr, "./output/pb_initial_replacement_rates.csv")
