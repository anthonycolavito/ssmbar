library(tidyverse)
library(slider)
library(checkmate)

load("./data/tr2025.rda")
load("./data/sef2025.rda")

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

test_worker <- generate_retired_worker(sef2025, tr2025, birth_yr = 1960, claim_age = 67, type = "very_low")
test_spouse <- generate_retired_worker(sef2025, tr2025, birth_yr = 1960, claim_age = 70, type = "high", spouse_id = "R-very_low-1960-44-67")




test_ben <- calc_ben(tr2025, test, output = "detailed")
