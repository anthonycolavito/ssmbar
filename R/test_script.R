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
source("./R/calc_tax.R")

test_worker <- generate_retired_worker(sef2025, tr2025, birth_yr = 1960, claim_age = 62, type = "very_low",spouse_id = "R-high-1980-44-70")
test_spouse <- generate_retired_worker(sef2025, tr2025, birth_yr = 1980, claim_age = 70, type = "high")


test_ben <- calc_ben(tr2025, test_worker, test_spouse, output = "skinny")
test_ben <- calc_tax(tr2025, test_ben)

