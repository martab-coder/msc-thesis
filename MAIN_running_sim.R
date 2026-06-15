library(DeclareDesign)
library(randomizr)
library(dplyr)
library(tidyr)
library(purrr)
library(furrr)
library(progressr)
library(tibble)
library(ranger)
library(grf)
library(mice)
library(readr)
library(fixest)
library(broom)
library(ggplot2)
library(modelsummary)
library(here)
library(future.apply)
library(progressr)
library(marginaleffects)
library(dplyr)
library(fixest)
library(ggplot2)
library(car)
library(broom)
library(knitr)
library(kableExtra)
# ============================================================
# main
# ============================================================

source(here("code","01_Setup.R"))
source(here("code","02_MIDA_M_I.R"))
source(here("code","03_MIDA_D.R"))
source(here("code","04_MIDA_A.R"))
source(here("code","05_Simulation.R"))

# M: fixed population — created once, never touched again
fixed_population <- generate_synthetic_crm_population(
  N_POP                  = N_POP,
  target_conversion_rate = TARGET_CONVERSION_RATE,
  seed                   = SEED_POPULATION
)
cat("mean(tau)=", mean(fixed_population$tau), 
    " sd(tau)=", sd(fixed_population$tau), 
    " max(tau)=", max(fixed_population$tau), "\n")

# run 
with_progress(suppressWarnings(simulation_results <- run_simulation()))

saveRDS(simulation_results, file = "simulation_results_final.rds")
