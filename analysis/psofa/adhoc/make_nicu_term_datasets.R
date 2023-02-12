library(tidyverse)
library(here)
library(readxl)
library(lubridate)

#----Complete these steps first----#
# 1. Run make_psofa_dataset.R nicu cohort to create nicu_psofa_data and nicu_psofa_summary files.
# 2. Change the input file dates to match your local file dates

cohort <- "nicu"

nicu_term_mrns <- read_excel(here("input", cohort, "NICU-term MRNs for pSOFA.xlsx"))

nicu_psofa_data <- read_csv(here("output", "psofa", cohort, "nicu_psofa_data_2023-02-12.csv"))
nicu_psofa_summary <- read_csv(here("output", cohort, "nicu_psofa_summary_2023-02-12.csv"))

nicu_term_psofa_data <- nicu_psofa_data %>%
  filter(child_mrn_uf %in% nicu_term_mrns$child_mrn_uf)

nicu_term_psofa_summary <- nicu_psofa_summary %>%
  filter(child_mrn_uf %in% nicu_term_mrns$child_mrn_uf)

write_csv(nicu_term_psofa_data, here("output", "psofa", cohort, str_c(cohort, "_term_psofa_data_", today(), ".csv")))
write_csv(nicu_term_psofa_summary, here("output", cohort, str_c(cohort, "_term_psofa_summary_", today(), ".csv")))


nicu_psofa_data <- read_csv(here("output", "psofa", cohort, "nicu_psofa_data_2023-02-12.csv"))
