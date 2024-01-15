library(tidyverse)
library(here)
library(readxl)
library(lubridate)

cohort <- "nicu"

# change the input file dates to match your local file dates
nsofa_data <- read_csv(here("output", "nsofa", cohort, str_c(cohort, "_nsofa_data_2024-01-13.csv")))
psofa_data <- read_csv(here("output", "psofa", cohort, str_c(cohort, "_psofa_data_2024-01-13.csv"))) %>%
  select(-ends_with("_datetime"))

vis_scores <- psofa_data %>%
  select(child_mrn_uf,
         q1hr,
         ends_with("datetime"),
         starts_with("dose"),
         starts_with("vis"))

nsofa_with_vis <- nsofa_data %>%
  left_join(vis_scores, by = c("child_mrn_uf", "q1hr"))

nsofa_with_vis_summary <- nsofa_with_vis %>%
  mutate(nsofa_above_zero = if_else(nsofa_score > 0, 1, 0)) %>%
  group_by(child_mrn_uf,
           child_birth_date,
           admit_datetime,
           dischg_datetime,
           dischg_disposition
  ) %>%
  summarise(
    across(
      c(
        number_inotropic_drugs,
        oxygenation,
        platelets,
        steroids,
        cv,
        starts_with("dose"),
        ends_with("_score"),
        starts_with("vis_")
      ),
      list(max = max, sum = sum),
      .names = "{.col}_{fn}"
    ),
    num_hours_nsofa_above_zero = sum(nsofa_above_zero),
    total_hospitalization_time_in_hours = n()
  ) %>%
  mutate(
    total_time_in_encounter = round(
      num_hours_nsofa_above_zero / total_hospitalization_time_in_hours,
      2
    )
  )

write_csv(nsofa_with_vis, here("output", "nsofa", cohort, str_c(cohort, "_nsofa_with_vis_", today(), ".csv")))
write_csv(nsofa_with_vis_summary, here("output", "nsofa", cohort, str_c(cohort, "_nsofa_with_vis_summary_", today(), ".csv")))
