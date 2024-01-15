library(tidyverse)
library(janitor)
library(lubridate)
library(here)
library(readxl)
library(sofa)

cohort <- "nicu"

respiratory_devices <- read_excel(here(
  "input",
  "nsofa_categorized_respiratory_devices.xlsx"
)) %>%
  filter(intubated %in% c('yes', 'no'))

read_child_encounter <- read_csv(here("input", cohort, "encounter.csv")) %>%
  clean_names() %>%
  filter(!is.na(dischg_datetime))

# Filter to term neonates (<=30 days old at admission) for PICU/PCICU. Also, note that
# expand_child_encounter only returns the first encounter for nsofa calculation
if (cohort != 'nicu') {
read_child_encounter <- read_child_encounter %>%
  arrange(child_mrn_uf, admit_datetime) %>%
  filter(as_date(admit_datetime) - child_birth_date <= 30)
}

child_encounter <- get_nsofa_child_encounter(read_child_encounter)

read_child_labs <- read_csv(here("input", cohort, "labs.csv")) %>%
  clean_names()

read_medications <- read_csv(here("input", cohort, "medications.csv")) %>%
  clean_names()

read_flowsheets <- read_csv(here("input", cohort, "flowsheets.csv")) %>%
  clean_names()

platelets <- get_nsofa_platelets(read_child_labs, child_encounter)

steroids <- get_nsofa_steroids(read_medications, child_encounter)

inotropes <- get_nsofa_inotropes(read_medications, child_encounter)

oxygenation <- get_nsofa_oxygenation(read_flowsheets, respiratory_devices, child_encounter)

nsofa_data <- list(child_encounter,
                   platelets,
                   steroids,
                   inotropes,
                   oxygenation) %>%
  reduce(left_join, by = c("child_mrn_uf", "q1hr")) %>%
  group_by(child_mrn_uf) %>%
  fill(
    c(
      platelets,
      steroids,
      inotrope_score,
      number_inotropic_drugs,
      oxygenation
    ),
    .direction = "down"
  ) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>%
  mutate(
    cv = case_when(
      number_inotropic_drugs == 0 & steroids == 0 ~ 0,
      number_inotropic_drugs == 0 &
        steroids == 1 ~ 1,
      number_inotropic_drugs == 1 &
        steroids == 0 ~ 2,
      (number_inotropic_drugs >= 2 &
         steroids == 0) |
        (number_inotropic_drugs == 1 &
           steroids == 1) ~ 3,
      number_inotropic_drugs >= 2 &
        steroids == 1 ~ 4
    )
  ) %>%
  mutate(nsofa_score = platelets + oxygenation + cv)

nsofa_summary <- nsofa_data %>%
  mutate(nsofa_above_zero = if_else(nsofa_score > 0, 1, 0)) %>%
  group_by(child_mrn_uf,
           admit_datetime,
           dischg_datetime,
           dischg_disposition) %>%
  summarise(
    across(
      c(platelets, oxygenation, cv, nsofa_score),
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

# max_score_within_24_hrs <- get_max_score_within_n_hours_of_admission(
#   min_hour = 1,
#   max_hour = 24
# )
#
# max_score_between_3_and_24_hrs <- get_max_score_within_n_hours_of_admission(
#   min_hour = 3,
#   max_hour = 24
# )
#
# max_score_within_28_days <- get_max_score_within_n_hours_of_admission(
#   min_hour = 1,
#   max_hour = 672
# )



write_csv(nsofa_data, here("output", "nsofa", cohort, str_c(cohort, "_nsofa_data_", today(), ".csv")))
write_csv(nsofa_summary, here("output", "nsofa", cohort, str_c(cohort, "_nsofa_summary_", today(), ".csv")))
