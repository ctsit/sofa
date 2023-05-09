library(tidyverse)
library(janitor)
library(lubridate)
library(sofa)
library(here)

# identify cohort of interest. e.g picu/pcicu/nicu
cohort <- "nicu"

categorized_respiratory_devices <- readxl::read_excel(here(
    "input",
    "psofa_categorized_respiratory_devices.xlsx"
  ),
  sheet = 4) %>%
  filter(on_resp_support == 0)

read_child_encounter <- read_csv(here("input", cohort, "encounter.csv")) %>%
  clean_names() %>%
  filter(!is.na(dischg_datetime)) %>%
  filter(child_mrn_uf %in% nicu_term$child_mrn_uf)

child_encounter <- get_psofa_child_encounter(read_child_encounter)

transformed_child_encounter <- child_encounter$transformed_child_encounter

expanded_child_encounter <- child_encounter$expanded_child_encounter

child_dob <- read_child_encounter %>%
  distinct(child_mrn_uf, child_birth_date)

read_child_labs <- read_csv(here("input", cohort, "labs.csv")) %>%
  clean_names() %>%
  filter(child_mrn_uf %in% transformed_child_encounter$child_mrn_uf) %>%
  mutate_if(is.character, tolower)

if (cohort != 'nicu') {
read_glasgow <- read_csv(here("input", cohort, "glasgow.csv")) %>%
  clean_names()
}

read_flowsheets <- read_csv(here("input", cohort, "flowsheets.csv")) %>%
  clean_names() %>%
  filter(child_mrn_uf %in% transformed_child_encounter$child_mrn_uf) %>%
  mutate_if(is.character, tolower)

read_medications <- read_csv(here("input", cohort, "medications.csv")) %>%
  clean_names() %>%
  filter(child_mrn_uf %in% transformed_child_encounter$child_mrn_uf) %>%
  mutate_if(is.character, tolower)

coagulation <- get_psofa_coagulation(read_child_labs)

hepatic <- get_psofa_hepatic(read_child_labs)

if (cohort != 'nicu') {
neurologic <- get_psofa_neurologic(read_glasgow)
} else {
  neurologic <- tibble(child_mrn_uf = NA_integer_,
                       q1hr = NA_POSIXct_,
                       neurologic_score = NA_integer_)
}

renal <- get_psofa_renal(read_child_labs, child_dob)

cv_by_age_group <- get_psofa_cv_by_age_group(read_flowsheets, child_dob) %>%
  select(-c(child_birth_date, age_in_months))

cv_by_vasoactive_infusion <- get_psofa_cv_by_vasoactive_infusion(read_medications, expanded_child_encounter)

cardiovascular <- get_psofa_cardiovascular(cv_by_age_group, cv_by_vasoactive_infusion)

respiratory_support_status <- get_psofa_respiratory_status(read_flowsheets, categorized_respiratory_devices)

pao2_fio2_resp_score <- get_psofa_pao2_fio2_resp_score(read_child_labs, transformed_child_encounter, respiratory_support_status)

nasal_flow_rate_state <- get_psofa_nasal_state(read_flowsheets)

complete_fio2_value <- get_psofa_fio2_value(
  categorized_respiratory_devices,
  read_flowsheets,
  nasal_flow_rate_state
)

spo2_value <- get_psofa_spo2_value(read_flowsheets)

respiratory <- get_psofa_respiratory(
  expanded_child_encounter,
  pao2_fio2_resp_score,
  complete_fio2_value,
  spo2_value,
  respiratory_support_status,
  nasal_flow_rate_state
)

psofa_data <- list(
  expanded_child_encounter,
  coagulation,
  hepatic,
  neurologic,
  renal,
  cardiovascular,
  respiratory
) %>%
  reduce(left_join, by = c("child_mrn_uf", "q1hr")) %>%
  arrange(child_mrn_uf, q1hr) %>%
  group_by(child_mrn_uf, encounter) %>%
  fill(c(epinephrine,
         dopamine,
         norepinephrine,
         dobutamine,
         vasopressin,
         milrinone,
         ends_with("_score")),
       .direction = "down") %>%
  ungroup() %>%
  mutate_at(
    vars(
      "epinephrine",
      "dopamine",
      "norepinephrine",
      "dobutamine",
      "vasopressin",
      "milrinone",
      ends_with("_score")
    ),
    ~ replace(., is.na(.), 0)
  ) %>%
  mutate(
    psofa_score = coagulation_score + hepatic_score + neurologic_score + renal_score + cardiovascular_score + respiratory_score,
    vis_dopamine = dopamine,
    vis_dobutamine = dobutamine,
    vis_milrinone = 10 * milrinone,
    vis_vasopressin = 10 * vasopressin,
    vis_epinephrine = 100 * epinephrine,
    vis_norepinephrine = 100 * norepinephrine,
    vis_score = dopamine + dobutamine + 10 * milrinone + 10 * vasopressin + 100 * epinephrine + 100 * norepinephrine,
    vis_above_zero = if_else(vis_score > 0, 1, 0)
    ) %>%
  select(
    child_mrn_uf,
    encounter,
    child_birth_date,
    age_at_admission,
    admit_datetime,
    dischg_datetime,
    dischg_disposition,
    q1hr,
    dose_dopamine = dopamine,
    dose_dobutamine = dobutamine,
    dose_milrinone = milrinone,
    dose_vasopressin = vasopressin,
    dose_epinephrine = epinephrine,
    dose_norepinephrine = norepinephrine,
    on_respiratory_support,
    ends_with("_score"),
    starts_with("vis_"),
    -ends_with("_cardiovascular_score")
  )

vis_greater_than_zero_at_discharge <- psofa_data %>%
  group_by(child_mrn_uf, encounter) %>%
  slice_max(q1hr) %>%
  select(child_mrn_uf, encounter, dischg_disposition, q1hr, dischg_datetime, starts_with("vis_"), vis_score) %>%
  mutate(last_vis_score_non_zero = if_else(vis_score > 0, "TRUE", NA_character_)) %>%
  ungroup() %>%
  filter(!is.na(last_vis_score_non_zero) & str_detect(dischg_disposition, "HOME"))

cv_greater_than_zero_at_discharge <- psofa_data %>%
  group_by(child_mrn_uf, encounter, age_at_admission) %>%
  filter(q1hr == max(q1hr)) %>%
  select(child_mrn_uf, encounter, dischg_disposition, q1hr, cardiovascular_score, age_at_admission) %>%
  mutate(last_cv_score_non_zero = if_else(cardiovascular_score > 0, "TRUE", NA_character_)) %>%
  ungroup() %>%
  filter(!is.na(last_cv_score_non_zero) & dischg_disposition %in% c("TO HOME", "TO HOMECARE"))

psofa_summary <- psofa_data %>%
  mutate(psofa_above_zero = if_else(psofa_score > 0, 1, 0)) %>%
  group_by(child_mrn_uf,
           encounter,
           child_birth_date,
           age_at_admission,
           admit_datetime,
           dischg_datetime,
           dischg_disposition
           ) %>%
  summarise(
    across(
      c(
        ends_with("_score"),
        starts_with("dose"),
        starts_with("vis_")
      ),
      list(max = max, sum = sum),
      .names = "{.col}_{fn}"
    ),
    num_hours_psofa_above_zero = sum(psofa_above_zero),
    total_hospitalization_time_in_hours = n()
  ) %>%
  mutate(
    total_time_in_encounter = round(
      num_hours_psofa_above_zero / total_hospitalization_time_in_hours,
      2
    )
  )

write_csv(psofa_data, here("output", "psofa", cohort, str_c(cohort, "_psofa_data_", today(), ".csv")))
write_csv(psofa_summary, here("output", "psofa", cohort, str_c(cohort, "_psofa_summary_", today(), ".csv")))
write_csv(vis_greater_than_zero_at_discharge, here("output", "psofa", cohort, str_c(cohort, "_vis_greater_than_zero_at_discharge_", today(), ".csv")))
write_csv(cv_greater_than_zero_at_discharge, here("output", "psofa", cohort, str_c(cohort, "_cv_greater_than_zero_at_discharge_", today(), ".csv")))
