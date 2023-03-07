library(tidyverse)
library(janitor)
library(lubridate)
library(here)

# identify cohort of interest. e.g picu/pcicu/nicu
cohort <- "nicu"

read_abxs <- read_csv(here("input", "abxs.csv"))

abxs <- read_abxs %>%
  group_by(drug_type) %>%
  mutate(drug_name = str_flatten(drug_name, collapse = "|")) %>%
  distinct()

drug_name_types <- abxs %>%
  split(.$drug_type)

read_child_encounter <- read_csv(here("input", cohort, "encounter.csv")) %>%
  clean_names() %>%
  filter(!is.na(dischg_datetime))

read_medications <- read_csv(here("input", cohort, "medications.csv")) %>%
  clean_names()

filtered_medications <- read_medications %>%
  select(
    child_mrn_uf,
    med_order_desc,
    med_order_display_name,
    med_order_route,
    ends_with("_datetime"),
    ends_with("_date"),
    mar_action,
    total_dose_character,
    med_infusion_rate,
    med_order_freq_desc
  ) %>%
  mutate_if(is.character, tolower) %>%
  filter(
    child_mrn_uf %in% read_child_encounter$child_mrn_uf &
      str_detect(mar_action, "given") &
      str_detect(med_order_route, "intravenous") &
      (
        str_detect(med_order_desc, drug_name_types$antibacterial$drug_name) |
        str_detect(med_order_desc, drug_name_types$antifungal$drug_name) |
        str_detect(med_order_desc, drug_name_types$antiviral$drug_name)
      )
  ) %>%
  mutate(
    med_order_freq_desc_hour = case_when(
      str_detect(med_order_freq_desc, "^every") ~ parse_number(med_order_freq_desc),
      str_detect(med_order_freq_desc, "^daily$") ~ 24,
      TRUE ~ NA_real_
    ),
    short_med_order_desc = word(med_order_desc, 1, sep = " "),
    antibiotic = if_else(short_med_order_desc == 'penicillin', 'penicillin g', short_med_order_desc)
  )

shortest_frequency_for_a_drug <- filtered_medications %>%
  filter(!is.na(med_order_freq_desc_hour)) %>%
  group_by(antibiotic) %>%
  select(antibiotic, shortest_med_order_freq_desc_hour = med_order_freq_desc_hour) %>%
  slice_min(shortest_med_order_freq_desc_hour, with_ties = FALSE)

antibacterial_file <- filtered_medications %>%
  filter(str_detect(med_order_desc, drug_name_types$antibacterial$drug_name)) %>%
  select(
    child_mrn_uf,
    antibiotic,
    med_order_desc,
    take_datetime,
    starts_with("med_order_freq_desc")
  ) %>%
  arrange(child_mrn_uf, take_datetime) %>%
  left_join(shortest_frequency_for_a_drug, by = "antibiotic") %>%
  mutate(
    pk_tail_hour = coalesce(med_order_freq_desc_hour, shortest_med_order_freq_desc_hour),
    pk_due_time = take_datetime + hours(pk_tail_hour)
  ) %>%
  group_by(child_mrn_uf) %>%
  mutate(
    lag_pk_due_time = lag(pk_due_time),
    lag_2_pk_due_time = lag(pk_due_time, 2),
    pk_coverage = coalesce(
      pmax(lag_pk_due_time, lag_2_pk_due_time, pk_due_time, na.rm = TRUE),
      pk_due_time
    ),
    pk_coverage_plus48hrs = pk_coverage + hours(48)
  ) %>%
  select(!matches("lag_"))


