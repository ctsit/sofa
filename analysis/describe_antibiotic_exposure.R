library(tidyverse)
library(janitor)
library(lubridate)
library(here)


# identify cohort of interest. e.g picu/pcicu/nicu
cohort <- "pcicu"

read_abxs <- read_csv(here("input", "abxs.csv"))

transformed_abxs <- read_abxs %>%
  group_by(drug_type) %>%
  mutate(drug_name = str_flatten(drug_name, collapse = "|")) %>%
  distinct()

read_child_encounter <- read_csv(here("input", cohort, "encounter.csv")) %>%
  clean_names() %>%
  filter(!is.na(dischg_datetime))

read_medications <- read_csv(here("input", cohort, "medications.csv")) %>%
  clean_names()

encounters <- read_child_encounter %>%
  select(child_mrn_uf, admit_datetime, dischg_datetime, dischg_disposition) %>%
  filter(!is.na(admit_datetime)) %>%
  group_by(child_mrn_uf) %>%
  mutate(encounter_number = row_number(),
         admit_datetime = as_date(admit_datetime),
         dischg_datetime = as_date(dischg_datetime)) %>%
  group_by(child_mrn_uf, encounter_number) %>%
  expand(encounter_date = seq(admit_datetime, dischg_datetime, by = "day"))

for (row in 1:nrow(transformed_abxs)) {
  abxs <- transformed_abxs %>%
    ungroup() %>%
    filter(row_number() == row)

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
        str_detect(med_order_desc, abxs$drug_name)
    ) %>%
    mutate(
      med_order_freq_desc_hour = case_when(
        str_detect(med_order_freq_desc, "^every") ~ parse_number(med_order_freq_desc),
        str_detect(med_order_freq_desc, "^daily$") ~ 24,
        TRUE ~ NA_real_
      ),
      short_med_order_desc = word(med_order_desc, 1, sep = " "),
      antibiotic = if_else(
        short_med_order_desc == 'penicillin',
        'penicillin g',
        short_med_order_desc
      )
    )

  shortest_frequency_for_a_drug <- filtered_medications %>%
    filter(!is.na(med_order_freq_desc_hour)) %>%
    group_by(antibiotic) %>%
    select(antibiotic, shortest_med_order_freq_desc_hour = med_order_freq_desc_hour) %>%
    slice_min(shortest_med_order_freq_desc_hour, with_ties = FALSE)

  abx_data <- filtered_medications %>%
    select(
      child_mrn_uf,
      antibiotic,
      med_order_desc,
      total_dose_character,
      med_order_datetime,
      take_datetime,
      starts_with("med_order_freq_desc")
    ) %>%
    arrange(child_mrn_uf, take_datetime) %>%
    left_join(shortest_frequency_for_a_drug, by = "antibiotic") %>%
    mutate(
      pk_tail_hour = coalesce(
        med_order_freq_desc_hour,
        shortest_med_order_freq_desc_hour
      ),
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
      pk_coverage_plus48hrs = pk_coverage + hours(48),
      time_diff = difftime(pk_coverage_plus48hrs, lag(pk_coverage_plus48hrs), units = "hours"),
      new_episode = case_when(
        time_diff >= 48 & lag(pk_coverage_plus48hrs) < take_datetime ~ 1,
        is.na(time_diff) ~ 1,
        TRUE ~ 0
      ),
      episode_number = cumsum(new_episode)
    ) %>%
    select(!c(matches("lag_"), time_diff, new_episode))

  abx_with_encounter_and_episode <- encounters %>%
    left_join(
      abx_data %>%
        mutate(take_date = as_date(take_datetime)),
      by = c("child_mrn_uf", "encounter_date" = "take_date")
    ) %>%
    ungroup() %>%
    filter(!is.na(antibiotic))

  # episode summaries -------------------------------------------------------


  total_abx_dose_per_episode <- abx_with_encounter_and_episode %>%
    group_by(child_mrn_uf, antibiotic, episode_number) %>%
    summarise(total_dose_sum = sum(total_dose_character))

  days_of_abx_exposure_per_episode <-
    abx_with_encounter_and_episode %>%
    select(child_mrn_uf, take_datetime, pk_coverage, episode_number) %>%
    mutate(take_datetime = as_date(take_datetime),
           pk_coverage = as_date(pk_coverage)) %>%
    distinct() %>%
    group_by(child_mrn_uf, episode_number) %>%
    summarise(
      first_date_method_1 = min(take_datetime),
      last_date_method_1 = max(take_datetime),
      first_date_method_2 = min(take_datetime),
      last_date_method_2 = max(pk_coverage)
    ) %>%
    mutate(
      abx_exposure_days_method_1 = (last_date_method_1 - first_date_method_1) + 1,
      abx_exposure_days_method_2 = (last_date_method_2 - first_date_method_2) + 1
    )

  time_between_take_and_med_order <-
    abx_with_encounter_and_episode %>%
    select(child_mrn_uf,
           antibiotic,
           episode_number,
           take_datetime,
           med_order_datetime) %>%
    distinct(child_mrn_uf, antibiotic, episode_number, .keep_all = TRUE) %>%
    group_by(child_mrn_uf, episode_number) %>%
    slice_min(take_datetime, with_ties = FALSE) %>%
    mutate(abx_1_turnaround_time_hour = as.numeric((round(
      difftime(take_datetime, med_order_datetime, units = "hours"),
      2
    )))) %>%
    select(-antibiotic)

  unordered_abx_per_episode <- abx_with_encounter_and_episode %>%
    select(child_mrn_uf, abx = antibiotic, episode_number) %>%
    distinct() %>%
    group_by(child_mrn_uf, episode_number) %>%
    mutate(antibiotic_count = row_number()) %>%
    pivot_wider(names_from = antibiotic_count,
                names_glue = "{.value}_{antibiotic_count}",
                values_from = abx) %>%
    arrange(child_mrn_uf, episode_number) %>%
    left_join(time_between_take_and_med_order,
              by = c("child_mrn_uf", "episode_number"))

  ordered_abx_per_episode <- abx_with_encounter_and_episode %>%
    select(child_mrn_uf, abx = antibiotic, episode_number) %>%
    distinct() %>%
    group_by(child_mrn_uf, episode_number) %>%
    arrange(abx) %>%
    mutate(antibiotic_count = row_number()) %>%
    pivot_wider(names_from = antibiotic_count,
                names_glue = "{.value}_{antibiotic_count}",
                values_from = abx) %>%
    arrange(child_mrn_uf, episode_number)

  abx_combinations <- ordered_abx_per_episode %>%
    ungroup() %>%
    select(starts_with("abx")) %>%
    rowwise() %>%
    mutate(abx_combination = paste(sort(c_across()), collapse = "_")) %>%
    ungroup() %>%
    count(abx_combination) %>%
    arrange(desc(n))

  abx_exposure_by_episode <- days_of_abx_exposure_per_episode %>%
    left_join(unordered_abx_per_episode,
              by = c("child_mrn_uf", "episode_number"))


  # encounter summaries --------------------------------------------------------

  total_abx_dose_per_encounter <- abx_with_encounter_and_episode %>%
    group_by(child_mrn_uf, antibiotic, encounter_number) %>%
    summarise(total_dose_sum = sum(total_dose_character))

  days_of_abx_exposure_per_encounter <-
    abx_with_encounter_and_episode %>%
    select(child_mrn_uf, take_datetime, pk_coverage, encounter_number) %>%
    mutate(take_datetime = as_date(take_datetime),
           pk_coverage = as_date(pk_coverage)) %>%
    distinct() %>%
    filter(!is.na(take_datetime)) %>%
    group_by(child_mrn_uf, encounter_number) %>%
    summarise(
      first_date_method_1 = min(take_datetime),
      last_date_method_1 = max(take_datetime),
      first_date_method_2 = min(take_datetime),
      last_date_method_2 = max(pk_coverage)
    ) %>%
    mutate(
      abx_exposure_days_method_1 = (last_date_method_1 - first_date_method_1) + 1,
      abx_exposure_days_method_2 = (last_date_method_2 - first_date_method_2) + 1
    )

  abx_per_encounter <- abx_with_encounter_and_episode %>%
    select(child_mrn_uf, abx = antibiotic, encounter_number) %>%
    distinct() %>%
    filter(!is.na(abx)) %>%
    group_by(child_mrn_uf, encounter_number) %>%
    mutate(antibiotic_count = row_number()) %>%
    pivot_wider(names_from = antibiotic_count,
                names_glue = "{.value}_{antibiotic_count}",
                values_from = abx) %>%
    arrange(child_mrn_uf, encounter_number)

  abx_exposure_by_encounter <- days_of_abx_exposure_per_encounter %>%
    left_join(abx_per_encounter, by = c("child_mrn_uf", "encounter_number"))


  # write outputs -----------------------------------------------------------


  output_files <- lst(
    abx_with_encounter_and_episode,
    abx_exposure_by_episode,
    total_abx_dose_per_episode,
    ordered_abx_per_episode,
    abx_combinations,
    abx_exposure_by_encounter,
    total_abx_dose_per_encounter
  )

  openxlsx::write.xlsx(
    output_files,
    paste0(
      "output/abx_exposure/",
      cohort,
      "_",
      abxs$drug_type,
      "_abx_exposure.xlsx"
    )
  )
}


