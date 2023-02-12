library(tidyverse)
files_to_rename <- tribble(
  ~source, ~target,
  "data/picu/PICU-child_encounter_data.csv", "data/picu/encounter.csv",
  "data/picu/PICU-child_flowsheets.csv", "data/picu/flowsheets.csv",
  "data/picu/PICU-child_labs.csv", "data/picu/labs.csv",
  "data/picu/PICU-child_medications.csv", "data/picu/medications.csv",
  "data/picu/Child_Glasgow_PICU.csv", "data/picu/glasgow.csv",

  "data/nicu/child_encounter_data.csv", "data/nicu/encounter.csv",
  "data/nicu/child_flowsheets.csv", "data/nicu/flowsheets.csv",
  "data/nicu/child_labs.csv", "data/nicu/labs.csv",
  "data/nicu/child_medications.csv", "data/nicu/medications.csv",

  "data/pcicu/PCICU-child_encounter_data.csv", "data/pcicu/encounter.csv",
  "data/pcicu/PCICU-child_flowsheets.csv", "data/pcicu/flowsheets.csv",
  "data/pcicu/PCICU-child_labs.csv", "data/pcicu/labs.csv",
  "data/pcicu/PCICU-child_medications.csv", "data/pcicu/medications.csv",
  "data/pcicu/Child_Glasgow_PICU.csv", "data/pcicu/glasgow.csv"
)

rename_these <- files_to_rename %>%
  filter(file.exists(source))

if(nrow(rename_these) > 0) {
  file.rename(rename_these$source, rename_these$target)
}
