library(tidyverse)
files_to_rename <- tribble(
  ~source, ~target,
  "input/picu/PICU-child_encounter_input.csv", "input/picu/encounter.csv",
  "input/picu/PICU-child_flowsheets.csv", "input/picu/flowsheets.csv",
  "input/picu/PICU-child_labs.csv", "input/picu/labs.csv",
  "input/picu/PICU-child_medications.csv", "input/picu/medications.csv",
  "input/picu/Child_Glasgow_PICU.csv", "input/picu/glasgow.csv",

  "input/nicu/child_encounter_input.csv", "input/nicu/encounter.csv",
  "input/nicu/child_flowsheets.csv", "input/nicu/flowsheets.csv",
  "input/nicu/child_labs.csv", "input/nicu/labs.csv",
  "input/nicu/child_medications.csv", "input/nicu/medications.csv",

  "input/pcicu/PCICU-child_encounter_input.csv", "input/pcicu/encounter.csv",
  "input/pcicu/PCICU-child_flowsheets.csv", "input/pcicu/flowsheets.csv",
  "input/pcicu/PCICU-child_labs.csv", "input/pcicu/labs.csv",
  "input/pcicu/PCICU-child_medications.csv", "input/pcicu/medications.csv",
  "input/pcicu/Child_Glasgow_PICU.csv", "input/pcicu/glasgow.csv"
)

rename_these <- files_to_rename %>%
  filter(file.exists(source))

if(nrow(rename_these) > 0) {
  file.rename(rename_these$source, rename_these$target)
}
