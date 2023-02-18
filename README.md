# Sequential Organ Failure Assessment (SOFA)

# Purpose of Study

This project is capable of generating SOFA scores for pediatric or neonatal populations in picu, pcicu or nicu unit designations. 

The Neonatal Sequential Organ Failure Assessment (nSOFA) score predicts mortality risk among preterm septic neonates, however it has yet to be studied in the general neonatal PICU/PCICU population. The Pediatric Sequential Organ Failure Assessment (pSOFA) has shown similar promise to predict PICU sepsis mortality risk but has yet to be studied in the general PICU/PCICU population. No comparison of utility between the two scoring systems (nSOFA/pSOFA) has been performed in neonates. This study plans to establish a normal range for pSOFA and nSOFA scores across nicu, picu and pcicu unit designations in the setting of other disease states to allow for comparison of the two scoring systems.

The principal investigator for this project is James L Wynn. The co-investigators are Lara Nichols and Diomel de la Cruz.

# About The Data

A retrospective examination of EHR encounter data of all patients below 22 years of age on admission to the UF Health NICU, PICU and PCICU from 2012-01-01 to 2020-07-23 was used to calculate the SOFA scores and determine if trends can be identified based on demographic features or clinical outcomes.

Data collected includes all laboratory results (including autopsy reports, radiology/ECHO/ultrasound/MRI results, pathology, microbiology, and clinical specimen testing), daily weights, ins/outs, medications, progress notes, operative notes, discharge summaries, vital signs, cardiopulmonary support and all supportive care beginning at the start of the hospital encounter (e.g. emergency department visit) to death or discharge.

These data were used to calculate the SOFA scores at q1 hour granularity by unit designation (NICU, PICU, PCICU) based on clinical parameters including the need for mechanical ventilation, oxygen requirement, requirement for cardiovascular support in the form of vasoactive drugs, and the presence of thrombocytopenia, renal function, neurologic status, and then averaging the score for each patient across different time intervals.

The data was acquired from the University of Florida Integrated Data Repository (IDR) under the aegis of IRB #202001996 (PICU/PCICU units) and #201902780 (NICU unit).

# Score Calculation

### nSOFA Score
See [nSOFA components and scoring](nsofa_components_and_scoring.pdf) for the scoring system.

### pSOFA Score
See [pSOFA Components and Scoring](psofa_components_and_scoring.pdf) for the scoring system.

### Vasoactive-Inotropic Score (VIS)

The VVIS is calculated according to this equation.

```
VIS = dopamine dose (μg/kg/min) +
      dobutamine dose (μg/kg/min) +
      10 x milrinone dose (μg/kg/min) +
      10 x vasopressin dose (mU/kg/min) +
      100 x epinephrine dose (μg/kg/min) +
      100 x norepinephrine dose (μg/kg/min)
```

# Setup Instructions

### Environment
The dependency management system [renv](https://rstudio.github.io/renv/articles/renv.html) is used to ensure that specific package versions and their required dependencies are installed.
To restore the project library locally call `renv::restore()`. 

Addtionally, see [collaborating with renv](https://rstudio.github.io/renv/articles/collaborating.html) for instructions on how to collaborate with renv.

### Data
The data was acquired from the University of Florida Integrated Data Repository (IDR) and is not accessible for public use due to PHI restrictions. For developers that have been granted access to this data please contact [James Wynn](james.wynn@peds.ufl.edu), [Philip Chase](pbc@ufl.edu) or [Laurence James-Woodley](lawjames1@ufl.edu) for access to the `sofa_inputs_and_outputs` folder. Once access has been granted copy the contents of the input folder to [./input](./input) to allow the analytic tools to run without modification.

### Package Build
Build the `sofa` package included in this repo. either by running R CMD INSTALL --preclean --no-multiarch --with-keep.source . at the root of the repo or using RStudio's Build features.

# Developer Notes

Software developers who would like to make contributions to this repository should read the [Developer Notes.](developer_notes.md)
