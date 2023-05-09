# sofa 0.2.0 (released 2023-05-09)
## Major changes
- Add `describe_antibiotic_exposure.R` a script that outputs datasets documenting antibiotic exposures in picu/pcicu/nicu populations. It reads the same Epic Clarity datasets from UF Health as the nSOFA and pSOFA code. (#2, @ljwoodley)

# sofa 0.1.0 (released 2023-02-18)
## Major changes
- Refactor software CTS-IT created for Dr. James Wynn to compute neonatal sequential organ failure assessment (nSOFA) scores for a neonatal inpatient population from UF Health [nSOFA_calculation](https://github.com/ctsit/nSOFA_calculation) and Dr. Lara Nicolas to assess pediatric Sequential Organ Failure Assessment (pSOFA) in the pediatric inpatient population from UF Health [psofa](https://github.com/ctsit/psofa) (#1, @ljwoodley)
- Create a package to simplify the reuse and maintenance of the nSOFA and pSOFA code. (#1, @ljwoodley)
- Refactor the existing reports to use the package functions. (#1, @ljwoodley)
