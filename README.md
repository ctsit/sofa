# Sequential Organ Failure Assessment (SOFA)
This project is capable of generating SOFA scores for pediatric or neonatal populations in picu, pcicu or nicu cohorts.


# Setup Instructions

### Environment
The dependency management system [renv](https://rstudio.github.io/renv/articles/renv.html) is used to ensure that specific package versions and their required dependencies are installed.
To restore the project library locally call `renv::restore()`. 

Addtionally, see [collaborating with renv](https://rstudio.github.io/renv/articles/collaborating.html) for instructions on how to collaborate with renv.


### Data
The data was acquired from the University of Florida Integrated Data Repository (IDR) and is not accessible for public use due to PHI restrictions. For developers that have been granted access to this data please contact [James Wynn](james.wynn@peds.ufl.edu), [Philip Chase](pbc@ufl.edu) or [Laurence James-Woodley](lawjames1@ufl.edu) for access to the `sofa_inputs_and_outputs` folder. Once access has been granted copy the contents of the input folder to [./input](./input).


# Developer Notes

Software developers who would like to make contributions to this repository should read the [Developer Notes.](developer_notes.md)

# Score Calculation

### nSOFA Score
See [nSOFA components and scoring](nsofa_components_and_scoring.pdf) for the scoring system.

### pSOFA Score
See [pSOFA Components and Scoring](psofa_components_and_scoring.pdf) for the scoring system.

### Vasoactive-Inotropic Score (VIS)

The Vasoactive-Inotropic Score (VIS) is calculated according to this equation.

```
VIS = dopamine dose (μg/kg/min) +
      dobutamine dose (μg/kg/min) +
      10 x milrinone dose (μg/kg/min) +
      10 x vasopressin dose (mU/kg/min) +
      100 x epinephrine dose (μg/kg/min) +
      100 x norepinephrine dose (μg/kg/min)
```
