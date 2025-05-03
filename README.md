# Large Herbivores and Herbaceous Plant Diversity

This repository contains data and R scripts associated with the manuscript:  
**"Large herbivores are linked to higher herbaceous plant diversity and functional redundancy across spatial scales"** (in review).

The R code is organized into three primary components:

- **Data Cleaning and Preparation** (`R/data_cleaning_and_prep/`)
- **Analysis** (`R/analysis/`)
- **Visualization** (`R/viz/`)

---

## Data Cleaning and Preparation

- `clean_plot_and_camera_locations.R`  
  Clean GPS coordinates of vegetation plots and camera trap stations.

- `initial_cleaning_and_data_exploration.R`  
  Clean and explore plant community data collected in the field.

- `compile_mammal_traits.R`  
  Compile mammalian trait data using **PHYLACINE** and **HerbiTraits** databases.

- `reserve_metadata.R`  
  Construct herbivore community metrics and extract reserve-level covariates (e.g., MAP, MAT, elevation).

- `plot_metadata.R`  
  Extract covariates at the plot level.

- `extract_digikam_metadata.R`  
  Extract species identification data from tagged camera trap images processed via *EcoAssist* and *digiKam* (see methods in the manuscript for details).

- `get_plant_functional_diversity.R`  
  Calculate plant functional diversity indices.

- `combine_all.R`  
  Merge cleaned and derived data into a single dataset for analysis.

---

## Analysis

- `univariate_glmms_r1.R`  
  Fit univariate generalized linear mixed models (GLMMs) for the main analysis.

- `sensitivity_multivariate_glmms.R`  
  Conduct sensitivity analyses with GLMMs that including multiple predictors and account for alternative hypotheses.

- `sensitivity_manovas.R`  
  Perform MANOVAs as an additional robustness check.

---

## Visualization


- `main_study_area.R`  
  Visual components of **Figure 1** (assembled later in Inkscape).

- `main_univariate_glmm_viz_r1.R`  
  Visualization of model estimates for **Figures 2–3**.

- `supplement_correlations.R`  
  Correlation matrices of variables.

- `supplement_get_response_distribution.R`  
  Distributions of response variables.

- `supplement_multivariate_glmm_viz.R`  
  Estimates from GLMMs with multiple predictors (including testing alternative hypotheses).

- `supplement_vizualize_trait_distribution.R`  
  Trait distributions within dominant plant families.

- `supplement_double_check_forb_richness.R`  
  Investigate and visualize the effects of herbivore regime on forb richness when including multiple predictors.

