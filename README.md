# Large Herbivores and Herbaceous Plant Diversity

This repository contains data and R scripts associated with the manuscript:  
**"Large herbivores are linked to higher herbaceous plant diversity and functional redundancy across spatial scales"** (Journal of Animal Ecology).

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

- `univariate_glmms.R`  
  Fit univariate generalized linear mixed models (GLMMs) for the main analysis.

- `sensitivity_multivariate_glmms.R`  
  Conduct sensitivity analyses with GLMMs that including multiple predictors and account for alternative hypotheses.

---

## Visualization


- `study_area_plot.R`  
  Visual components of **Figure 1** (assembled later in Inkscape).

- `viz_univariate_glmms.R`  
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

## Main dataset: 
The main dataset can be found here: data/processed_data/clean_data/waterberg_2024_main_dataset.csv
The most important colums are: 



| Column Name(s)                         | Description |
|------------------------------------|-------------|
| `plot_ID`, `site_ID`, `reserve`    | Unique identifiers (plot, site, reserve) |
| `plant_richness_*`                 | Species richness of all herbaceous plants at plot, site, and reserve levels |
| `forb_richness_*`                  | Forb richness at each spatial scale |
| `graminoid_richness_*`            | Graminoid richness at each scale |
| `woody_richness_*`                | Woody species richness at each scale |
| `functional_richness_*`           | Functional plant richness at each scale |
| `functional_redundancy_*`         | Functional redundancy at each scale |
| `max_species_body_mass`, `mean_species_body_mass`, `median_species_body_mass` | Body mass summaries of mammal species present in reserves |
| `cw_*body_mass`                   | Community-weighted mean, median, and max body mass (abundance-weighted) |
| `herbivore_species_richness`      | Number of large herbivore species in a reserve |
| `herbivore_biomass_kg_ha`         | Total biomass of herbivorous mammals (kg/ha) |
| `browser_biomass_ha`, `grazer_biomass_ha`, `mixed_feeder_biomass_ha` | Biomass of browsers, grazers, and mixed feeders in kg/ha|
| `area_ha`                         | Reserve area (hectares) |
| `elevation_*`, `mat_*`, `map_*`   | Elevation, mean annual temperature (MAT), and precipitation (MAP) at plot, site, and reserve scales |
| `n_trigger_events_day_*`          | Herbivore visitation rate (number of independent trigger events per day) |
| `cameraID`                        | Camera trap identifier |
| `bare_ground`, `rock_cover`       | Percent cover of bare soil and rock at plots |
| `date_time`                       | Date and time when plots were surveyed |
