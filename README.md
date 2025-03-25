This repository contains data and scripts for the manuscript **"Large herbivore regime shapes savanna plant diversity and functional redundancy across spatial scales"** submitted to *Journal of Ecology* (January 2025).

## R Script Order

### Data Cleaning and Preparation
1. **Clean GPS locations of plots and cameras**  
   `clean_plot_and_camera_locations.R`

2. **Clean and explore plant community data from field records**  
   `initial_cleaning_and_data_exploration.R`

3. **Compile mammal traits using Phylacine and HerbiTraits databases**  
   `compile_mammal_traits.R`

4. **Compile reserve species lists, calculate mammal community metrics, and extract reserve-level covariates**  
   (e.g., MAP, MAT, elevation)  
   `reserve_metadata.R`

5. **Extract plot-level covariates**  
   `plot_metadata.R`

6. **Extract metadata (species information) after tagging species in images using EcoAssist and Digikam**  
   `extract_digikam_metadata.R`

7. **Calculate structural metrics on the LiDAR data**  
   `get_lidar_results.R`

8. **Calculate diversity and resilience indices and combine all fragments into the main dataset**  
   `get_plant_functional_diversity_and_combine_all.R`

---

### Analysis
1. **Run GLMMs**  
   `univariate_GLMMs.R`

---

### Visualization
1. **Components of Figure 1** (combined in Inkscape later on)  
   `study_area.R`

2. **Figures 2–5**  
   `glmm_viz_BH.R`

3. **Supplementary Figures**  
   `correlations.R`

---
