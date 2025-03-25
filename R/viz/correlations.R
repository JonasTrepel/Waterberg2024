library(data.table)
library(tidyverse)
library(MuMIn)
library(tidyr)
library(broom)
library(brms)
library(tidybayes)
library(GGally)
library(ggplot2)
library(ggcorrplot)

dt <- fread("data/processed_data/clean_data/waterberg_2024_main_dataset.csv") 


#### Explanatory variables 


dt_exp <- dt %>% dplyr::select("herbivore_biomass_kg_ha",
                                "herbivore_species_richness",
                              "n_trigger_events_day_reserve") %>% 
  rename(
    `Herbivore Biomass (kg/ha)` = herbivore_biomass_kg_ha, 
    `Herbivore Species Richness` = herbivore_species_richness, 
    `Herbivore Visitation` = n_trigger_events_day_reserve,
  )

p_exp <- ggpairs(dt_exp) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"))
p_exp

#### Plot scale responses

dt_vars_plot <- dt %>% dplyr::select(  ## Taxonomic diversity 
  "plant_richness_plot",
  "graminoid_richness_plot",
  "forb_richness_plot",
  "woody_richness_plot",
  
  ## Functional_diversity  
  "functional_redundancy_plot",
  "functional_diversity_plot",
  "functional_richness_plot",
  
  ## Structure
  "lidar_adjusted_mean_3d_plot", # mean distance travelled by a point, adjusted for the return fraction
  "lidar_adjusted_mean_3d_woody_plot", # mean distance travelled by a point above 90cm, adjusted for the return fraction
  "lidar_sd_adjusted_3d_partial_plot", # sd of mean distance travelled by a point, adjusted for the return fraction of 5 parts of the scan
  ) %>% 
  filter(complete.cases(.)) 

plot_corr <- round(cor(dt_vars_plot), 2)

p_plot_corr <- ggcorrplot(plot_corr, hc.order = TRUE, type = "lower",
           lab = TRUE) +
  labs(title = "Plot Scale") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

p_plot_corr

#### Site scale responses 

dt_vars_site <- dt %>% dplyr::select( ## Taxonomic diversity 
  "plant_richness_site",
  "graminoid_richness_site",
  "forb_richness_site",
  "woody_richness_site",
  
  ## Functional_diversity  
  "functional_redundancy_site",
  "functional_diversity_site",
  "functional_richness_site",
  
  ## Structure
  "lidar_adjusted_mean_3d_site", # mean distance travelled by a point, adjusted for the return fraction
  "lidar_adjusted_mean_3d_woody_site", # mean distance travelled by a point above 90cm, adjusted for the return fraction
  "lidar_sd_adjusted_mean_3d_site",) %>% 
  filter(complete.cases(.)) 

site_corr <- round(cor(dt_vars_site), 2)

p_site_corr <- ggcorrplot(site_corr, hc.order = TRUE, type = "lower",
                         lab = TRUE) +
  labs(title = "Site Scale") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

p_site_corr


#### Reserve scale responses 

dt_vars_reserve <- dt %>% dplyr::select( ## Taxonomic diversity 
  "plant_richness_reserve",
  "graminoid_richness_reserve",
  "forb_richness_reserve",
  "woody_richness_reserve",
  
  ## Functional_diversity  
  "functional_redundancy_reserve",
  "functional_diversity_reserve",
  "functional_richness_reserve",
  
  ## Structure
  "lidar_adjusted_mean_3d_reserve", # mean distance travelled by a point, adjusted for the return fraction
  "lidar_adjusted_mean_3d_woody_reserve", # mean distance travelled by a point above 90cm, adjusted for the return fraction
  "lidar_sd_adjusted_mean_3d_reserve",) %>% 
  filter(complete.cases(.)) 

site_corr <- round(cor(dt_vars_reserve), 2)

p_reserve_corr <- ggcorrplot(site_corr, hc.order = TRUE, type = "lower",
                          lab = TRUE) +
  labs(title = "Reserve Scale") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

p_reserve_corr

### correlation herbivore variables 


dt_herbi <- dt %>% 
  dplyr::select(
    herbivore_species_richness, mean_species_body_mass, cw_mean_species_body_mass,
    browser_biomass_ha, grazer_biomass_ha, 
    mixed_feeder_biomass_ha, herbivore_biomass_kg_ha,
  ) %>%
  rename(
    `Herbivore Species Richness` = herbivore_species_richness,
    `Mean Body Mass` = mean_species_body_mass,
    `Mean Body Mass (CWM)`= cw_mean_species_body_mass,
    `Browser Biomass` = browser_biomass_ha,
    `Grazer Biomass` = grazer_biomass_ha,
    `Mixed Feeder Biomass` = mixed_feeder_biomass_ha,
    `Total Herbivore Biomass` = herbivore_biomass_kg_ha
  ) %>% unique()

herbi_corr <- round(cor(dt_herbi), 2)

p_herbi_corr <- ggcorrplot(herbi_corr, hc.order = TRUE, type = "lower",
                            lab = TRUE) +
  labs(title = "Herbivore Variables") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

p_herbi_corr

##### Herbivores and other explanatories 
 
############ Site 
dt_alt_site <- dt %>%
  dplyr::select(
    elevation_site, 
    map_site, 
    mat_site, 
    area_ha, 
    herbivore_species_richness,
    n_trigger_events_day, 
    herbivore_biomass_kg_ha
  ) %>%
  filter(complete.cases(.)) %>%
  rename(
    `Elevation (m)` = elevation_site,
    `MAP (mm)` = map_site,
    `MAT (°C)` = mat_site,
    `Reserve Area (ha)` = area_ha,
    `Herbivore\nSpecies Richness` = herbivore_species_richness,
    `Herbivore Visitation` = n_trigger_events_day,
    `Herbivore Biomass\n(kg/ha)` = herbivore_biomass_kg_ha
  )

p_alt_site <- ggpairs(dt_alt_site) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"))

p_alt_site


alt_corr_site <- round(cor(dt_alt_site), 2)

p_alt_corr_site <- ggcorrplot(alt_corr_site, hc.order = TRUE, type = "lower",
                         lab = TRUE) +
  labs(title = "Site Scale") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

p_alt_corr_site

#### Environmental variables ---
library(ggridges)
p_ridges <- dt %>% 
  pivot_longer(cols = c("elevation_plot", "map_plot", "mat_plot"), 
               names_to = "var_name", values_to = "var_value") %>% 
  mutate(var_name = case_when(
    var_name == "elevation_plot" ~ "Elevation (m)", 
    var_name == "map_plot" ~ "MAP (mm)",
    var_name == "mat_plot" ~ "MAT (°C)"
  )) %>% 
  ggplot() +
  geom_density_ridges(aes(x = var_value, y = fct_rev(reserve), fill = reserve), alpha = 0.9) +
  facet_wrap(~var_name, scales = "free_x", ncol = 4) +
  scale_fill_manual(values = c("Ant's Farm" = "#33A02C",
                               "Dabchick" = "#B2DF8A",
                               "Jembisa" = "#FF7F00",
                               "Kaingo" = "#CAB2D6",
                               "Lapalala" = "#6A3D9A",
                               "Marakele" = "#FB9A99" ,
                               "Summerplace" = "#A6CEE3",
                               "Swebeswebe" = "#1F78B4",
                               "Syringa Sands" = "#FDBF6F",
                               "Willowisp" = "#E31A1C")) +
  labs(x = "", y = "") +
  theme_bw() + 
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 10), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12, face = "bold"), 
        strip.background = element_rect(color = "grey85"),
  ) 

p_ridges

#save 
ggsave(plot = p_exp, "builds/plots/supplement/corr_explanatories.png", dpi = 600, height = 6, width = 6)
ggsave(plot = p_plot_corr, "builds/plots/supplement/corr_plot_vars.png", dpi = 600, height = 10, width = 10)
ggsave(plot = p_site_corr, "builds/plots/supplement/corr_plot_vars.png", dpi = 600,  height = 10, width = 10)
ggsave(plot = p_reserve_corr, "builds/plots/supplement/corr_reserve_vars.png", dpi = 600,  height = 10, width = 10)
ggsave(plot = p_herbi_corr, "builds/plots/supplement/corr_herbi_vars.png", dpi = 600)
ggsave(plot= p_alt_site, "builds/plots/supplement/pairwise_corr_alternative_explanatories.png", height = 10, width = 10, dpi = 600)
ggsave(plot= p_ridges, "builds/plots/supplement/environmental_variables_ridges.png", height = 3, width = 10, dpi = 600)

