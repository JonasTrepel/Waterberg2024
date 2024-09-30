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

dt <- fread("data/processedData/cleanData/waterberg2024DataPrelim.csv") %>% 
  rename(species_per_reserve = total_plant_species_richness_reserve, 
         species_per_site = total_plant_species_richness_site, 
         reserve_mean_beta_divq1 = mean_beta_divq1) %>% 
  mutate( grazer_mf_biomass_ha = grazer_biomass_ha + (mixed_feeder_biomass_ha/2),
          browser_mf_biomass_ha = browser_biomass_ha + (mixed_feeder_biomass_ha/2))



#### Explanatory variables 


dtExp <- dt %>% dplyr::select("MAP",
                                "herbi_biomass_ha",
                                "herbi_fun_ent",
                                "grazer_mf_biomass_ha",
                                "browser_mf_biomass_ha",
                                "meanBodyMassKgReserve", 
                              "nEventsDayReserve") %>% 
  rename(
    `Herbivore Biomass (kg/ha)` = herbi_biomass_ha, 
    `Herbivore Functional Groups` = herbi_fun_ent, 
    `Grazer Biomass (kg/ha)` = grazer_mf_biomass_ha,
    `Browser Biomass (kg/ha)` = browser_mf_biomass_ha, 
    `Mean Visitor Body Mass (kg)` = meanBodyMassKgReserve, 
    `Visiting Frequency` = nEventsDayReserve,
  )

p.exp <- ggpairs(dtExp)
p.exp

#### Plot scale responses

dtVarsPlot <- dt %>% dplyr::select(  "species_per_plot",
                                     "shannon_plot",
                                     
                                     ## Life form diversity 
                                     "graminoids_per_plot",
                                     "forbs_per_plot",
                                     
                                     ## Resilience 
                                     "plot_plant_fun_red",
                                     "plot_plant_fun_div_distq1",
                                     "plot_plant_evenness_pielou",
                                     
                                     ## Structure
                                     "plot_lidar_adjusted_mean_3d",
                                     "plot_lidar_point_fraction",
                                     "plot_lidar_sd_adjusted_3d_partial") %>% 
  filter(complete.cases(.)) %>% 
  rename("Plant Species Richness" = "species_per_plot", 
         "Plant Shannon Diversity" = "shannon_plot",
    ## Life form diversity 
   "Graminoid Richness" =  "graminoids_per_plot",
   "Forb Richness" =   "forbs_per_plot",
    
    ## Resilience 
   "Plant Functional Redundancy" = "plot_plant_fun_red",
   "Plant Functional Diversity" = "plot_plant_fun_div_distq1",
   "Plant evenness" = "plot_plant_evenness_pielou",
    
    ## Structure
   "LiDAR Mean Distance (adj.)" = "plot_lidar_adjusted_mean_3d",
   "LiDAR Point Return Fraction" = "plot_lidar_point_fraction",
   "LiDAR SD" = "plot_lidar_sd_adjusted_3d_partial"
  )

plotCorr <- round(cor(dtVarsPlot), 1)

p.plotCorr <- ggcorrplot(plotCorr, hc.order = TRUE, type = "lower",
           lab = TRUE) +
  labs(title = "Plot Scale") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

p.plotCorr

#### Site scale responses 

dtVarsSite <- dt %>% dplyr::select(  "species_per_site",
                                     "shannon_site",
                                     
                                     ## Life form diversity 
                                     "graminoids_per_site",
                                     "forbs_per_site",
                                     
                                     ## Resilience 
                                     "site_plant_fun_red",
                                     "site_plant_fun_div_distq1",
                                     "site_plant_evenness_pielou",
                                     
                                     ## Structure
                                     "site_adj_mean_3d",
                                     "site_mean_return_fraction",
                                     "site_sd_adj_mean_3d") %>% 
  filter(complete.cases(.)) %>% 
  rename("Plant Species Richness" = "species_per_site", 
         "Plant Shannon Diversity" = "shannon_site",
         ## Life form diversity 
         "Graminoid Richness" =  "graminoids_per_site",
         "Forb Richness" =   "forbs_per_site",
         
         ## Resilience 
         "Plant Functional Redundancy" = "site_plant_fun_red",
         "Plant Functional Diversity" = "site_plant_fun_div_distq1",
         "Plant evenness" = "site_plant_evenness_pielou",
         
         ## Structure
         "LiDAR Mean Distance (adj.)" = "site_adj_mean_3d",
         "LiDAR Point Return Fraction" = "site_mean_return_fraction",
         "LiDAR SD" = "site_sd_adj_mean_3d"
  )

siteCorr <- round(cor(dtVarsSite), 1)

p.siteCorr <- ggcorrplot(siteCorr, hc.order = TRUE, type = "lower",
                         lab = TRUE) +
  labs(title = "Site Scale") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

p.siteCorr


#### Reserve scale responses 

dtVarsReserve <- dt %>% dplyr::select(  "species_per_reserve",
                                     "shannon_reserve",
                                     
                                     ## Life form diversity 
                                     "graminoids_per_reserve",
                                     "forbs_per_reserve",
                                     
                                     ## Resilience 
                                     "reserve_plant_fun_red",
                                     "reserve_plant_fun_div_distq1",
                                     "reserve_plant_evenness_pielou",
                                     
                                     ## Structure
                                     "reserve_adj_mean_3d",
                                     "reserve_mean_return_fraction",
                                     "reserve_sd_adj_mean_3d") %>% 
  filter(complete.cases(.)) %>% 
  rename("Plant Species Richness" = "species_per_reserve", 
         "Plant Shannon Diversity" = "shannon_reserve",
         ## Life form diversity 
         "Graminoid Richness" =  "graminoids_per_reserve",
         "Forb Richness" =   "forbs_per_reserve",
         
         ## Resilience 
         "Plant Functional Redundancy" = "reserve_plant_fun_red",
         "Plant Functional Diversity" = "reserve_plant_fun_div_distq1",
         "Plant evenness" = "reserve_plant_evenness_pielou",
         
         ## Structure
         "LiDAR Mean Distance (adj.)" = "reserve_adj_mean_3d",
         "LiDAR Point Return Fraction" = "reserve_mean_return_fraction",
         "LiDAR SD" = "reserve_sd_adj_mean_3d"
  )

reserveCorr <- round(cor(dtVarsReserve), 1)

p.reserveCorr <- ggcorrplot(reserveCorr, hc.order = TRUE, type = "lower",
                         lab = TRUE) +
  labs(title = "Reserve Scale") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

p.reserveCorr

#save 
ggsave(plot = p.exp, "builds/plots/supplement/corrExplanatories.png", dpi = 600, height = 12, width = 12)
ggsave(plot = p.plotCorr, "builds/plots/supplement/corrPlotVars.png", dpi = 600)
ggsave(plot = p.siteCorr, "builds/plots/supplement/corrSiteVars.png", dpi = 600)
ggsave(plot = p.reserveCorr, "builds/plots/supplement/corrReserveVars.png", dpi = 600)
