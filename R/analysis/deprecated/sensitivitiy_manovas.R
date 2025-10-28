#Try Manova 
library(data.table)
library(tidyverse)

dt <- fread("data/processed_data/clean_data/waterberg_2024_main_dataset.csv") 
names(dt)


### life form richness -------


shapiro.test(dt$forb_richness_site)
library(rstatix)
# Check outliers 
sum(mahalanobis_distance(data = dt[, c("graminoid_richness_plot",
                                       "forb_richness_plot",
                                       "woody_richness_plot", 
                                       "functional_richness_plot", 
                                       "functional_redundancy_plot")])$is.outlier, na.rm = T)
sum(mahalanobis_distance(data = dt[, c("graminoid_richness_site",
                                       "forb_richness_site", 
                                       "woody_richness_site", 
                                       "functional_richness_site", 
                                       "functional_redundancy_site")])$is.outlier)
sum(mahalanobis_distance(data = dt[, c("graminoid_richness_reserve",
                                       "forb_richness_reserve",
                                       "woody_richness_reserve", 
                                       "functional_richness_reserve", 
                                       "functional_redundancy_reserve")])$is.outlier)

### plot scale 

dt_plot <- dt %>% 
  dplyr::select("graminoid_richness_plot",
                "forb_richness_plot",
                "woody_richness_plot", 
                "functional_richness_plot", 
                "functional_redundancy_plot", 
                "herbivore_biomass_kg_ha",
                "n_trigger_events_day",
                "herbivore_species_richness") %>% 
  unique()


man_p1 <- manova(cbind(forb_richness_plot, graminoid_richness_plot, woody_richness_plot) ~ herbivore_biomass_kg_ha, data = dt_plot)
summary(man_p1)
summary.aov(man_p1)

man_p2 <- manova(cbind(forb_richness_plot, graminoid_richness_plot, woody_richness_plot) ~ n_trigger_events_day, data = dt_plot)
summary(man_p2)
summary.aov(man_p2)

man_p3 <- manova(cbind(forb_richness_plot, graminoid_richness_plot, woody_richness_plot) ~ herbivore_species_richness, data = dt_plot)
summary(man_p3)
summary.aov(man_p3)

man_p4 <- manova(cbind(functional_richness_plot, functional_redundancy_plot) ~ herbivore_biomass_kg_ha, data = dt_plot)
summary(man_p4)
summary.aov(man_p4)

man_p5 <- manova(cbind(functional_richness_plot, functional_redundancy_plot) ~ n_trigger_events_day, data = dt_plot)
summary(man_p5)
summary.aov(man_p5)

man_p6 <- manova(cbind(functional_richness_plot, functional_redundancy_plot) ~ herbivore_species_richness, data = dt_plot)
summary(man_p6)
summary.aov(man_p6)



dt_site <- dt %>% 
  dplyr::select("graminoid_richness_site",
                "forb_richness_site",
                "woody_richness_site", 
                "functional_richness_site", 
                "functional_redundancy_site", 
                "herbivore_biomass_kg_ha",
                "n_trigger_events_day",
                "herbivore_species_richness") %>% 
  unique()


man_s1 <- manova(cbind(forb_richness_site, graminoid_richness_site, woody_richness_site) ~ herbivore_biomass_kg_ha, data = dt_site)
summary(man_s1)
summary.aov(man_s1)

man_s2 <- manova(cbind(forb_richness_site, graminoid_richness_site, woody_richness_site) ~ n_trigger_events_day, data = dt_site)
summary(man_s2)
summary.aov(man_s2)

man_s3 <- manova(cbind(forb_richness_site, graminoid_richness_site, woody_richness_site) ~ herbivore_species_richness, data = dt_site)
summary(man_s3)
summary.aov(man_s3)

man_s4 <- manova(cbind(functional_richness_site, functional_redundancy_site) ~ herbivore_biomass_kg_ha, data = dt_site)
summary(man_s4)
summary.aov(man_s4)

man_s5 <- manova(cbind(functional_richness_site, functional_redundancy_site) ~ n_trigger_events_day, data = dt_site)
summary(man_s5)
summary.aov(man_s5)

man_s6 <- manova(cbind(functional_richness_site, functional_redundancy_site) ~ herbivore_species_richness, data = dt_site)
summary(man_s6)
summary.aov(man_s6)


