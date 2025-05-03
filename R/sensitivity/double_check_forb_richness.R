library(glmmTMB)
library(data.table)
library(tidyverse)
library(sjPlot)
library(ggeffects)
library(gridExtra)

dt <- fread("data/processed_data/clean_data/waterberg_2024_main_dataset.csv") 

# Plot with all reserves 
m_plot_all <- glmmTMB(forb_richness_plot ~ 
                scale(herbivore_species_richness) + 
                scale(herbivore_biomass_kg_ha) + 
                scale(n_trigger_events_day) +
                (1 | reserve/site_ID), 
              data = dt
)
summary(m_plot_all)

tidy_plot_all <- tidy(m_plot_all) %>% 
  filter(effect == "fixed" & term != "(Intercept)") %>% 
  mutate(p_bonferroni = round(p.adjust(p.value, method = "bonferroni"), 4), 
         var_name = gsub("scale\\(", "", term), 
         var_name = gsub("\\)", "", var_name), 
         sig = ifelse(p_bonferroni < 0.05, "Significant", "Non-Significant")) %>% 
  dplyr::select(var_name, p_bonferroni, p_value = p.value, estimate, std_error_estimate = std.error, sig)

predictors <- c("herbivore_species_richness", "herbivore_biomass_kg_ha", "n_trigger_events_day")

prediction_list_plot_all <- lapply(predictors, function(var) {
  pred <- ggpredict(m_plot_all, terms = var)
  as.data.table(pred)[, var_name := var]
})

dt_pred_plot_all <- rbindlist(prediction_list_plot_all) %>% 
  mutate(clean_var = case_when(
    var_name %in% c("herbivore_biomass_kg_ha") ~ "Herbivore Biomass",
    var_name %in% c("herbivore_species_richness") ~ "Herbivore Species Richness",
    var_name %in% c("n_trigger_events_day") ~ "Herbivore Visitation")) %>% 
left_join(tidy_plot_all)
    
  

dt_plot_all <- dt %>% 
  pivot_longer(cols = all_of(predictors), values_to = "var_value", names_to = "var_name") %>% 
  mutate(clean_var = case_when(
    var_name %in% c("herbivore_biomass_kg_ha") ~ "Herbivore Biomass",
    var_name %in% c("herbivore_species_richness") ~ "Herbivore Species Richness",
    var_name %in% c("n_trigger_events_day") ~ "Herbivore Visitation"))

p_plot_all <- ggplot() +
  geom_jitter(data = dt_plot_all, aes(x = var_value, y = forb_richness_plot, color = reserve), 
              alpha = 0.5, size = 2) +
  scale_color_manual(values = c("Ant's Farm" = "#33A02C",
                                "Dabchick" = "#B2DF8A",
                                "Jembisa" = "#FF7F00",
                                "Kaingo" = "#CAB2D6",
                                "Lapalala" = "#6A3D9A",
                                "Marakele" = "#FB9A99" ,
                                "Summerplace" = "#A6CEE3",
                                "Swebeswebe" = "#1F78B4",
                                "Syringa Sands" = "#FDBF6F",
                                "Willowisp" = "#E31A1C")) +
  geom_ribbon(data = dt_pred_plot_all, aes(x = x, ymin = conf.low, ymax = conf.high), 
              alpha = 0.3, fill = "grey80") +
  geom_line(data = dt_pred_plot_all, aes(x = x, y = predicted, linetype = sig), alpha = 1, linewidth = 1.4) +
  scale_linetype_manual(values = c("Significant" = "solid", "Non-Significant" = "dashed")) +
  facet_wrap(~clean_var, scales = "free_x") +
  labs(x = "Variable Value", y = "Forb Richness (Plot-Scale)", title = "All Reserves Included") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none")
p_plot_all

### Plot scale without areas without large herbivores 
dt_sub <- dt %>% 
  filter(!reserve %in% c("Willowisp", "Marakele"))

m_plot_sub <- glmmTMB(forb_richness_plot ~ 
                        scale(herbivore_species_richness) + 
                        scale(herbivore_biomass_kg_ha) + 
                        scale(n_trigger_events_day) +
                        (1 | reserve/site_ID), 
                      data = dt_sub)
summary(m_plot_sub)

tidy_plot_sub <- tidy(m_plot_sub) %>% 
  filter(effect == "fixed" & term != "(Intercept)") %>% 
  mutate(p_bonferroni = round(p.adjust(p.value, method = "bonferroni"), 4), 
         var_name = gsub("scale\\(", "", term), 
         var_name = gsub("\\)", "", var_name), 
         sig = ifelse(p_bonferroni < 0.05, "Significant", "Non-Significant")) %>% 
  dplyr::select(var_name, p_bonferroni, p_value = p.value, estimate, std_error_estimate = std.error, sig)

predictors <- c("herbivore_species_richness", "herbivore_biomass_kg_ha", "n_trigger_events_day")

prediction_list_plot_sub <- lapply(predictors, function(var) {
  pred <- ggpredict(m_plot_sub, terms = var)
  as.data.table(pred)[, var_name := var]
})

dt_pred_plot_sub <- rbindlist(prediction_list_plot_sub) %>% 
  mutate(clean_var = case_when(
    var_name %in% c("herbivore_biomass_kg_ha") ~ "Herbivore Biomass",
    var_name %in% c("herbivore_species_richness") ~ "Herbivore Species Richness",
    var_name %in% c("n_trigger_events_day") ~ "Herbivore Visitation")) %>% 
  left_join(tidy_plot_sub)



dt_plot_sub <- dt_sub %>% 
  pivot_longer(cols = all_of(predictors), values_to = "var_value", names_to = "var_name") %>% 
  mutate(clean_var = case_when(
    var_name %in% c("herbivore_biomass_kg_ha") ~ "Herbivore Biomass",
    var_name %in% c("herbivore_species_richness") ~ "Herbivore Species Richness",
    var_name %in% c("n_trigger_events_day") ~ "Herbivore Visitation"))

p_plot_sub <- ggplot() +
  geom_jitter(data = dt_plot_sub, aes(x = var_value, y = forb_richness_plot, color = reserve), 
              alpha = 0.5, size = 2) +
  scale_color_manual(values = c("Ant's Farm" = "#33A02C",
                                "Dabchick" = "#B2DF8A",
                                "Jembisa" = "#FF7F00",
                                "Kaingo" = "#CAB2D6",
                                "Lapalala" = "#6A3D9A",
                                "Marakele" = "#FB9A99" ,
                                "Summerplace" = "#A6CEE3",
                                "Swebeswebe" = "#1F78B4",
                                "Syringa Sands" = "#FDBF6F",
                                "Willowisp" = "#E31A1C")) +
  geom_ribbon(data = dt_pred_plot_sub, aes(x = x, ymin = conf.low, ymax = conf.high), 
              alpha = 0.3, fill = "grey80") +
  geom_line(data = dt_pred_plot_sub, aes(x = x, y = predicted, linetype = sig), alpha = 1, linewidth = 1.4) +
  scale_linetype_manual(values = c("Significant" = "solid", "Non-Significant" = "dashed")) +
  facet_wrap(~clean_var, scales = "free_x") +
  labs(x = "Variable Value", y = "Forb Richness (Plot-Scale)", title = "Only Reserves With Large Herbivores") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none")

p_plot_sub

########################################### Site Scale ####################################################

m_site_all <- glmmTMB(forb_richness_site ~ 
                        scale(herbivore_species_richness) + 
                        scale(herbivore_biomass_kg_ha) + 
                        scale(n_trigger_events_day) +
                        (1 | reserve), 
                      data = dt)
summary(m_site_all)

tidy_site_all <- tidy(m_site_all) %>% 
  filter(effect == "fixed" & term != "(Intercept)") %>% 
  mutate(p_bonferroni = round(p.adjust(p.value, method = "bonferroni"), 4), 
         var_name = gsub("scale\\(", "", term), 
         var_name = gsub("\\)", "", var_name), 
         sig = ifelse(p_bonferroni < 0.05, "Significant", "Non-Significant")) %>% 
  dplyr::select(var_name, p_bonferroni, p_value = p.value, estimate, std_error_estimate = std.error, sig)

predictors <- c("herbivore_species_richness", "herbivore_biomass_kg_ha", "n_trigger_events_day")

prediction_list_site_all <- lapply(predictors, function(var) {
  pred <- ggpredict(m_site_all, terms = var)
  as.data.table(pred)[, var_name := var]
})

dt_pred_site_all <- rbindlist(prediction_list_site_all) %>% 
  mutate(clean_var = case_when(
    var_name %in% c("herbivore_biomass_kg_ha") ~ "Herbivore Biomass",
    var_name %in% c("herbivore_species_richness") ~ "Herbivore Species Richness",
    var_name %in% c("n_trigger_events_day") ~ "Herbivore Visitation")) %>% 
  left_join(tidy_site_all)



dt_site_all <- dt %>% 
  pivot_longer(cols = all_of(predictors), values_to = "var_value", names_to = "var_name") %>% 
  mutate(clean_var = case_when(
    var_name %in% c("herbivore_biomass_kg_ha") ~ "Herbivore Biomass",
    var_name %in% c("herbivore_species_richness") ~ "Herbivore Species Richness",
    var_name %in% c("n_trigger_events_day") ~ "Herbivore Visitation")) %>% 
  dplyr::select(var_value, var_name, reserve, site_ID, clean_var, forb_richness_site) %>% 
  unique()

p_site_all <- ggplot() +
  geom_jitter(data = dt_site_all, aes(x = var_value, y = forb_richness_site, color = reserve), 
              alpha = 0.5, size = 2) +
  scale_color_manual(values = c("Ant's Farm" = "#33A02C",
                                "Dabchick" = "#B2DF8A",
                                "Jembisa" = "#FF7F00",
                                "Kaingo" = "#CAB2D6",
                                "Lapalala" = "#6A3D9A",
                                "Marakele" = "#FB9A99" ,
                                "Summerplace" = "#A6CEE3",
                                "Swebeswebe" = "#1F78B4",
                                "Syringa Sands" = "#FDBF6F",
                                "Willowisp" = "#E31A1C")) +
  geom_ribbon(data = dt_pred_site_all, aes(x = x, ymin = conf.low, ymax = conf.high), 
              alpha = 0.3, fill = "grey80") +
  geom_line(data = dt_pred_site_all, aes(x = x, y = predicted, linetype = sig), alpha = 1, linewidth = 1.4) +
  scale_linetype_manual(values = c("Significant" = "solid", "Non-Significant" = "dashed")) +
  facet_wrap(~clean_var, scales = "free_x") +
  labs(x = "Variable Value", y = "Forb Richness (Site-Scale)", title = "All Reserves Included") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none")
p_site_all


### Plot scale without areas without large herbivores 
dt_sub <- dt %>% 
  filter(!reserve %in% c("Willowisp", "Marakele"))

m_site_sub <- glmmTMB(forb_richness_site ~ 
                        scale(herbivore_species_richness) + 
                        scale(herbivore_biomass_kg_ha) + 
                        scale(n_trigger_events_day) +
                        (1 | reserve), 
                      data = dt_sub)
summary(m_site_sub)

tidy_site_sub <- tidy(m_site_sub) %>% 
  filter(effect == "fixed" & term != "(Intercept)") %>% 
  mutate(p_bonferroni = round(p.adjust(p.value, method = "bonferroni"), 4), 
         var_name = gsub("scale\\(", "", term), 
         var_name = gsub("\\)", "", var_name), 
         sig = ifelse(p_bonferroni < 0.05, "Significant", "Non-Significant")) %>% 
  dplyr::select(var_name, p_bonferroni, p_value = p.value, estimate, std_error_estimate = std.error, sig)

predictors <- c("herbivore_species_richness", "herbivore_biomass_kg_ha", "n_trigger_events_day")

prediction_list_site_sub <- lapply(predictors, function(var) {
  pred <- ggpredict(m_site_sub, terms = var)
  as.data.table(pred)[, var_name := var]
})

dt_pred_site_sub <- rbindlist(prediction_list_site_sub) %>% 
  mutate(clean_var = case_when(
    var_name %in% c("herbivore_biomass_kg_ha") ~ "Herbivore Biomass",
    var_name %in% c("herbivore_species_richness") ~ "Herbivore Species Richness",
    var_name %in% c("n_trigger_events_day") ~ "Herbivore Visitation")) %>% 
  left_join(tidy_site_sub)



dt_site_sub <- dt_sub %>% 
  pivot_longer(cols = all_of(predictors), values_to = "var_value", names_to = "var_name") %>% 
  mutate(clean_var = case_when(
    var_name %in% c("herbivore_biomass_kg_ha") ~ "Herbivore Biomass",
    var_name %in% c("herbivore_species_richness") ~ "Herbivore Species Richness",
    var_name %in% c("n_trigger_events_day") ~ "Herbivore Visitation")) %>% 
  dplyr::select(var_value, var_name, reserve, site_ID, clean_var, forb_richness_site) %>% 
  unique()

p_site_sub <- ggplot() +
  geom_jitter(data = dt_site_sub, aes(x = var_value, y = forb_richness_site, color = reserve), 
              alpha = 0.5, size = 2) +
  scale_color_manual(values = c("Ant's Farm" = "#33A02C",
                                "Dabchick" = "#B2DF8A",
                                "Jembisa" = "#FF7F00",
                                "Kaingo" = "#CAB2D6",
                                "Lapalala" = "#6A3D9A",
                                "Marakele" = "#FB9A99" ,
                                "Summerplace" = "#A6CEE3",
                                "Swebeswebe" = "#1F78B4",
                                "Syringa Sands" = "#FDBF6F",
                                "Willowisp" = "#E31A1C")) +
  geom_ribbon(data = dt_pred_site_sub, aes(x = x, ymin = conf.low, ymax = conf.high), 
              alpha = 0.3, fill = "grey80") +
  geom_line(data = dt_pred_site_sub, aes(x = x, y = predicted, linetype = sig), alpha = 1, linewidth = 1.4) +
  scale_linetype_manual(values = c("Significant" = "solid", "Non-Significant" = "dashed")) +
  facet_wrap(~clean_var, scales = "free_x") +
  labs(x = "Variable Value", y = "Forb Richness (Site-Scale)", title = "Only Reserves With Large Herbivores") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none")

p_site_sub

## Legend ------

p <- ggplot() +
  geom_tile(data = dt, aes(x = herbivore_biomass_kg_ha, y = map_reserve, color = reserve, fill = reserve), alpha = 1, size = 3) +
  scale_color_manual(values = c("Ant's Farm" = "#33A02C",
                                "Dabchick" = "#B2DF8A",
                                "Jembisa" = "#FF7F00",
                                "Kaingo" = "#CAB2D6",
                                "Lapalala" = "#6A3D9A",
                                "Marakele" = "#FB9A99" ,
                                "Summerplace" = "#A6CEE3",
                                "Swebeswebe" = "#1F78B4",
                                "Syringa Sands" = "#FDBF6F",
                                "Willowisp" = "#E31A1C")) +
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
  labs(color = "Reserve", fill = "Reserve") +
  theme(legend.position = "bottom", 
        legend.key = element_blank())
p

reserve_leg <- ggpubr::get_legend(p, position = "bottom")
p_leg <- as_ggplot(reserve_leg)


empty_plot <- ggplot() + theme_void()

##### Combine all 


p_comb <- grid.arrange(p_plot_all, 
                       p_plot_sub, 
                       empty_plot, 
                       p_site_all, 
                       p_site_sub, 
                       p_leg,
                       ncol = 1, 
                       heights = c(1, 1, 0.1, 1, 1, 0.2))

ggsave(plot = p_comb, "builds/plots/supplement/forb_richness_sensitivity.png", dpi = 600, height = 12, width = 10)

