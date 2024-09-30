
library(data.table)
library(tidyverse)
library(MuMIn)
library(tidyr)
library(broom)
library(brms)
library(tidybayes)

dt <- fread("data/processedData/cleanData/waterberg2024DataPrelim.csv") %>% 
  rename(species_per_reserve = total_plant_species_richness_reserve, 
         species_per_site = total_plant_species_richness_site, 
         reserve_mean_beta_divq1 = mean_beta_divq1)

foreach.results <- readRDS("builds/modelOutputs/univarBayesAug2024.Rds")

res <- foreach.results$res %>% unique() %>% as.data.table()
estimates <- foreach.results$estimates %>% unique() %>% as.data.table()
pred <- foreach.results$pred %>% unique() %>% as.data.table()
pred.int <- foreach.results$pred.int %>% unique() %>% as.data.table()
unique(estimates$term)

dt.est <- estimates %>% 
  filter(!grepl("Intercept", term)) %>% 
  mutate(clean_term = case_when(
    term %in% c("MAP_plot_scaled", "MAP_site_scaled", "MAP_scaled") ~ "MAP",
    term %in% c("herbi_biomass_ha_scaled") ~ "Herbivore Biomass",
    term %in% c("herbi_fun_ent_scaled") ~ "Herbi Functional Groups",
    term %in% c("grazer_mf_biomass_ha_scaled") ~ "Grazer Biomass",
    term %in% c("browser_mf_biomass_ha_scaled") ~ "Browser Biomass",
    term %in% c("CW_mean_species_body_mass_scaled") ~ "Body Size (CWM)", 
    term %in% c("meanBodyMassKgReserve_scaled", "meanBodyMassKg_scaled") ~ "Mean Visitor Body Mass", 
term %in% c("nEventsDayReserve_scaled", "nEventsDay_scaled") ~ "Visitor Frequency"), 

    
clean_response = case_when(
      
      #Diversity
      response %in% c("species_per_plot", "species_per_site", "species_per_reserve") ~ "Plant Species Richness",
      response %in% c("shannon_plot", "shannon_site", "shannon_reserve") ~ "Shannon Diversity",
      response %in% c("reserve_sor_beta_div", "site_sor_beta_div") ~ "Beta Diversity",
      
      #Life Form Specific Diversity
      response %in% c("forbs_per_plot", "forbs_per_site", "forbs_per_reserve") ~ "Forb Richness",
      response %in% c("graminoids_per_plot", "graminoids_per_site", "graminoids_per_reserve") ~ "Graminoid Richness",
      response %in% c("woodies_per_site", "woodies_per_reserve") ~ "Woody Species Richness",
      
      #Resilience
      response %in% c("plot_plant_evenness_pielou", "site_plant_evenness_pielou", "reserve_plant_evenness_pielou") ~ "Plant Evenness",
      response %in% c("plot_plant_fun_div_distq1", "site_plant_fun_div_distq1", "reserve_plant_fun_div_distq1") ~ "Plant Functional Diversity",
      response %in% c("plot_plant_fun_red", "site_plant_fun_red", "reserve_plant_fun_red") ~ "Plant Functional Redundancy",
      response %in% c("reserve_mean_beta_divq1", "site_mean_beta_divq1") ~ "Functional Beta Diversity",
      
      #Structure
      response %in% c("plot_lidar_adjusted_mean_3d", "site_adj_mean_3d", "reserve_adj_mean_3d") ~ "LiDAR Mean Distance (adj.)",
      response %in% c("plot_lidar_point_fraction", "site_mean_return_fraction", "reserve_mean_return_fraction") ~ "LiDAR Point Return Fraction",
      response %in% c("plot_lidar_sd_adjusted_3d_partial", "site_sd_adj_mean_3d", "reserve_sd_adj_mean_3d") ~ "LiDAR SD"), 
    scale_n = case_when(
      scale == "Plot" ~ "Plot\nn=250",
      scale == "Site" ~ "Site\nn=50",
      scale == "Reserve" ~ "Reserve\nn=10"
    ),
    scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50", "Reserve\nn=10"))
  ) %>% #filter(interaction == FALSE) %>% 
  left_join(res) %>% 
  mutate(rsq_label = paste0("R-sq =", round(r_squared, 2)),
         sig_pn = case_when(
           .default = "Non Significant",
           ci.lb > 0 ~ "Significantly Positive",
           ci.ub < 0 ~ "Significantly Negative"), 
         better_than_intercept = case_when(
           deltaLoo <= -2 ~ "Worse than Null-Model",
           deltaLoo >= 2 ~ "Better than Null-Model", 
           abs(deltaLoo) < 2 ~ "Similar to Null-Model"
         ))

library(MetBrewer)

as.character(met.brewer("Archambault", n = 12))
#[1] "#88A0DC" "#5C5698" "#3E1E62" "#63396C" "#905877" "#CE8185" "#DB7B71" "#B6443A" "#C05029" "#E17C29" "#EFA738" "#F9D14A"
?met.brewer

#### Diversity 
dt.est.div <- dt.est[response_tier == "Diversity"] %>% 
  filter(clean_term %in% c("MAP", "Herbi Functional Groups", "Herbivore Biomass", "Visitor Frequency")) %>% filter(interaction == FALSE)  %>% 
  mutate(scale = factor(scale, levels = c("Plot", "Site", "Reserve")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50", "Reserve\nn=10"))
  )

p.div <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(data = dt.est.div, aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                                         color = sig_pn, alpha = better_than_intercept),
                  linewidth = 1.3) +
  geom_text(data = dt.est.div, aes(x = 2 ,y = clean_term, label = rsq_label), position = position_nudge(y = 0.4), size = 3.5 ) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" =  .2)) +
  scale_color_manual(values=c("Non Significant" = "#88A0DC","Significantly Negative" = "#63396C","Significantly Positive"= "#ED9D34")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free") +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:") +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_text(size = 14), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background = element_rect(color = "grey85"),
        ) 
p.div


ggsave(plot = p.div, "builds/plots/diversityGridBayes.png", dpi = 600, height =9, width = 12)

#### Life Form Specific Diversity
dt.est.lfd <- dt.est[response_tier == "Life Form Specific Diversity" & clean_term != "Mean Visitor Body Mass",] 

p.lfd <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(data = dt.est.lfd, aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                                         color = sig_pn, alpha = better_than_intercept),
                  linewidth = 1.3) +
  geom_text(data = dt.est.lfd, aes(x = 1.4 ,y = clean_term, label = rsq_label), position = position_nudge(y = 0.4), size = 3.5 ) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" =  .2)) +
  scale_color_manual(values=c("Non Significant" = "#88A0DC","Significantly Negative" = "#63396C","Significantly Positive"= "#ED9D34")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free") +
  labs(y = "", x = "Estimate", title = "Life Form Specific Diversity Responses", alpha = "Quality:", color = "Significance:") +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_text(size = 14), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background = element_rect(color = "grey85"),
  ) 
p.lfd

ggsave(plot = p.lfd, "builds/plots/lifeFormDivGridBayes.png", dpi = 600, height = 10, width = 12)


#### Resilience 

dt.est.resi <- dt.est[response_tier == "Resilience"] %>% 
  filter(clean_term %in% c("MAP", "Herbi Functional Groups", "Herbivore Biomass", "Visitor Frequency")) %>% filter(interaction == FALSE)  %>% 
  mutate(scale = factor(scale, levels = c("Plot", "Site", "Reserve")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50", "Reserve\nn=10")), 
         clean_response = case_when(
           .default = clean_response, 
           clean_response == "Plant Functional Redundancy" ~ "Plant Functional\nRedundancy",
           clean_response == "Plant Functional Diversity" ~ "Plant Functional\nDiversity")
  )

p.resi <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(data = dt.est.resi, aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                                         color = sig_pn, alpha = better_than_intercept),
                  linewidth = 1.3) +
  geom_text(data = dt.est.resi, aes(x = -0.2 ,y = clean_term, label = rsq_label), position = position_nudge(y = 0.4), size = 3.5 ) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" =  .2)) +
  scale_color_manual(values=c("Non Significant" = "#88A0DC","Significantly Negative" = "#63396C","Significantly Positive"= "#ED9D34")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free") +
  labs(y = "", x = "Estimate", title = "Resilience Related Responses", alpha = "Quality:", color = "Significance:") +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_text(size = 14), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background = element_rect(color = "grey85"),
  ) 
p.resi
ggsave(plot = p.resi, "builds/plots/resilienceGridBayes.png", dpi = 600, height = 12, width = 12)

#### Structure

unique(dt.est$clean_response)

dt.est.str <- dt.est[response_tier == "Structure"] %>% 
  filter(clean_term %in% c("MAP", "Herbi Functional Groups", "Herbivore Biomass")) %>% filter(interaction == FALSE)  %>% 
  mutate(scale = factor(scale, levels = c("Plot", "Site", "Reserve")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50", "Reserve\nn=10")), 
         clean_response = case_when(
           .default = clean_response, 
           clean_response == "LiDAR Mean Distance (adj.)" ~ "LiDAR\nMean Distance (adj.)",
           clean_response == "LiDAR Point Return Fraction" ~ "LiDAR\nPoint Return Fraction")
  )

p.str <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(data = dt.est.str, aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                                          color = sig_pn, alpha = better_than_intercept),
                  linewidth = 1.3) +
  geom_text(data = dt.est.str, aes(x = -0.2 ,y = clean_term, label = rsq_label), position = position_nudge(y = 0.4), size = 3.5 ) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" =  .2)) +
  scale_color_manual(values=c("Non Significant" = "#88A0DC","Significantly Negative" = "#63396C","Significantly Positive"= "#ED9D34")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free") +
  labs(y = "", x = "Estimate", title = "Ecosystem Structure", alpha = "Quality:", color = "Significance:") +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_text(size = 14), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background = element_rect(color = "grey85"),
  ) 
p.str

ggsave(plot = p.str, "builds/plots/structureGridBayes.png", dpi = 600, height = 8, width = 12)

