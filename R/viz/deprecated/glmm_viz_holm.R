
library(data.table)
library(tidyverse)
library(MuMIn)
library(tidyr)
library(broom)
library(brms)
library(tidybayes)
library(ggpubr)


dt <- fread("data/processedData/cleanData/waterberg2024DataPrelim.csv") %>% 
  rename(species_per_reserve = total_plant_species_richness_reserve, 
         species_per_site = total_plant_species_richness_site, 
         reserve_mean_beta_divq1 = mean_beta_divq1)

foreach.results <- readRDS("builds/modelOutputs/univarGLMMsOct2024.Rds")

res <- foreach.results$res %>% unique() %>% as.data.table()
estimates <- foreach.results$estimates %>% unique() %>% as.data.table()
pred <- foreach.results$pred %>% unique() %>% as.data.table() %>% dplyr::select(-std.error)
pred.int <- foreach.results$pred.int %>% unique() %>% as.data.table()
unique(estimates$response)

estimates[grepl("max_cover_ms", response) & grepl("Events", term)]

dt.est <- estimates %>% 
  filter(!grepl("Intercept", term)) %>% 
  mutate(
    ci.lb = estimate - (1.96*std.error), 
    ci.ub = estimate + (1.96*std.error), 
    
    clean_term = case_when(
      term %in% c("MAP_plot_scaled", "MAP_site_scaled", "MAP_scaled") ~ "MAP",
      term %in% c("herbi_biomass_ha_scaled") ~ "Herbivore Biomass",
      term %in% c("n_herbi_sp_reserve_scaled") ~ "Herbivore Species Richness",
      term %in% c("grazer_mf_biomass_ha_scaled") ~ "Grazer Biomass",
      term %in% c("browser_mf_biomass_ha_scaled") ~ "Browser Biomass",
      term %in% c("CW_mean_species_body_mass_scaled") ~ "Body Size (CWM)", 
      term %in% c("meanBodyMassKgReserve_scaled", "meanBodyMassKg_scaled") ~ "Mean Visitor Body Mass", 
      term %in% c("nEventsDayReserve_scaled", "nEventsDay_scaled") ~ "Herbivore Visitor Frequency"), 
    
    clean_response = case_when(
      
      #Diversity
      response %in% c("species_per_plot", "species_per_site", "species_per_reserve") ~ "Plant Species\nRichness",
      response %in% c("shannon_plot", "shannon_site", "shannon_reserve") ~ "Shannon\nDiversity",
      response %in% c("reserve_sor_beta_div", "site_sor_beta_div") ~ "Beta\nDiversity",
      
      #Life Form Specific Diversity
      response %in% c("forbs_per_plot", "forbs_per_site", "forbs_per_reserve") ~ "Forb\nRichness",
      response %in% c("graminoids_per_plot", "graminoids_per_site", "graminoids_per_reserve") ~ "Graminoid\nRichness",
      response %in% c("woodies_per_site", "woodies_per_reserve") ~ "Woody Species\nRichness",
      
      #Resilience
      response %in% c("plot_max_cover_ms", "site_max_cover_ms", "reserve_max_cover_ms") ~ "Plant Dominance\n(3 most abundant sp.)",
      response %in% c("plot_plant_fun_div_distq1", "site_plant_fun_div_distq1", "reserve_plant_fun_div_distq1") ~ "Plant Functional Diversity",
      response %in% c("plot_plant_fun_red", "site_plant_fun_red", "reserve_plant_fun_red") ~ "Plant Functional Redundancy",
      response %in% c("reserve_berger_parker", "site_berger_parker", "plot_berger_parker") ~ "Plant Dominance\n(Berger-Parker)",
      
      #Structure
      response %in% c("plot_lidar_adjusted_mean_3d", "site_adj_mean_3d", "reserve_adj_mean_3d") ~ "Vegetation Openness",
      response %in% c("plot_adj_mean_3d_woody", "site_adj_mean_3d_woody", "reserve_adj_mean_3d_woody") ~ "Canopy Openness",
      response %in% c("plot_lidar_sd_adjusted_3d_partial", "site_sd_adj_mean_3d", "reserve_sd_adj_mean_3d") ~ "LiDAR SD"), 
    scale_n = case_when(
      scale == "Plot" ~ "Plot\nn=250",
      scale == "Site" ~ "Site\nn=50",
      scale == "Reserve" ~ "Reserve\nn=10"
    ),
    scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50", "Reserve\nn=10"))
  ) %>% #filter(interaction == FALSE) %>% 
  left_join(res) %>% 
  group_by(response) %>% 
  mutate(
    p.adj = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  mutate(
    sig = ifelse(p.adj < .05, "significant", "non-significant"),
    sig_pn = case_when(
      .default = "Non Significant",
      ci.lb > 0 & p.adj > .05 ~ "Positive estimate;\nCI not overlapping 0;\nnon-significant p value",
      ci.ub < 0 & p.adj > .05 ~ "Negative estimate;\nCI not overlapping 0;\nnon-significant p value",
      estimate > 0 & p.adj < .05 ~ "Positive estimate;\nCI not overlapping 0;\nsignificant p value",
      estimate < 0 & p.adj < .05 ~ "Negative estimate;\nCI not overlapping 0;\nsignificant p value"),
    rsq_label = paste0("R-sq =", round(R2m, 2)),
    better_than_intercept = case_when(
      deltaAicc <= -2 ~ "Worse than Null-Model",
      deltaAicc >= 2 ~ "Better than Null-Model", 
      abs(deltaAicc) < 2 ~ "Similar to Null-Model"
    )) %>% as.data.table()

dtSig <- dt.est %>% dplyr::select(formula_id, sig, sig_pn, response, term) %>% unique()

dtPred <- pred %>% left_join(estimates) %>% 
  filter(!grepl("Intercept", term)) %>% 
  mutate(
    ci.lb = estimate - (1.96*std.error), 
    ci.ub = estimate + (1.96*std.error), 
    
    clean_term = case_when(
      term %in% c("MAP_plot_scaled", "MAP_site_scaled", "MAP_scaled") ~ "MAP",
      term %in% c("herbi_biomass_ha_scaled") ~ "Herbivore Biomass",
      term %in% c("n_herbi_sp_reserve_scaled") ~ "Herbivore Species Richness",
      term %in% c("grazer_mf_biomass_ha_scaled") ~ "Grazer Biomass",
      term %in% c("browser_mf_biomass_ha_scaled") ~ "Browser Biomass",
      term %in% c("CW_mean_species_body_mass_scaled") ~ "Body Size (CWM)", 
      term %in% c("meanBodyMassKgReserve_scaled", "meanBodyMassKg_scaled") ~ "Mean Visitor Body Mass", 
      term %in% c("nEventsDayReserve_scaled", "nEventsDay_scaled") ~ "Herbivore Visitor Frequency"), 
    
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
      response %in% c("plot_max_cover_ms", "site_max_cover_ms", "reserve_max_cover_ms") ~ "Plant Dominance\n(3 most abundant sp.)",
      response %in% c("plot_plant_fun_div_distq1", "site_plant_fun_div_distq1", "reserve_plant_fun_div_distq1") ~ "Plant Functional Diversity",
      response %in% c("plot_plant_fun_red", "site_plant_fun_red", "reserve_plant_fun_red") ~ "Plant Functional Redundancy",
      response %in% c("reserve_berger_parker", "site_berger_parker", "plot_berger_parker") ~ "Plant Dominance\n(Berger-Parker)",
      
      #Structure
      response %in% c("plot_lidar_adjusted_mean_3d", "site_adj_mean_3d", "reserve_adj_mean_3d") ~ "LiDAR Mean Distance (adj.)",
      response %in% c("plot_lidar_point_fraction", "site_adj_mean_3d_woody", "reserve_adj_mean_3d_woody") ~ "LiDAR Point Return Fraction",
      response %in% c("plot_lidar_sd_adjusted_3d_partial", "site_sd_adj_mean_3d", "reserve_sd_adj_mean_3d") ~ "LiDAR SD")) %>% 
  dplyr::select(-sig) %>% 
  left_join(dtSig)


library(MetBrewer)

as.character(met.brewer("Archambault", n = 12))
#[1] "#88A0DC" "#5C5698" "#3E1E62" "#63396C" "#905877" "#CE8185" "#DB7B71" "#B6443A" "#C05029" "#E17C29" "#EFA738" "#F9D14A"
#?met.brewer
  
#### Diversity -------------------------------------------------- 
dt.est.div <- dt.est[response_tier == "Diversity"] %>% 
  filter(clean_term %in% c("MAP", "Herbivore Species Richness", "Herbivore Biomass", "Herbivore Visitor Frequency")) %>%
  filter(interaction == FALSE)  %>% 
  mutate(scale = factor(scale, levels = c("Plot", "Site", "Reserve")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50", "Reserve\nn=10"))
  )
# Add an empty row for Plot panel
# Ensure all levels are in `scale_n`, even if empty
dt.est.div <- dt.est.div %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci.lb = NA, ci.ub = NA, 
                       sig_pn = "Non Significant", better_than_intercept = "Similar to Null-Model"))  %>% 
  mutate(sig_pn = factor(sig_pn, 
                         levels = c("Negative estimate;\nCI not overlapping 0;\nsignificant p value", 
                                    "Negative estimate;\nCI not overlapping 0;\nnon-significant p value", 
                                    "Non Significant", 
                                    "Positive estimate;\nCI not overlapping 0;\nnon-significant p value", 
                                    "Positive estimate;\nCI not overlapping 0;\nsignificant p value")))

p.div1 <- dt.est.div %>% 
  filter(clean_response == "Beta\nDiversity") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,   shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  #geom_text(aes(x = 0.01 ,y = clean_term, label = rsq_label), position = position_nudge(y = 0.4), size = 3.5 ) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" =  .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" =  21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_text(size = 14), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background = element_rect(color = "grey85"),
  ) 
p.div1

p.div2 <- dt.est.div %>% 
  filter(clean_response == "Plant Species\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,   shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  #geom_text(aes(x = 2 ,y = clean_term, label = rsq_label), position = position_nudge(y = 0.4), size = 3.5 ) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" =  .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" =  21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_blank(),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_blank(), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background.y = element_rect(color = "grey85"),
        strip.background.x = element_blank()
        
  ) 
p.div2

p.div3 <- dt.est.div %>% 
  filter(clean_response == "Shannon\nDiversity") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,   shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  #geom_text(aes(x = 2 ,y = clean_term, label = rsq_label), position = position_nudge(y = 0.4), size = 3.5 ) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" =  .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" =  21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_blank(),
        panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_blank(), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background.y = element_rect(color = "grey85"),
        strip.background.x = element_blank()
        
  ) 
p.div3

p.div.leg <- dt.est.div %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,   shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" =  .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" =  21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme(legend.position = "right", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12), 
        legend.key = element_blank(), 
        legend.spacing.y = unit(1, 'cm'), 
        legend.key.height = unit(1.5, 'cm'), 
        legend.title = element_text(size = 14))

div.leg <- ggpubr::get_legend(p.div.leg, position = "right")
as_ggplot(div.leg)

p.div.raw <- gridExtra::grid.arrange(p.div1, p.div2, p.div3, heights = c(1.2, 1, 1, 0.4))
p.div <- gridExtra::grid.arrange(p.div.raw, div.leg, widths = c(3, 1)) 

ggsave(plot = p.div, "builds/plots/diversityGridGLMM_bh.png", dpi = 600, height =9, width = 12)


#### Life Form Specific Diversity -------------------------------------------------- 
dt.est.lfd <- dt.est[response_tier == "Life Form Specific Diversity" & clean_term != "Mean Visitor Body Mass",] 


# Ensure all levels are in `scale_n`, even if empty
dt.est.lfd <- dt.est.lfd %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci.lb = NA, ci.ub = NA, 
                       sig_pn = "Non Significant", better_than_intercept = "Similar to Null-Model")) %>% 
  mutate(sig_pn = factor(sig_pn, 
                         levels = c("Negative estimate;\nCI not overlapping 0;\nsignificant p value", 
                                    "Negative estimate;\nCI not overlapping 0;\nnon-significant p value", 
                                    "Non Significant", 
                                    "Positive estimate;\nCI not overlapping 0;\nnon-significant p value", 
                                    "Positive estimate;\nCI not overlapping 0;\nsignificant p value")))
unique(dt.est.lfd$sig_pn)
# Create individual plots for Life Form Specific Diversity

# Plot 1
p.lfd1 <- dt.est.lfd %>%
  filter(clean_response == "Forb\nRichness") %>%  
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,  shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" = .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" = 21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Life Form Specific Diversity", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_text(size = 14), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background = element_rect(color = "grey85"),
  ) 
p.lfd1

# Plot 2
p.lfd2 <- dt.est.lfd %>%
  filter(clean_response == "Graminoid\nRichness") %>%  
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,  shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" = .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" = 21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Life Form Specific Diversity", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_blank(),
        panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        axis.title.x = element_blank(),
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_blank(), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background.y = element_rect(color = "grey85"),
        strip.background.x = element_blank()
        
  ) 
p.lfd2

p.lfd3 <- dt.est.lfd %>%
  filter(clean_response == "Woody Species\nRichness") %>%  
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,  shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" = .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" = 21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Life Form Specific Diversity", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_blank(),
        panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_blank(), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background.y = element_rect(color = "grey85"),
        strip.background.x = element_blank()
        
  ) 

p.lfd3

p.lfd.leg <- dt.est.lfd %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,   shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" =  .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" =  21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme(legend.position = "right", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12), 
        legend.key = element_blank(), 
        legend.spacing.y = unit(1, 'cm'), 
        legend.key.height = unit(1.5, 'cm'), 
        legend.title = element_text(size = 14))

lfd.leg <- ggpubr::get_legend(p.lfd.leg, position = "right")
as_ggplot(lfd.leg)

p.lfd.raw <- gridExtra::grid.arrange(p.lfd1, p.lfd2, p.lfd3, heights = c(1.2, 1, 1))
p.lfd <- gridExtra::grid.arrange(p.lfd.raw, lfd.leg, widths = c(3, 1))


ggsave(plot = p.lfd, "builds/plots/lifeFormDivGridGLMM_bh.png", dpi = 600, height = 8, width = 12)

#### Resilience -------------------------------------------------- 

dt.est.resi <- dt.est[response_tier == "Resilience"] %>% 
  filter(clean_term %in% c("MAP", "Herbivore Species Richness", "Herbivore Biomass", "Herbivore Visitor Frequency") &
           clean_response != "Functional Beta Diversity") %>% filter(interaction == FALSE)  %>% 
  mutate(scale = factor(scale, levels = c("Plot", "Site", "Reserve")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50", "Reserve\nn=10")), 
         clean_response = case_when(
           .default = clean_response, 
           clean_response == "Plant Functional Redundancy" ~ "Plant Functional\nRedundancy",
           clean_response == "Plant Functional Diversity" ~ "Plant Functional\nDiversity")
  )


dt.est.resi <- dt.est.resi %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci.lb = NA, ci.ub = NA, 
                       sig_pn = "Non Significant", better_than_intercept = "Similar to Null-Model")) %>% 
  mutate(sig_pn = factor(sig_pn, 
                         levels = c("Negative estimate;\nCI not overlapping 0;\nsignificant p value", 
                                    "Negative estimate;\nCI not overlapping 0;\nnon-significant p value", 
                                    "Non Significant", 
                                    "Positive estimate;\nCI not overlapping 0;\nnon-significant p value", 
                                    "Positive estimate;\nCI not overlapping 0;\nsignificant p value")))


#  1
p.resi1 <- dt.est.resi %>%
  filter(clean_response == "Plant Dominance\n(Berger-Parker)") %>%  
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,  shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" = .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" = 21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Resilience Related Responses", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_text(size = 14), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background = element_rect(color = "grey85"),
  ) 
p.resi1

p.resi1.5 <- dt.est.resi %>%
  filter(clean_response == "Plant Dominance\n(3 most abundant sp.)") %>%  
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,  shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" = .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" = 21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Resilience Related Responses", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_blank(),
        panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_blank(), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background.y = element_rect(color = "grey85"),
        strip.background.x = element_blank()
        
  ) 
p.resi1.5


p.resi2 <- dt.est.resi %>%
  filter(clean_response == "Plant Functional\nDiversity") %>%  
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,  shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" = .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" = 21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Life Form Specific Diversity", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_blank(),
        panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        axis.title.x = element_blank(),
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_blank(), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background.y = element_rect(color = "grey85"),
        strip.background.x = element_blank()
        
  ) 
p.resi2

p.resi3 <- dt.est.resi %>%
  filter(clean_response == "Plant Functional\nRedundancy") %>%  
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,  shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" = .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" = 21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Life Form Specific Diversity", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_blank(),
        panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_blank(), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background.y = element_rect(color = "grey85"),
        strip.background.x = element_blank()
        
  ) 

p.resi3

p.resi.leg <- dt.est.resi %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,   shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" =  .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" =  21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme(legend.position = "right", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12), 
        legend.key = element_blank(), 
        legend.spacing.y = unit(1, 'cm'), 
        legend.key.height = unit(1.5, 'cm'), 
        legend.title = element_text(size = 14))

resi.leg <- ggpubr::get_legend(p.resi.leg, position = "right")
as_ggplot(resi.leg)

p.resi.raw <- gridExtra::grid.arrange(p.resi1, p.resi1.5, p.resi2, p.resi3, heights = c(1.2, 1, 1, 1))
p.resi <- gridExtra::grid.arrange(p.resi.raw, resi.leg, widths = c(3, 1))


ggsave(plot = p.resi, "builds/plots/resilienceGridGLMM_bh.png", dpi = 600, height = 11, width = 12)


#### Structure ---------------------

unique(dt.est$clean_response)

dt.est.str <- dt.est[response_tier == "Structure"] %>% 
  filter(clean_term %in% c("MAP", "Herbivore Species Richness", "Herbivore Biomass")) %>% filter(interaction == FALSE)  %>% 
  mutate(scale = factor(scale, levels = c("Plot", "Site", "Reserve")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50", "Reserve\nn=10")), 
         clean_response = case_when(
           .default = clean_response, 
           clean_response == "LiDAR Mean Distance (adj.)" ~ "LiDAR\nMean Distance (adj.)",
           clean_response == "LiDAR Point Return Fraction" ~ "LiDAR\nPoint Return Fraction")
  )

# Ensure all levels are in `scale_n`, even if empty
dt.est.str <- dt.est.str %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci.lb = NA, ci.ub = NA, 
                       sig_pn = "Non Significant", better_than_intercept = "Similar to Null-Model")) %>% 
  mutate(sig_pn = factor(sig_pn, 
                         levels = c("Negative estimate;\nCI not overlapping 0;\nsignificant p value", 
                                    "Negative estimate;\nCI not overlapping 0;\nnon-significant p value", 
                                    "Non Significant", 
                                    "Positive estimate;\nCI not overlapping 0;\nnon-significant p value", 
                                    "Positive estimate;\nCI not overlapping 0;\nsignificant p value")))

# Create individual plots for Life Form Specific Diversity

# Plot 1
p.str1 <- dt.est.str %>%
  filter(clean_response == "Vegetation Openness") %>%  
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,  shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" = .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" = 21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Ecosystem Structure", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_text(size = 14), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background = element_rect(color = "grey85"),
  ) 
p.str1

# Plot 2
p.str2 <- dt.est.str %>%
  filter(clean_response == "Canopy Openness") %>%  
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,  shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" = .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" = 21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Ecosystem Structure", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_blank(),
        panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        axis.title.x = element_blank(),
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_blank(), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background.y = element_rect(color = "grey85"),
        strip.background.x = element_blank()
        
  ) 
p.str2

p.str3 <- dt.est.str %>%
  filter(clean_response == "LiDAR SD") %>%  
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,  shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" = .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" = 21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Ecosystem Structure", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme(legend.position = "none", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12),
        plot.title = element_blank(),
        panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        panel.border = element_rect(color = NA), 
        panel.background = element_rect(fill = "snow"), 
        strip.text.x = element_blank(), 
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.background.y = element_rect(color = "grey85"),
        strip.background.x = element_blank()
        
  ) 

p.str3

p.str.leg <- dt.est.str %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = clean_term,
                      color = sig_pn, fill = sig_pn,   shape = better_than_intercept),
                  linewidth = 1.4, size = 1.2, alpha = 0.8) +
  scale_alpha_manual(values = c("Better than Null-Model" = 1, "Similar to Null-Model" = .5, "Worse than Null-Model" =  .2)) +
  scale_shape_manual(values = c("Better than Null-Model" = 23, "Similar to Null-Model" = 22, "Worse than Null-Model" =  21)) +
  scale_fill_manual(values=c("Non Significant" = "grey50",
                             "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                             "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                             "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                             "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62"), guide = "none") + 
  scale_color_manual(values=c("Non Significant" = "grey50",
                              "Positive estimate;\nCI not overlapping 0;\nsignificant p value" =  "#E17C29",
                              "Positive estimate;\nCI not overlapping 0;\nnon-significant p value" =  "#EFA738", 
                              "Negative estimate;\nCI not overlapping 0;\nnon-significant p value" = "#905877", 
                              "Negative estimate;\nCI not overlapping 0;\nsignificant p value" = "#3E1E62")) +
  labs(y = "", x = "Estimate", title = "Ecosystem Structure", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme(legend.position = "right", 
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = 12), 
        legend.key = element_blank(), 
        legend.spacing.y = unit(1, 'cm'), 
        legend.key.height = unit(1.5, 'cm'), 
        legend.title = element_text(size = 14))

str.leg <- ggpubr::get_legend(p.str.leg, position = "right")
as_ggplot(resi.leg)

p.str.raw <- gridExtra::grid.arrange(p.str1, p.str2, p.str3, heights = c(1.2, 1, 1))
p.str <- gridExtra::grid.arrange(p.str.raw, str.leg, widths = c(3, 1))

ggsave(plot = p.str, "builds/plots/structureGridGLMM_bh.png", dpi = 600, height = 9, width = 12)


#### Trends ---------------------------


##### Div------------
dt.pred.div <- dtPred %>%
  filter(response_tier == "Diversity" & grepl("CI", sig_pn) & !grepl("MAP", term))

# Get unique combinations of response and clean_var
comb.div <- unique(dt.pred.div %>% dplyr::select(clean_response, clean_term, scale))%>% arrange(clean_response)

# Create a list to store individual plots
p.div.trends <- list()

# Loop over each unique combination
for(i in 1:nrow(comb.div)) {
  
  clean.response <- comb.div$clean_response[i]
  clean.term <- comb.div$clean_term[i]
  Scale <- comb.div$scale[i]
  
  dt.pred.sub <- dt.pred.div %>%
    filter(clean_response == clean.response, clean_term == clean.term, scale == Scale)
  
  
  term <- unique(dt.pred.sub$clean_var)
  resp <- unique(dt.pred.sub$response)
  
  
  # Create a plot with custom x and y labels
  p <- ggplot() +
    geom_point(data = dt, aes_string(x = term, y = resp, color = "reserve"), alpha = 0.5, size = 2) +
    scale_color_manual(values = c("Ant's Farm" = "#011959",
                                  "Dabchick" = "#FACCFA",
                                  "Jembisa" = "#828231",
                                  "Kaingo" = "#226061",
                                  "Lapalala" = "#F19D6B",
                                  "Marakele" = "#114360" ,
                                  "Summerplace" = "#FDB4B4",
                                  "Swebeswebe" = "#4D734D",
                                  "Syringa Sands" = "#C09036",
                                  "Willowisp" = "#677B3E")) +
    geom_ribbon(data = dt.pred.sub, aes(x = clean_var_value, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.3) +
    geom_line(data = dt.pred.sub, aes(x = clean_var_value, y = predicted, linetype = sig), alpha = 1, linewidth = 1.05) +
    scale_linetype_manual(values = c("significant" = "solid", "non-significant" = "dashed")) +
    labs(x = clean.term, y = clean.response, title = paste0(Scale, " Scale")) + 
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "none")
  p
  # Add plot to list
  p.div.trends[[i]] <- p
}
library(patchwork)
# Combine all plots using patchwork
p.div.t <- wrap_plots(p.div.trends, ncol = 5, nrow = 1)  # Adjust ncol as needed

# Display the combined plot
print(p.div.t)

### get legend 

p <- ggplot() +
  geom_tile(data = dt, aes(x = herbi_biomass_ha, y = MAP, color = reserve, fill = reserve), alpha = 1, size = 3) +
  scale_color_manual(values = c("Ant's Farm" = "#011959",
                                "Dabchick" = "#FACCFA",
                                "Jembisa" = "#828231",
                                "Kaingo" = "#226061",
                                "Lapalala" = "#F19D6B",
                                "Marakele" = "#114360" ,
                                "Summerplace" = "#FDB4B4",
                                "Swebeswebe" = "#4D734D",
                                "Syringa Sands" = "#C09036",
                                "Willowisp" = "#677B3E")) +
  scale_fill_manual(values = c("Ant's Farm" = "#011959",
                               "Dabchick" = "#FACCFA",
                               "Jembisa" = "#828231",
                               "Kaingo" = "#226061",
                               "Lapalala" = "#F19D6B",
                               "Marakele" = "#114360" ,
                               "Summerplace" = "#FDB4B4",
                               "Swebeswebe" = "#4D734D",
                               "Syringa Sands" = "#C09036",
                               "Willowisp" = "#677B3E")) +
  labs(color = "Reserve", fill = "Reserve") +
  theme(legend.position = "bottom", 
        legend.key = element_blank())
p

reserve.leg <- ggpubr::get_legend(p, position = "bottom")
as_ggplot(reserve.leg)
ggsave(plot = reserve.leg, "builds/plots/reserveLeg.png", dpi = 600, height =2, width = 10)




##### LFSD -----------

dt.pred.lfd <- dtPred %>%
  filter(response_tier == "Life Form Specific Diversity" & grepl("CI", sig_pn) & !grepl("MAP", term))

# Get unique combinations of response and clean_var
comb.lfd <- unique(dt.pred.lfd %>% dplyr::select(clean_response, clean_term, scale)) %>% arrange(clean_response)

# Create a list to store individual plots
p.lfd.trends <- list()

# Loop over each unique combination
for(i in 1:nrow(comb.lfd)) {
  
  clean.response <- comb.lfd$clean_response[i]
  clean.term <- comb.lfd$clean_term[i]
  Scale <- comb.lfd$scale[i]
  
  dt.pred.sub <- dt.pred.lfd %>%
    filter(clean_response == clean.response, clean_term == clean.term, scale == Scale)
  
  
  term <- unique(dt.pred.sub$clean_var)
  resp <- unique(dt.pred.sub$response)
  
  
  # Create a plot with custom x and y labels
  p <- ggplot() +
    geom_point(data = dt, aes_string(x = term, y = resp, color = "reserve"), alpha = 0.5, size = 2) +
    scale_color_manual(values = c("Ant's Farm" = "#011959",
                                  "Dabchick" = "#FACCFA",
                                  "Jembisa" = "#828231",
                                  "Kaingo" = "#226061",
                                  "Lapalala" = "#F19D6B",
                                  "Marakele" = "#114360" ,
                                  "Summerplace" = "#FDB4B4",
                                  "Swebeswebe" = "#4D734D",
                                  "Syringa Sands" = "#C09036",
                                  "Willowisp" = "#677B3E")) +
    geom_ribbon(data = dt.pred.sub, aes(x = clean_var_value, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.3) +
    geom_line(data = dt.pred.sub, aes(x = clean_var_value, y = predicted, linetype = sig), alpha = 1, linewidth = 1.05) +
    scale_linetype_manual(values = c("significant" = "solid", "non-significant" = "dashed")) +
    labs(x = clean.term, y = clean.response, title = paste0(Scale, " Scale")) + 
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "none")
  p
  # Add plot to list
  p.lfd.trends[[i]] <- p
}
library(patchwork)
# Combine all plots using patchwork
p.lfd.t <- wrap_plots(p.lfd.trends, ncol = 5, nrow = 2)  # Adjust ncol as needed

print(p.lfd.t)



##### Resilience ----------

dt.pred.resi <- dtPred %>%
  filter(response_tier == "Resilience" & grepl("CI", sig_pn) & !grepl("MAP", term)) %>% 
  filter(clean_term %in% c("MAP", "Herbivore Species Richness", "Herbivore Biomass", "Herbivore Visitor Frequency") &
           clean_response != "Functional Beta Diversity") %>% filter(interaction == FALSE)

# Get unique combinations of response and clean_var
comb.resi <- unique(dt.pred.resi %>% dplyr::select(clean_response, clean_term, scale)) %>% arrange(clean_response)

# Create a list to store individual plots
p.resi.trends <- list()

# Loop over each unique combination
for(i in 1:nrow(comb.resi)) {
  
  clean.response <- comb.resi$clean_response[i]
  clean.term <- comb.resi$clean_term[i]
  Scale <- comb.resi$scale[i]
  
  dt.pred.sub <- dt.pred.resi %>%
    filter(clean_response == clean.response, clean_term == clean.term, scale == Scale)
  
  
  term <- unique(dt.pred.sub$clean_var)
  resp <- unique(dt.pred.sub$response)
  
  
  # Create a plot with custom x and y labels
  p <- ggplot() +
    geom_point(data = dt, aes_string(x = term, y = resp, color = "reserve"), alpha = 0.5, size = 2) +
    scale_color_manual(values = c("Ant's Farm" = "#011959",
                                  "Dabchick" = "#FACCFA",
                                  "Jembisa" = "#828231",
                                  "Kaingo" = "#226061",
                                  "Lapalala" = "#F19D6B",
                                  "Marakele" = "#114360" ,
                                  "Summerplace" = "#FDB4B4",
                                  "Swebeswebe" = "#4D734D",
                                  "Syringa Sands" = "#C09036",
                                  "Willowisp" = "#677B3E")) +
    geom_ribbon(data = dt.pred.sub, aes(x = clean_var_value, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.3) +
    geom_line(data = dt.pred.sub, aes(x = clean_var_value, y = predicted, linetype = sig), alpha = 1, linewidth = 1.05) +
    scale_linetype_manual(values = c("significant" = "solid", "non-significant" = "dashed")) +
    labs(x = clean.term, y = clean.response, title = paste0(Scale, " Scale")) + 
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "none")
  p
  # Add plot to list
  p.resi.trends[[i]] <- p
}
library(patchwork)
p.resi.t <- wrap_plots(p.resi.trends, ncol = 4)  

print(p.resi.t)

#### Combine Diversity Predictions ####

empty_plot <- ggplot() +
  theme_void()

gridDiv <- gridExtra::grid.arrange(p.div.trends[[1]], p.div.trends[[2]],
                                         p.div.trends[[3]], p.div.trends[[4]], 
                                         ncol = 5)

gridLfd <- gridExtra::grid.arrange(p.lfd.trends[[1]], p.lfd.trends[[2]],
                                   p.lfd.trends[[3]], p.lfd.trends[[4]], 
                                   p.lfd.trends[[5]], p.lfd.trends[[6]],
                                   p.lfd.trends[[7]], p.lfd.trends[[8]], 
                                   p.lfd.trends[[9]], p.lfd.trends[[10]],
                                   ncol = 5)

gridResi <- gridExtra::grid.arrange(p.resi.trends[[1]], p.resi.trends[[2]],
                                    p.resi.trends[[3]], p.resi.trends[[4]],
                                    p.resi.trends[[5]], p.resi.trends[[6]],
                                    p.resi.trends[[7]], 
                                   ncol = 5)



gridComb <- gridExtra::grid.arrange(gridDiv, empty_plot, gridLfd, empty_plot, gridResi, empty_plot, reserve.leg, 
                                    heights = c(1, 0.2, 2, 0.2, 2, 0.05, 0.2))



ggsave(plot = gridComb, "builds/plots/gridTest.png", width = 12.3, height = 13.8)

