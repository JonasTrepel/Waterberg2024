
library(data.table)
library(tidyverse)
library(MuMIn)
library(tidyr)
library(broom)
#library(brms)
library(tidybayes)
library(ggpubr)
dt <- fread("data/processed_data/clean_data/waterberg_2024_main_dataset.csv") 


dt_res <- fread("builds/model_outputs/multivariate_model_estimates.csv")


unique(dt_res$response)

dt_est <- dt_res %>% 
  mutate(
    clean_term = case_when(
      term %in% c("map_plot_scaled", "map_site_scaled", "map_reserve_scaled") ~ "MAP",
      term %in% c("mat_plot_scaled", "mat_site_scaled", "mat_reserve_scaled") ~ "MAT",
      term %in% c("elevation_plot_scaled", "elevation_site_scaled", "elevation_reserve_scaled") ~ "Elevation",
      term %in% c("area_ha_scaled") ~ "Reserve Area",
      term %in% c("herbivore_biomass_kg_ha_scaled") ~ "Herbivore Biomass",
      term %in% c("herbivore_species_richness_scaled") ~ "Herbivore Species Richness",
      term %in% c("n_trigger_events_day_scaled", "n_trigger_events_day_reserve_scaled") ~ "Herbivore Visitation"), 
    
    clean_response = case_when(
      
      #Taxonomic Diversity
      response %in% c("plant_richness_plot", "plant_richness_site", "plant_richness_reserve") ~ "Plant Species\nRichness",
      #Life Form Specific Diversity
      response %in% c("forb_richness_plot", "forb_richness_site", "forb_richness_reserve") ~ "Forb\nRichness",
      response %in% c("graminoid_richness_plot", "graminoid_richness_site", "graminoid_richness_reserve") ~ "Graminoid\nRichness",
      response %in% c("woody_richness_plot", "woody_richness_site", "woody_richness_reserve") ~ "Woody\nRichness",
      
      #Functional Diversity 
      response %in% c("functional_richness_plot", "functional_richness_site", "functional_richness_reserve") ~ "Plant Functional\nRichness",
      response %in% c("functional_diversity_plot", "functional_diversity_site", "functional_diversity_reserve") ~ "Plant Functional\nDiversity",
      response %in% c("functional_redundancy_plot", "functional_redundancy_site", "functional_redundancy_reserve") ~ "Plant Functional\nRedundancy",
      
      
      #Additional Responses 
      response %in% c("berger_parker_plot", "berger_parker_site", "berger_parker_reserve") ~ "Plant Dominance\n(Berger-Parker)",
      response %in% c("community_dominance_plot", "community_dominance_site", "community_dominance_reserve") ~ "Community Dominance",
      response %in% c("shannon_diversity_plot", "shannon_diversity_site", "shannon_diversity_reserve") ~ "Shannon Diversity",
      
      
      #Structure
      response %in% c("lidar_adjusted_mean_3d_plot", "lidar_adjusted_mean_3d_site", "lidar_adjusted_mean_3d_site") ~ "Vegetation Openness",
      response %in% c("lidar_adjusted_mean_3d_woody_plot", "lidar_adjusted_mean_3d_woody_site", "lidar_adjusted_mean_3d_woody_reserve") ~ "Canopy Openness",
      response %in% c("lidar_sd_adjusted_3d_partial_plot", "lidar_sd_adjusted_mean_3d_site", "lidar_sd_adjusted_mean_3d_reserve") ~ "LiDAR SD"), 
    scale_n = case_when(
      scale == "plot" ~ "Plot\nn=250",
      scale == "site" ~ "Site\nn=50",
      scale == "reserve" ~ "Reserve\nn=10"
    ),
    scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50", "Reserve\nn=10"))
  ) %>% #filter(interaction == FALSE) %>% 
  group_by(response, alternative_hypothesis) %>% 
  mutate(
    p_bh = round(p.adjust(p.value, method = "BH"), 4), 
    p_bonferroni = round(p.adjust(p.value, method = "bonferroni"), 4),
    p_uncorrected = round(p.value, 4)) %>%
  ungroup() %>%
  mutate(
    ci_overlap = case_when(
      .default = "CI overlapping 0",
      ci_lb > 0 ~ "Positive estimate;\nCI not overlapping 0",
      ci_ub < 0 ~ "Negative estimate;\nCI not overlapping 0"),
    rsq_label = paste0("R-sq =", round(rsq_m, 2)),
    sig = ifelse(p_bonferroni < 0.05, "Significant", "Non-Significant"),
    better_than_intercept = case_when(
      delta_aicc <= -2 ~ "Worse than Null-Model",
      delta_aicc >= 2 ~ "Better than Null-Model", 
      abs(delta_aicc) < 2 ~ "Similar to Null-Model"
    )) %>% as.data.table()

dt_sig <- dt_est %>% dplyr::select(response, term, scale, p_uncorrected, p_bh, p_bonferroni, ci_overlap, response, term) %>% unique()


library(MetBrewer)

as.character(met.brewer("Archambault", n = 12))
#[1] "#88A0DC" "#5C5698" "#3E1E62" "#63396C" "#905877" "#CE8185" "#DB7B71" "#B6443A" "#C05029" "#E17C29" "#EFA738" "#F9D14A"
#?met_brewer

#### THEMES ####

theme_first <- theme(legend.position = "none", 
                       legend.box="vertical",
                       legend.margin=margin(),
                       legend.text = element_text(size = 12),
                       plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                       panel.grid = element_blank(), 
                       axis.title.x = element_blank(), 
                       axis.text = element_text(size = 12), 
                       axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
                       panel.border = element_rect(color = NA), 
                       panel.background = element_rect(fill = "snow"), 
                       strip.text.x = element_text(size = 14), 
                       strip.text.y = element_text(size = 14, face = "bold"), 
                       strip.background = element_rect(color = "grey85", fill = "grey85")
) 

theme_lower <- theme(legend.position = "none", 
                     legend.box="vertical",
                     legend.margin=margin(),
                     legend.text = element_text(size = 12),
                     plot.title = element_blank(),
                     panel.grid = element_blank(), 
                     axis.title.x = element_blank(), 
                     axis.text = element_text(size = 12), 
                     axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
                     panel.border = element_rect(color = NA), 
                     panel.background = element_rect(fill = "snow"), 
                     strip.text.x = element_blank(), 
                     strip.text.y = element_text(size = 14, face = "bold"), 
                     strip.background.y = element_rect(color = "grey85", fill = "grey85"),
                     strip.background.x = element_blank()
                     
) 

## No alternative --------------------
##### Taxonomic diversity -----------------
dt_div_none<- dt_est[clean_response %in% c("Plant Species\nRichness",
                                            "Graminoid\nRichness",
                                            "Forb\nRichness",
                                            "Woody\nRichness"), ] %>% 
  filter(alternative_hypothesis == "none") %>%
  mutate(scale = factor(scale, levels = c("Plot", "Site")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50"))) %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci_lb = NA, ci_ub = NA, 
                       ci_overlap = "CI overlapping 0", better_than_intercept = "Similar to Null-Model"))  %>% 
  mutate(ci_overlap = factor(ci_overlap, 
                         levels = c("Negative estimate;\nCI not overlapping 0", 
                                    "CI overlapping 0",  
                                    "Positive estimate;\nCI not overlapping 0")))

p_div_none1 <- dt_div_none%>% 
  filter(clean_response == "Plant Species\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate",
       subtitle = "Taxonomic Diversity Responses", 
       title = "No Alternative Hypothesis", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  theme_first
p_div_none1

p_div_none2 <- dt_div_none%>% 
  filter(clean_response == "Graminoid\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_div_none2

p_div_none3 <- dt_div_none%>% 
  filter(clean_response == "Forb\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_div_none3

p_div_none4 <- dt_div_none%>% 
  filter(clean_response == "Woody\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_div_none4


##### Functional diversity -------------------
dt_fd_none <- dt_est[clean_response %in% c("Plant Functional\nRichness",
                                            "Plant Functional\nDiversity",
                                            "Plant Functional\nRedundancy"), ] %>% 
  filter(alternative_hypothesis == "none") %>%
  mutate(scale = factor(scale, levels = c("Plot", "Site")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50"))) %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci_lb = NA, ci_ub = NA, 
                       ci_overlap = "CI overlapping 0", better_than_intercept = "Similar to Null-Model"))  %>% 
  mutate(ci_overlap = factor(ci_overlap, 
                             levels = c("Negative estimate;\nCI not overlapping 0", 
                                        "CI overlapping 0",  
                                        "Positive estimate;\nCI not overlapping 0")))

p_fd_none1 <- dt_fd_none %>% 
  filter(clean_response == "Plant Functional\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate",
       subtitle = "Functional Diversity Responses", 
       title = "No Alternative Hypothesis", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  theme_first
p_fd_none1

# p_fd_none2 <- dt_fd_none %>% 
#   filter(clean_response == "Plant Functional\nDiversity") %>% 
#   ggplot() +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
#   geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
#                       color = sig, fill = sig),
#                   linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
#   scale_fill_manual(values=c("Non-Significant" = "gray50",
#                              "Significant" =  "darkorange"), guide = "none") + 
#   scale_color_manual(values=c("Non-Significant" = "gray50",
#                               "Significant" =  "darkorange")) +
#   facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
#   labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
#   theme_bw() +
#   scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
#   theme_lower
# p_fd_none2

p_fd_none3 <- dt_fd_none %>% 
  filter(clean_response == "Plant Functional\nRedundancy") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_fd_none3

empty_plot <- ggplot() + theme_void()

p_fd_none <- gridExtra::grid.arrange(p_fd_none1, p_fd_none3, empty_plot, empty_plot, heights = c(1.5, 1, 1, 1))

p_div_none <- gridExtra::grid.arrange(p_div_none1, p_div_none2, p_div_none3, p_div_none4, heights = c(1.5, 1, 1, 1))

p_none <- gridExtra::grid.arrange(p_div_none, p_fd_none, ncol = 2)
ggsave(plot = p_none, "builds/plots/supplement/multivariate_glmms_no_alternative.png", height = 9.5, width = 10)


## MAP --------------------
##### Taxonomic diversity -----------------
dt_map_div_map<- dt_est[clean_response %in% c("Plant Species\nRichness",
                                                "Graminoid\nRichness",
                                                "Forb\nRichness",
                                                "Woody\nRichness"), ] %>% 
  filter(grepl("map", alternative_hypothesis)) %>%
  mutate(scale = factor(scale, levels = c("Plot", "Site")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50"))) %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci_lb = NA, ci_ub = NA, 
                       ci_overlap = "CI overlapping 0", better_than_intercept = "Similar to Null-Model"))  %>% 
  mutate(ci_overlap = factor(ci_overlap, 
                             levels = c("Negative estimate;\nCI not overlapping 0", 
                                        "CI overlapping 0",  
                                        "Positive estimate;\nCI not overlapping 0")))

p_div_map1 <- dt_map_div_map%>% 
  filter(clean_response == "Plant Species\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate",
       subtitle = "Taxonomic Diversity Responses", 
       title = "Including MAP", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  theme_first
p_div_map1

p_div_map2 <- dt_map_div_map%>% 
  filter(clean_response == "Graminoid\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_div_map2

p_div_map3 <- dt_map_div_map%>% 
  filter(clean_response == "Forb\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_div_map3

p_div_map4 <- dt_map_div_map%>% 
  filter(clean_response == "Woody\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_div_map4


##### Functional diversity -------------------
dt_map_fd_map <- dt_est[clean_response %in% c("Plant Functional\nRichness",
                                                "Plant Functional\nDiversity",
                                                "Plant Functional\nRedundancy"), ] %>% 
  filter(grepl("map", alternative_hypothesis)) %>%
  mutate(scale = factor(scale, levels = c("Plot", "Site")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50"))) %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci_lb = NA, ci_ub = NA, 
                       ci_overlap = "CI overlapping 0", better_than_intercept = "Similar to Null-Model"))  %>% 
  mutate(ci_overlap = factor(ci_overlap, 
                             levels = c("Negative estimate;\nCI not overlapping 0", 
                                        "CI overlapping 0",  
                                        "Positive estimate;\nCI not overlapping 0")))

p_fd_map1 <- dt_map_fd_map %>% 
  filter(clean_response == "Plant Functional\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate",
       subtitle = "Functional Diversity Responses", 
       title = "Including MAP", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  theme_first
p_fd_map1

# p_fd_map2 <- dt_map_fd_map %>% 
#   filter(clean_response == "Plant Functional\nDiversity") %>% 
#   ggplot() +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
#   geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
#                       color = sig, fill = sig),
#                   linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
#   scale_fill_manual(values=c("Non-Significant" = "gray50",
#                              "Significant" =  "darkorange"), guide = "none") + 
#   scale_color_manual(values=c("Non-Significant" = "gray50",
#                               "Significant" =  "darkorange")) +
#   facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
#   labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
#   theme_bw() +
#   scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
#   theme_lower
# p_fd_map2

p_fd_map3 <- dt_map_fd_map %>% 
  filter(clean_response == "Plant Functional\nRedundancy") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_fd_map3

empty_plot <- ggplot() + theme_void()

p_fd_map <- gridExtra::grid.arrange(p_fd_map1, p_fd_map3, empty_plot, empty_plot, heights = c(1.5, 1, 1, 1))

p_div_map <- gridExtra::grid.arrange(p_div_map1, p_div_map2, p_div_map3, p_div_map4, heights = c(1.5, 1, 1, 1))

p_map <- gridExtra::grid.arrange(p_div_map, p_fd_map, ncol = 2)
ggsave(plot = p_map, "builds/plots/supplement/multivariate_glmms_include_map.png", height = 9.5, width = 10)

## MAT --------------------
##### Taxonomic diversity -----------------
dt_mat_div_mat<- dt_est[clean_response %in% c("Plant Species\nRichness",
                                              "Graminoid\nRichness",
                                              "Forb\nRichness",
                                              "Woody\nRichness"), ] %>% 
  filter(grepl("mat", alternative_hypothesis)) %>%
  mutate(scale = factor(scale, levels = c("Plot", "Site")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50"))) %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci_lb = NA, ci_ub = NA, 
                       ci_overlap = "CI overlapping 0", better_than_intercept = "Similar to Null-Model"))  %>% 
  mutate(ci_overlap = factor(ci_overlap, 
                             levels = c("Negative estimate;\nCI not overlapping 0", 
                                        "CI overlapping 0",  
                                        "Positive estimate;\nCI not overlapping 0")))

p_div_mat1 <- dt_mat_div_mat%>% 
  filter(clean_response == "Plant Species\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate",
       subtitle = "Taxonomic Diversity Responses", 
       title = "Including MAT", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  theme_first
p_div_mat1

p_div_mat2 <- dt_mat_div_mat%>% 
  filter(clean_response == "Graminoid\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_div_mat2

p_div_mat3 <- dt_mat_div_mat%>% 
  filter(clean_response == "Forb\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_div_mat3

p_div_mat4 <- dt_mat_div_mat%>% 
  filter(clean_response == "Woody\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_div_mat4


##### Functional diversity -------------------
dt_mat_fd_mat <- dt_est[clean_response %in% c("Plant Functional\nRichness",
                                              "Plant Functional\nDiversity",
                                              "Plant Functional\nRedundancy"), ] %>% 
  filter(grepl("mat", alternative_hypothesis)) %>%
  mutate(scale = factor(scale, levels = c("Plot", "Site")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50"))) %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci_lb = NA, ci_ub = NA, 
                       ci_overlap = "CI overlapping 0", better_than_intercept = "Similar to Null-Model"))  %>% 
  mutate(ci_overlap = factor(ci_overlap, 
                             levels = c("Negative estimate;\nCI not overlapping 0", 
                                        "CI overlapping 0",  
                                        "Positive estimate;\nCI not overlapping 0")))

p_fd_mat1 <- dt_mat_fd_mat %>% 
  filter(clean_response == "Plant Functional\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate",
       subtitle = "Functional Diversity Responses", 
       title = "Including MAT", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  theme_first
p_fd_mat1

# p_fd_mat2 <- dt_mat_fd_mat %>% 
#   filter(clean_response == "Plant Functional\nDiversity") %>% 
#   ggplot() +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
#   geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
#                       color = sig, fill = sig),
#                   linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
#   scale_fill_manual(values=c("Non-Significant" = "gray50",
#                              "Significant" =  "darkorange"), guide = "none") + 
#   scale_color_manual(values=c("Non-Significant" = "gray50",
#                               "Significant" =  "darkorange")) +
#   facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
#   labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
#   theme_bw() +
#   scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
#   theme_lower
# p_fd_mat2

p_fd_mat3 <- dt_mat_fd_mat %>%
  filter(clean_response == "Plant Functional\nRedundancy") %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") +
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "Estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_fd_mat3

empty_plot <- ggplot() + theme_void()

p_fd_mat <- gridExtra::grid.arrange(p_fd_mat1, p_fd_mat3, empty_plot, empty_plot, heights = c(1.5, 1, 1, 1))

p_div_mat <- gridExtra::grid.arrange(p_div_mat1, p_div_mat2, p_div_mat3, p_div_mat4, heights = c(1.5, 1, 1, 1))

p_mat <- gridExtra::grid.arrange(p_div_mat, p_fd_mat, ncol = 2)
ggsave(plot = p_mat, "builds/plots/supplement/multivariate_glmms_include_mat.png", height = 9.5, width = 10)


## Elevation --------------------
##### Taxonomic diversity -----------------
dt_est_div_elevation<- dt_est[clean_response %in% c("Plant Species\nRichness",
                                              "Graminoid\nRichness",
                                              "Forb\nRichness",
                                              "Woody\nRichness"), ] %>% 
  filter(grepl("elevation", alternative_hypothesis)) %>%
  mutate(scale = factor(scale, levels = c("Plot", "Site")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50"))) %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci_lb = NA, ci_ub = NA, 
                       ci_overlap = "CI overlapping 0", better_than_intercept = "Similar to Null-Model"))  %>% 
  mutate(ci_overlap = factor(ci_overlap, 
                             levels = c("Negative estimate;\nCI not overlapping 0", 
                                        "CI overlapping 0",  
                                        "Positive estimate;\nCI not overlapping 0")))

p_div_elevation1 <- dt_est_div_elevation%>% 
  filter(clean_response == "Plant Species\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "estimate",
       subtitle = "Taxonomic Diversity Responses", 
       title = "Including Elevation", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  theme_first
p_div_elevation1

p_div_elevation2 <- dt_est_div_elevation%>% 
  filter(clean_response == "Graminoid\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_div_elevation2

p_div_elevation3 <- dt_est_div_elevation%>% 
  filter(clean_response == "Forb\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_div_elevation3

p_div_elevation4 <- dt_est_div_elevation%>% 
  filter(clean_response == "Woody\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_div_elevation4


##### Functional diversity -------------------
dt_est_fd_elevation <- dt_est[clean_response %in% c("Plant Functional\nRichness",
                                              "Plant Functional\nDiversity",
                                              "Plant Functional\nRedundancy"), ] %>% 
  filter(grepl("elevation", alternative_hypothesis)) %>%
  mutate(scale = factor(scale, levels = c("Plot", "Site")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50"))) %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci_lb = NA, ci_ub = NA, 
                       ci_overlap = "CI overlapping 0", better_than_intercept = "Similar to Null-Model"))  %>% 
  mutate(ci_overlap = factor(ci_overlap, 
                             levels = c("Negative estimate;\nCI not overlapping 0", 
                                        "CI overlapping 0",  
                                        "Positive estimate;\nCI not overlapping 0")))

p_fd_elevation1 <- dt_est_fd_elevation %>% 
  filter(clean_response == "Plant Functional\nRichness") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "estimate",
       subtitle = "Functional Diversity Responses", 
       title = "Including Elevation", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  theme_first
p_fd_elevation1

# p_fd_elevation2 <- dt_est_fd_elevation %>% 
#   filter(clean_response == "Plant Functional\nDiversity") %>% 
#   ggplot() +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
#   geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
#                       color = sig, fill = sig),
#                   linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
#   scale_fill_manual(values=c("Non-Significant" = "gray50",
#                              "Significant" =  "darkorange"), guide = "none") + 
#   scale_color_manual(values=c("Non-Significant" = "gray50",
#                               "Significant" =  "darkorange")) +
#   facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
#   labs(y = "", x = "estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
#   theme_bw() +
#   scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
#   theme_lower
# p_fd_elevation2

p_fd_elevation3 <- dt_est_fd_elevation %>% 
  filter(clean_response == "Plant Functional\nRedundancy") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.75, linewidth = .5) +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = clean_term,
                      color = sig, fill = sig),
                  linewidth = 1.5, size = 1.1, alpha = 0.85, shape = 23) +
  scale_fill_manual(values=c("Non-Significant" = "gray50",
                             "Significant" =  "darkorange"), guide = "none") + 
  scale_color_manual(values=c("Non-Significant" = "gray50",
                              "Significant" =  "darkorange")) +
  facet_grid(cols = vars(scale_n), rows = vars(clean_response), scales = "free_x", drop = FALSE) +
  labs(y = "", x = "estimate", title = "Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_lower
p_fd_elevation3

empty_plot <- ggplot() + theme_void()

p_fd_elevation <- gridExtra::grid.arrange(p_fd_elevation1, p_fd_elevation3, empty_plot, empty_plot, heights = c(1.5, 1, 1, 1))

p_div_elevation <- gridExtra::grid.arrange(p_div_elevation1, p_div_elevation2, p_div_elevation3, p_div_elevation4, heights = c(1.5, 1, 1, 1))

p_elevation <- gridExtra::grid.arrange(p_div_elevation, p_fd_elevation, ncol = 2)
ggsave(plot = p_elevation, "builds/plots/supplement/multivariate_glmms_include_elevation.png", height = 9.5, width = 10)


