
library(data.table)
library(tidyverse)
library(MuMIn)
library(tidyr)
library(broom)
#library(brms)
library(tidybayes)
library(ggpubr)


dt <- fread("data/processed_data/clean_data/waterberg_2024_main_dataset.csv") 


dt_res <- fread("builds/model_outputs/univariate_glmms_r1_model_results.csv") %>% 
  filter(predictor_tier == "herbivores")
  
dt_pred_raw <- fread("builds/model_outputs/univariate_glmms_r1_model_predictions.csv")

unique(dt_res$response)

dt_est <- dt_res %>% 
  mutate(
    clean_term = case_when(
      term %in% c("map_plot_scaled", "map_site_scaled", "map_reserve_scaled") ~ "MAP",
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
      response %in% c("lidar_adjusted_mean_3d_plot", "lidar_adjusted_mean_3d_site", "lidar_adjusted_mean_3d_reserve") ~ "Vegetation Openness",
      response %in% c("lidar_adjusted_mean_3d_woody_plot", "lidar_adjusted_mean_3d_woody_site", "lidar_adjusted_mean_3d_woody_reserve") ~ "Canopy Openness",
      response %in% c("lidar_sd_adjusted_3d_partial_plot", "lidar_sd_adjusted_mean_3d_site", "lidar_sd_adjusted_mean_3d_reserve") ~ "LiDAR SD"), 
    scale_n = case_when(
      scale == "plot" ~ "Plot\nn=250",
      scale == "site" ~ "Site\nn=50",
      scale == "reserve" ~ "Reserve\nn=10"
    ),
    scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50", "Reserve\nn=10"))
  ) %>% #filter(interaction == FALSE) %>% 
  group_by(response) %>% 
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
    sig = ifelse(p_bonferroni < 0.05, "Significant", "Non-Significant"),
    rsq_label = paste0("R-sq =", round(rsq_m, 2)),
    better_than_intercept = case_when(
      delta_aicc <= -2 ~ "Worse than Null-Model",
      delta_aicc >= 2 ~ "Better than Null-Model", 
      abs(delta_aicc) < 2 ~ "Similar to Null-Model"
    )) %>% as.data.table()

dt_sig <- dt_est %>% dplyr::select(response, term, scale, p_uncorrected, p_bh, p_bonferroni, ci_overlap, response, term, formula_id) %>% unique()

fwrite(dt_sig, "builds/model_outputs/herbivore_model_significance", )
library(MetBrewer)

as.character(met.brewer("Archambault", n = 12))
#[1] "#88A0DC" "#5C5698" "#3E1E62" "#63396C" "#905877" "#CE8185" "#DB7B71" "#B6443A" "#C05029" "#E17C29" "#EFA738" "#F9D14A"
#?met.brewer

#### Define themes --------

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
                     strip.background = element_rect(fill = "seashell", color = "seashell") )

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
                     strip.background.y = element_rect(fill = "seashell", color = "seashell"),
                     strip.background.x = element_blank()
                     
) 



#### Diversity -------------------------------------------------- 
dt_est_div <- dt_est[response_tier == "taxonomic_diversity"] %>% 
  mutate(scale = factor(scale, levels = c("Plot", "Site", "Reserve")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50", "Reserve\nn=10"))
  )
# Add an empty row for Plot panel
# Ensure all levels are in `scale_n`, even if empty
dt_est_div <- dt_est_div %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci_lb = NA, ci_ub = NA, 
                       ci_overlap = "CI overlapping 0", better_than_intercept = "Similar to Null-Model"))  %>% 
  mutate(ci_overlap = factor(ci_overlap, 
                         levels = c("Negative estimate;\nCI not overlapping 0", 
                                    "CI overlapping 0",  
                                    "Positive estimate;\nCI not overlapping 0")))

p_div1 <- dt_est_div %>% 
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
  labs(y = "", x = "Estimate", title = "Taxonomic Diversity Responses", alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  theme_bw() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_first
p_div1

p_div2 <- dt_est_div %>% 
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
p_div2

p_div3 <- dt_est_div %>% 
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
p_div3

p_div4 <- dt_est_div %>% 
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
p_div4


p_div_raw <- gridExtra::grid.arrange(p_div1, p_div2, p_div3, p_div4, heights = c(1.3, 1, 1, 1))

ggsave(plot = p_div_raw, "builds/plots/taxonomic_diversity_grid_univariate.png", dpi = 600, height = 9, width = 7)


#### Functional Diversity -------------------------------------------------- 
dt_est_fd <- dt_est[response_tier == "functional_diversity", ] 


dt_est_fd <- dt_est_fd %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci_lb = NA, ci_ub = NA, 
                       ci_overlap = "CI overlapping 0", better_than_intercept = "Similar to Null-Model"))  %>% 
  mutate(ci_overlap = factor(ci_overlap, 
                         levels = c("Negative estimate;\nCI not overlapping 0", 
                                    "CI overlapping 0",  
                                    "Positive estimate;\nCI not overlapping 0")))
unique(dt_est_fd$clean_response)

# Plot 1
p_fd1 <- dt_est_fd %>%
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
  labs(y = "", x = "Estimate", title = "Functional Diversity Responses", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme_first
p_fd1

# Plot 2
p_fd2 <- dt_est_fd %>%
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
  labs(y = "", x = "Estimate", title = "Functional Diversity Responses", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme_lower
p_fd2


empty_plot <- ggplot() + theme_void()

p_fd_raw <- gridExtra::grid.arrange(p_fd1, p_fd2, heights = c(1.3, 1))
p_fd <- gridExtra::grid.arrange(p_fd_raw, lfd_leg, heights = c(5, 1))


ggsave(plot = p_fd_raw, "builds/plots/functional_diversity_grid_univariate.png", dpi = 600, height = 6, width = 6)


#### Structure ---------------------

unique(dt_est$clean_response)

dt_est_str <- dt_est[response_tier == "vegetation_structure"] %>% 
  filter(clean_term != "Herbivore Visitation") %>% 
  mutate(scale = factor(scale, levels = c("Plot", "Site", "Reserve")), 
         scale_n = factor(scale_n, levels = c("Plot\nn=250", "Site\nn=50", "Reserve\nn=10")), 
         clean_response = case_when(
           .default = clean_response, 
           clean_response == "LiDAR Mean Distance (adj.)" ~ "LiDAR\nMean Distance (adj.)",
           clean_response == "LiDAR Point Return Fraction" ~ "LiDAR\nPoint Return Fraction")
  )

dt_est_str <- dt_est_str %>%
  complete(scale_n, nesting(clean_term, clean_response),
           fill = list(estimate = NA, ci_lb = NA, ci_ub = NA, 
                       ci_overlap = "CI overlapping 0", better_than_intercept = "Similar to Null-Model"))  %>% 
  mutate(ci_overlap = factor(ci_overlap, 
                         levels = c("Negative estimate;\nCI not overlapping 0", 
                                    "CI overlapping 0",  
                                    "Positive estimate;\nCI not overlapping 0")))

# Create individual plots for Life Form Specific Diversity

# Plot 1
p_str1 <- dt_est_str %>%
  filter(clean_response == "Vegetation Openness") %>%  
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
  labs(y = "", x = "Estimate", title = "Vegetation Structure Responses", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme_first
p_str1

# Plot 2
p_str2 <- dt_est_str %>%
  filter(clean_response == "Canopy Openness") %>%  
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
  labs(y = "", x = "Estimate", title = "Ecosystem Structure", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme_lower
p_str2

p_str3 <- dt_est_str %>%
  filter(clean_response == "LiDAR SD") %>%  
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
  labs(y = "", x = "Estimate", title = "Ecosystem Structure", 
       alpha = "Quality:", color = "Significance:", shape = "Quality:") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))+
  theme_bw() +
  theme_lower

p_str3

p_str_raw <- gridExtra::grid.arrange(p_str1, p_str2, p_str3, heights = c(1.3, 1, 1))

ggsave(plot = p_str_raw, "builds/plots/vegetation_structure_grid_univariate.png", dpi = 600, height = 10, width = 7)


#### Trends ---------------------------
dt_pred <- dt_pred_raw %>% left_join(dt_est %>% 
                                       dplyr::select(c(clean_response, clean_term, formula_id,
                                                       response_tier, predictor_tier,
                                                       ci_overlap,
                                                      n, p_uncorrected, p_bh, p_bonferroni)) %>% unique()) %>% 
  filter(!is.na(p_uncorrected)) %>% 
  mutate(sig = ifelse(p_bonferroni < 0.05, "significant", "non-significant"))



##### Taxonomic Div------------
dt_pred_div <- dt_pred %>%
  filter(response_tier == "taxonomic_diversity" & grepl("CI not", ci_overlap)) %>% 
  filter(sig == "significant")

# Get unique combinations of response and clean_var
comb_div <- unique(dt_pred_div %>% dplyr::select(clean_response, clean_term, scale))%>% arrange(clean_response)

# Create a list to store individual plots
p_div_trends <- list()

# Loop over each unique combination
for(i in 1:nrow(comb_div)) {
  
 
  clean.response <- comb_div$clean_response[i]
  clean.term <- comb_div$clean_term[i]
  Scale <- comb_div$scale[i]
  
  dt_pred_sub <- dt_pred_div %>%
    filter(clean_response == clean.response, clean_term == clean.term, scale == Scale)
  
  
  term <- unique(dt_pred_sub$clean_var)
  resp <- unique(dt_pred_sub$response)
  
  
  # Create a plot with custom x and y labels
  p <- ggplot() +
    geom_point(data = dt, aes_string(x = term, y = resp, color = "reserve"), alpha = 0.5, size = 2) +
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
    geom_ribbon(data = dt_pred_sub, aes(x = clean_var_value, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey80") +
    geom_line(data = dt_pred_sub, aes(x = clean_var_value, y = predicted, linetype = sig), alpha = 1, linewidth = 1.4) +
    scale_linetype_manual(values = c("significant" = "solid", "non-significant" = "dashed")) +
    labs(x = clean.term, y = clean.response, title = paste0(str_to_title(Scale), " Scale")) + 
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "none")
  p
  # Add plot to list
  p_div_trends[[i]] <- p
}

library(patchwork)
# Combine all plots using patchwork
p_div_t <- wrap_plots(p_div_trends, ncol = 2, nrow = 4)  # Adjust ncol as needed

# Display the combined plot
print(p_div_t)

### get legend 

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
as_ggplot(reserve_leg)
ggsave(plot = reserve_leg, "builds/plots/reserve_leg.png", dpi = 600, height =2, width = 10)

ggsave(plot = p_div_t, "builds/plots/taxonomic_diversity_sig_pred.png", dpi = 600, height = 10, width = 5.5)


##### Functional Diversity -----------


dt_pred_fd <- dt_pred %>%
  filter(response_tier == "functional_diversity" & grepl("CI not", ci_overlap)) %>% 
  filter(sig == "significant")

# Get unique combinations of response and clean_var
comb_fd <- unique(dt_pred_fd %>% dplyr::select(clean_response, clean_term, scale))%>% arrange(clean_response)

# Create a list to store individual plots
p_fd_trends <- list()

# Loop over each unique combination
for(i in 1:nrow(comb_fd)) {
  
  
  clean.response <- comb_fd$clean_response[i]
  clean.term <- comb_fd$clean_term[i]
  Scale <- comb_fd$scale[i]
  
  dt_pred_sub <- dt_pred_fd %>%
    filter(clean_response == clean.response, clean_term == clean.term, scale == Scale)
  
  
  term <- unique(dt_pred_sub$clean_var)
  resp <- unique(dt_pred_sub$response)
  
  
  # Create a plot with custom x and y labels
  p <- ggplot() +
    geom_point(data = dt, aes_string(x = term, y = resp, color = "reserve"), alpha = 0.5, size = 2) +
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
    geom_ribbon(data = dt_pred_sub, aes(x = clean_var_value, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey80") +
    geom_line(data = dt_pred_sub, aes(x = clean_var_value, y = predicted, linetype = sig), alpha = 1, linewidth = 1.4) +
    scale_linetype_manual(values = c("significant" = "solid", "non-significant" = "dashed")) +
    labs(x = clean.term, y = clean.response, title = paste0(str_to_title(Scale), " Scale")) + 
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "none")
  p
  # Add plot to list
  p_fd_trends[[i]] <- p
}

library(patchwork)
# Combine all plots using patchwork
p_fd_t <- wrap_plots(p_fd_trends, ncol = 2, nrow = 2)  # Adjust ncol as needed

# Display the combined plot
print(p_fd_t)


ggsave(plot = p_fd_t, "builds/plots/functional_diversity_sig_pred.png", dpi = 600, height = 5, width = 5)


### combine -------------

empty_plot <- ggplot() + theme_void()


## Figure 2: 
p_div_raw <- gridExtra::grid.arrange(p_div1, p_div2, p_div3, p_div4, heights = c(1.3, 1, 1, 1))
p_div_trend_fig <- gridExtra::grid.arrange(p_div_trends[[1]], p_div_trends[[2]],
                             p_div_trends[[3]], p_div_trends[[4]],
                             p_div_trends[[5]], p_div_trends[[6]],
                             p_div_trends[[7]], p_div_trends[[8]], ncol = 2, nrow = 4) 
print(p_div_trend_fig)


fig2 <- gridExtra::grid.arrange(p_div_raw, empty_plot,  p_div_trend_fig, widths = c(1.2, 0.1, 1))
ggsave(plot = fig2, "builds/plots/fig2_new.png", height = 10, width = 12)

## Figure 3: 
p_fd_raw <- gridExtra::grid.arrange(p_fd1, p_fd2, heights = c(1.3, 1))
empty_plot <- ggplot() + theme_void()
p_fd_trend_fig <- gridExtra::grid.arrange(p_fd_trends[[1]], p_fd_trends[[2]],
                                          p_fd_trends[[3]], empty_plot,  ncol = 2, nrow = 2) 
print(p_fd_trend_fig)


fig3 <- gridExtra::grid.arrange(p_fd_raw, empty_plot, p_fd_trend_fig, widths = c(1.2, 0.15, 1))
ggsave(plot = fig3, "builds/plots/fig3_new.png", height = 5, width = 12)

### Export Model Stats ---------------------
dt_exp <- dt_est %>% 
  filter(response_tier %in% c("taxonomic_diversity", "functional_diversity", "vegetation_structure")) %>% 
  filter(!(response_tier == "vegetation_structure" & clean_var == "Herbivore Visitation")) %>%
  mutate(estimate_and_ci = paste0(round(estimate,2),
                                  " (", round(ci_lb, 2), "-",
                                  round(ci_ub,2), ")"), 
         rsq_m = round(rsq_m, 3), 
         rsq_c = round(rsq_c, 2), 
         clean_response = gsub("\\\n", " ", clean_response), 
         clean_response = gsub("SpeciesRichness", "Species Richness", clean_response),
         clean_response = gsub("ShannonDiversity", "Shannon Diversity", clean_response),
         clean_response = gsub("BetaDiversity", "Beta Diversity", clean_response),
         clean_response = gsub("ForbRichness", "Forb Richness", clean_response),
         clean_response = gsub("GraminoidRichness", "Graminoid Richness", clean_response),
         clean_response = gsub("Dominance\\(", "Dominance \\(", clean_response),
         clean_term = gsub("\\\n", " ", clean_term) 
  ) %>% 
  arrange(response_tier, scale, clean_response) %>% 
  dplyr::select(clean_response, clean_term, scale,
                estimate_and_ci,
                p_uncorrected, p_bh, p_bonferroni,
                rsq_m, rsq_c) %>% 
  rename(Response = clean_response,
         Predictor = clean_term, 
         Scale = scale,
         `Estimate (Confidence Interval)` = estimate_and_ci, 
         `p (uncorrected)` = p_uncorrected, 
         `p (benjamini-hochberg)` = p_bh, 
         `p (bonferroni)` = p_bonferroni, 
         `Marginal R2` = rsq_m, 
         `Conditional R2` = rsq_c
         )

fwrite(dt_exp, "builds/model_outputs/glmm_estimates_cleanish.csv")
