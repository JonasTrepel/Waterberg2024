### Sensitivity MAP and Herbis

library(data.table)
library(tidyverse)
library(gridExtra)
library(glmmTMB)
library(GGally)
library(MuMIn)
library(DHARMa)

dt <- fread("data/processed_data/clean_data/waterberg_2024_main_dataset.csv") 


#### run multivariate models 

######### Plot Level ###########

dt_plot <- dt %>% 
  mutate(map_plot_scaled = as.numeric(scale(map_plot)), 
         map_site_scaled = as.numeric(scale(map_site)), 
         map_reserve_scaled = as.numeric(scale(map_reserve)), 
         
         herbivore_biomass_kg_ha_scaled = as.numeric(scale(herbivore_biomass_kg_ha)),
         
         herbivore_species_richness_scaled = as.numeric(scale(herbivore_species_richness)),
         
         n_trigger_events_day_scaled = as.numeric(scale(n_trigger_events_day)), 
         n_trigger_events_day_reserve_scaled = as.numeric(scale(n_trigger_events_day_reserve)), 
         
         mat_plot_scaled = as.numeric(scale(mat_plot)), 
         mat_site_scaled = as.numeric(scale(mat_site)), 
         mat_reserve_scaled = as.numeric(scale(mat_reserve)), 
         
         elevation_plot_scaled = as.numeric(scale(elevation_plot)), 
         elevation_site_scaled = as.numeric(scale(elevation_site)), 
         elevation_reserve_scaled = as.numeric(scale(elevation_reserve)), 
         
         area_ha_scaled = as.numeric(scale(area_ha))
         
  ) 
  

  
plot_resp <- c(
  ## Taxonomic diversity 
  "plant_richness_plot",
  "graminoid_richness_plot",
  "forb_richness_plot",
  "woody_richness_plot",
  
  ## Functional_diversity  
  "functional_redundancy_plot",
  "functional_diversity_plot",
  "functional_richness_plot")

alternative_hypo <- c(
  "elevation_plot_scaled", 
  "map_plot_scaled", 
  "mat_plot_scaled", 
  "none"
)

plot_guide <- CJ(alternative_hypothesis = alternative_hypo, 
                 response = plot_resp) %>% 
  mutate(response_tier = case_when(
    response %in% c("functional_redundancy_plot","functional_diversity_plot", 
                    "functional_richness_plot") ~ "functional_diversity",
    response %in% c("plant_richness_plot", "forb_richness_plot",
                    "graminoid_richness_plot", "woody_richness_plot") ~ "taxonomic_diversity"))


resp <- "plant_richness_plot"
alt <- "none"
res_plot <- data.table()
estimates_plot <- data.table()

mod_res_plot <- data.table()

for(i in 1:nrow(plot_guide)){
  
  resp <- plot_guide[i, ]$response
  alt <-  plot_guide[i, ]$alternative_hypothesis

  
  i_form <- paste0(resp, " ~ 1 + (1 | reserve/site_ID)")
  
  
  if(alt == "none"){
    form <- paste0(resp, " ~ herbivore_biomass_kg_ha_scaled + herbivore_species_richness_scaled + n_trigger_events_day_scaled + (1 | reserve/site_ID)")
  }else{
    form <- paste0(resp, " ~ ", alt, " + herbivore_biomass_kg_ha_scaled + herbivore_species_richness_scaled + n_trigger_events_day_scaled + (1 | reserve/site_ID)")
  }

  
  if(grepl("richness", resp) &
    !grepl("functional_richness", resp)){
    
    fam <- poisson()
    
  }else{
    fam <- Gamma(link = "log")
    
  } 
  
  
  m0 <- glmmTMB(as.formula(i_form), data = dt_plot, family = fam)
  summary(m0)
  
  m <- glmmTMB(as.formula(form), data = dt_plot, family = fam)
  summary(m)
  
  m_sum <- summary(m)

  tmp <- data.table(response = resp)
  
  delta_aicc<- AICc(m0) - AICc(m)
  delta_aic<- AIC(m0) - AIC(m)
  delta_bic<- BIC(m0) - BIC(m)
  
  rsq_m <-  as.numeric(r.squaredGLMM(m)[1])
  rsq_c <-  as.numeric(r.squaredGLMM(m)[2])
  
  
  tmp_res <- tmp %>% 
    mutate(
      rsq_m = round(rsq_m, 3), 
      rsq_c = round(rsq_c, 3), 
      formula = form, 
      response = resp, 
      scale = "plot", 
      delta_aicc = delta_aicc, 
      delta_aic = delta_aic, 
      delta_bic = delta_bic, 
      alternative_hypothesis = alt)
  
  #res_plot <- rbind(res_plot, tmp)
  
  ## extract estimates 
  tidy_m <- broom.mixed::tidy(m)
  
  #bring in good shape 
  tmp_est <- tidy_m %>% 
    filter(effect == "fixed") %>% 
    mutate(ci_ub = estimate + (std.error*1.96), 
           ci_lb = estimate - (std.error*1.96), 
           scale = "plot", 
           response = resp, 
           formula = form, 
           sig = ifelse(p.value < 0.05, "significant", "non significant")) %>% 
    filter(!effect == "ran_pars") %>%
    filter(!grepl("ntercept", term)) %>% 
    mutate(group = NULL)
  
  tmp_mod_res <- left_join(tmp_est, tmp_res)
  
  mod_res_plot <- rbind(tmp_mod_res, mod_res_plot)
  
  print(paste0(i, "/", nrow(plot_guide), " done"))
  
}


mod_res_plot %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = term, color = sig), 
                  size = 0.8) +
  facet_grid(rows = vars(response), cols = vars(alternative_hypothesis), 
             scales = "free", labeller = label_wrap_gen(width = 20)) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.major = element_line(color = "gray90"), 
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  labs(
    x = "Estimate (with Confidence Interval)",
    y = "Predictor"
  )


######### Site level ###########


dt_site <- dt_plot %>%
  dplyr::select(
    ## Taxonomic diversity 
    plant_richness_site,
    graminoid_richness_site,
    forb_richness_site,
    woody_richness_site,
    
    ## Functional_diversity  
    functional_redundancy_site,
    functional_diversity_site,
    functional_richness_site,

    #predictors 
    elevation_site_scaled, 
    map_site_scaled, 
    mat_site_scaled, 
    herbivore_biomass_kg_ha_scaled,
    herbivore_species_richness_scaled,
    n_trigger_events_day_scaled,
    
    reserve,
    site_ID
  ) %>% unique() 
  

site_resp <- c(
  ## Taxonomic diversity 
  "plant_richness_site",
  "graminoid_richness_site",
  "forb_richness_site",
  "woody_richness_site",
  
  ## Functional_diversity  
  "functional_redundancy_site",
  "functional_diversity_site",
  "functional_richness_site")

alternative_hypo <- c(
  "elevation_site_scaled", 
  "map_site_scaled", 
  "mat_site_scaled", 
  "none"
)

site_guide <- CJ(alternative_hypothesis = alternative_hypo, 
                 response = site_resp) %>% 
  mutate(response_tier = case_when(
    response %in% c("functional_redundancy_site","functional_diversity_site", 
                    "functional_richness_site") ~ "functional_diversity",
    response %in% c("plant_richness_site", "forb_richness_site",
                    "graminoid_richness_site", "woody_richness_site") ~ "taxonomic_diversity"))


resp <- "plant_richness_site"
alt <- "none"
res_site <- data.table()
estimates_site <- data.table()

mod_res_site <- data.table()

for(i in 1:nrow(site_guide)){
  
  resp <- site_guide[i, ]$response
  alt <-  site_guide[i, ]$alternative_hypothesis
  
  
  i_form <- paste0(resp, " ~ 1 + (1 | reserve)")
  
  
  if(alt == "none"){
    form <- paste0(resp, " ~ herbivore_biomass_kg_ha_scaled + herbivore_species_richness_scaled + n_trigger_events_day_scaled + (1 | reserve)")
  }else{
    form <- paste0(resp, " ~ ", alt, " + herbivore_biomass_kg_ha_scaled + herbivore_species_richness_scaled + n_trigger_events_day_scaled + (1 | reserve)")
  }
  
  
  if(grepl("richness", resp) &
     !grepl("functional_richness", resp)){
    
    fam <- poisson()
    
  }else{
    fam <- Gamma(link = "log")
    
  } 
  
  
  m0 <- glmmTMB(as.formula(i_form), data = dt_site, family = fam)
  summary(m0)
  
  m <- glmmTMB(as.formula(form), data = dt_site, family = fam)
  summary(m)
  
  m_sum <- summary(m)
  
  tmp <- data.table(response = resp)
  
  delta_aicc<- AICc(m0) - AICc(m)
  delta_aic<- AIC(m0) - AIC(m)
  delta_bic<- BIC(m0) - BIC(m)
  
  rsq_m <-  as.numeric(r.squaredGLMM(m)[1])
  rsq_c <-  as.numeric(r.squaredGLMM(m)[2])
  
  
  tmp_res <- tmp %>% 
    mutate(
      rsq_m = round(rsq_m, 3), 
      rsq_c = round(rsq_c, 3), 
      formula = form, 
      response = resp, 
      scale = "plot", 
      delta_aicc = delta_aicc, 
      delta_aic = delta_aic, 
      delta_bic = delta_bic, 
      alternative_hypothesis = alt)
  
  #res_site <- rbind(res_site, tmp)
  
  ## extract estimates 
  tidy_m <- broom.mixed::tidy(m)
  
  #bring in good shape 
  tmp_est <- tidy_m %>% 
    filter(effect == "fixed") %>% 
    mutate(ci_ub = estimate + (std.error*1.96), 
           ci_lb = estimate - (std.error*1.96), 
           scale = "plot", 
           response = resp, 
           formula = form, 
           sig = ifelse(p.value < 0.05, "significant", "non significant")) %>% 
    filter(!effect == "ran_pars") %>%
    filter(!grepl("ntercept", term)) %>% 
    mutate(group = NULL)
  
  tmp_mod_res <- left_join(tmp_est, tmp_res)
  
  mod_res_site <- rbind(tmp_mod_res, mod_res_site)
  
  print(paste0(i, "/", nrow(site_guide), " done"))
  
}


mod_res_site %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(x = estimate, xmin = ci_lb, xmax = ci_ub, y = term, color = sig), 
                  size = 0.8) +
  facet_grid(rows = vars(response), cols = vars(alternative_hypothesis), 
             scales = "free", labeller = label_wrap_gen(width = 20)) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.major = element_line(color = "gray90"), 
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  labs(
    x = "Estimate (with Confidence Interval)",
    y = "Predictor"
  )



### Combine -----
res <- rbind(mod_res_plot %>% 
               mutate(scale = "plot"), mod_res_site %>% 
               mutate(scale = "site"))

fwrite(res, "builds/model_outputs/multivariate_model_estimates.csv")
