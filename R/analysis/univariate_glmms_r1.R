######## 

library(data.table)
library(tidyverse)
library(MuMIn)
library(tidyr)
library(broom)
library(brms)
library(tidybayes)
library(glmmTMB)
library("sjPlot")
library(tictoc)
library(broom.mixed)


dt <- fread("data/processed_data/clean_data/waterberg_2024_main_dataset.csv")
names(dt)

## to dos: 

########################### BUILD MODEL GUIDE ############################

dt_mod <- dt %>% 
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

dt_mod$lidar_adjusted_mean_3d_plot


########################### Plot scale ####################################

responses_plot <- c(
  ## Taxonomic diversity 
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
  
  ## Additional responses  
  "community_dominance_plot",
  "berger_parker_plot",
  "shannon_diversity_plot"
)

vars_plot <- c(
  "herbivore_biomass_kg_ha_scaled",
  "herbivore_species_richness_scaled",
  "n_trigger_events_day_scaled", 
  
  ## additional predictors / alternative hypotheses 
  "mat_plot_scaled", 
  "map_plot_scaled", 
  "elevation_plot_scaled", 
  "area_ha_scaled"
  
)

## build
guide_plot <- CJ(vars = vars_plot, 
                 response = responses_plot, 
                 scale = "plot") %>% 
  mutate(formula_id = paste0("plot_formula_", 1:nrow(.)), 
         formula = paste0(response, " ~ ", vars), 
         intercept_only_formula = paste0(response, " ~ 1"), 
         formula = paste0(response, " ~ ", vars, " + (1 | reserve/site_ID)"), 
         intercept_only_formula = paste0(response, " ~ 1 + (1 | reserve/site_ID)"), 
         response_tier = case_when(
           response %in% c("functional_redundancy_plot","functional_diversity_plot", 
                           "functional_richness_plot") ~ "functional_diversity",
           response %in% c("plant_richness_plot", "forb_richness_plot",
                           "graminoid_richness_plot", "woody_richness_plot") ~ "taxonomic_diversity",
           response %in% c("community_dominance_plot", "berger_parker_plot",
                           "beta_diversity_plot", "shannon_diversity_plot") ~ "additional_responses",
           response %in% c(  "lidar_adjusted_mean_3d_plot",
                             "lidar_adjusted_mean_3d_woody_plot",
                             "lidar_sd_adjusted_3d_partial_plot") ~ "vegetation_structure"),
         predictor_tier = ifelse(vars %in% c("herbivore_biomass_kg_ha_scaled",
                                             "n_trigger_events_day_scaled", 
                                             "herbivore_species_richness_scaled"), 
                                 "herbivores", 
                                 "alternative_hypotheses")
         ) 

table(guide_plot$response_tier)

########################### Site scale ####################################

responses_site <- c(
  ## Taxonomic diversity 
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
  "lidar_sd_adjusted_mean_3d_site", # sd of mean distance travelled by a point, adjusted for the return fraction of 5 parts of the scan
  
  ## Additional responses  
  "community_dominance_site",
  "berger_parker_site",
  "beta_diversity_site", 
  "shannon_diversity_site"
)

vars_site <- c(
  "herbivore_biomass_kg_ha_scaled",
  "herbivore_species_richness_scaled",
  "n_trigger_events_day_scaled", 
  
  ## additional predictors / alternative hypotheses 
  "mat_site_scaled", 
  "map_site_scaled", 
  "elevation_site_scaled", 
  "area_ha_scaled"
  
)

## build
guide_site <- CJ(vars = vars_site, 
                 response = responses_site, 
                 scale = "site") %>% 
  mutate(formula_id = paste0("site_formula_", 1:nrow(.)), 
         formula = paste0(response, " ~ ", vars), 
         intercept_only_formula = paste0(response, " ~ 1"), 
         formula = paste0(response, " ~ ", vars, " + (1 | reserve)"), 
         intercept_only_formula = paste0(response, " ~ 1 + (1 | reserve)"), 
         response_tier = case_when(
           response %in% c("functional_redundancy_site","functional_diversity_site", 
                           "functional_richness_site") ~ "functional_diversity",
           response %in% c("plant_richness_site", "forb_richness_site",
                           "graminoid_richness_site", "woody_richness_site") ~ "taxonomic_diversity",
           response %in% c("community_dominance_site", "berger_parker_site",
                           "beta_diversity_site", "shannon_diversity_site") ~ "additional_responses",
           response %in% c(  "lidar_adjusted_mean_3d_site",
                             "lidar_adjusted_mean_3d_woody_site",
                             "lidar_sd_adjusted_mean_3d_site") ~ "vegetation_structure"),
         predictor_tier = ifelse(vars %in% c("herbivore_biomass_kg_ha_scaled",
                                             "n_trigger_events_day_scaled", 
                                             "herbivore_species_richness_scaled"), 
                                 "herbivores", 
                                 "alternative_hypotheses")
  ) 
table(guide_site$response_tier)
table(guide_site$predictor_tier)

########################### Reserve scale #################################

responses_reserve <- c(
  ## Taxonomic diversity 
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
  "lidar_sd_adjusted_mean_3d_reserve", # sd of mean distance travelled by a point, adjusted for the return fraction of 5 parts of the scan
  
  ## Additional responses  
  "community_dominance_reserve",
  "berger_parker_reserve",
  "beta_diversity_reserve", 
  "shannon_diversity_reserve"
)

vars_reserve <- c(
  "herbivore_biomass_kg_ha_scaled",
  "herbivore_species_richness_scaled",
  "n_trigger_events_day_reserve_scaled", 
  
  ## additional predictors / alternative hypotheses 
  "mat_reserve_scaled", 
  "map_reserve_scaled", 
  "elevation_reserve_scaled", 
  "area_ha_scaled"
  
)

## build
guide_reserve <- CJ(vars = vars_reserve, 
                 response = responses_reserve, 
                 scale = "reserve") %>% 
  mutate(formula_id = paste0("reserve_formula_", 1:nrow(.)), 
         formula = paste0(response, " ~ ", vars), 
         intercept_only_formula = paste0(response, " ~ 1"), 
         formula = paste0(response, " ~ ", vars), 
         response_tier = case_when(
           response %in% c("functional_redundancy_reserve","functional_diversity_reserve", 
                           "functional_richness_reserve") ~ "functional_diversity",
           response %in% c("plant_richness_reserve", "forb_richness_reserve",
                           "graminoid_richness_reserve", "woody_richness_reserve") ~ "taxonomic_diversity",
           response %in% c("community_dominance_reserve", "berger_parker_reserve",
                           "beta_diversity_reserve", "shannon_diversity_reserve") ~ "additional_responses",
           response %in% c(  "lidar_adjusted_mean_3d_reserve",
                             "lidar_adjusted_mean_3d_woody_reserve",
                             "lidar_sd_adjusted_mean_3d_reserve") ~ "vegetation_structure"),
         predictor_tier = ifelse(vars %in% c("herbivore_biomass_kg_ha_scaled",
                                             "n_trigger_events_day_reserve_scaled", 
                                             "herbivore_species_richness_scaled"), 
                                 "herbivores", 
                                 "alternative_hypotheses")
  ) 
table(guide_reserve$response_tier)
table(guide_reserve$predictor_tier)

##### combine guides ####


guide <- rbind(guide_plot, guide_site, guide_reserve) %>% 
  mutate(distribution_family = ifelse(grepl("richness", response) &
                                        !grepl("functional_richness", response), "poisson", "gamma")) 
table(guide$distribution_family)
## test 

vars <- unique(guide$vars)
responses <- unique(guide$response)

cols <- c(vars, responses)
invalids <- c()
for(col in unique(cols)){
  
  dt_sub <- tryCatch(
    {
      dt_mod %>% dplyr::select(all_of(col))
    },
    error = function(e) {cat(col, ": No valid name ") 
      return(NULL)})
  if(is.null(dt_sub)){
    invalids <- c(invalids, col)
  }else{
    print(paste0(col, ": Valid name"))
  }
}
invalids


#### run models --------

#i = 13
tic()
dt_res <- data.table()
dt_estimates <- data.table()
dt_pred <- data.table()

for(i in 1:nrow(guide)){
  
  tier <- guide[i,]$response_tier
  scale <- guide[i,]$scale
  formula_id <- guide[i,]$formula_id
  var <- guide[i,]$var
                             
  clean_var <- gsub("_scaled", "", var)
  filter_resp <- unique(guide[i, ]$response)
  
                             
  print(paste0(i," - response: ", filter_resp,"; predictor: ", clean_var, "; scale: ", scale))
                             
  guide <- guide %>% data.table() %>% as_tibble() %>% as.data.table() 
                             
  filter_resp <- unique(guide[i, ]$response)
                             
  #get formulas 
  formula <- as.formula(guide[i,]$formula)
                             
  intercept_only_formula <- as.formula(guide[i,]$intercept_only_formula)
                             
  #subset data 
  if(scale == "plot"){
         dt_sub <- dt_mod %>% 
         dplyr::select(all_of(var), all_of(clean_var), plot_ID, site_ID, reserve, all_of(filter_resp)) %>% 
         unique() %>% 
         filter(complete.cases(.))
      }else if(scale == "site"){
         dt_sub <- dt_mod %>% 
         dplyr::select(all_of(var), all_of(clean_var), site_ID, reserve, all_of(filter_resp)) %>% 
         unique() %>%
         filter(complete.cases(.))
      }else if(scale == "reserve"){
         dt_sub <- dt_mod %>% 
         dplyr::select(all_of(var), all_of(clean_var), reserve, all_of(filter_resp)) %>% 
         unique() %>%
         filter(complete.cases(.))}
                             
      #models
                             
      if(guide[i,]$distribution == "gamma"){
        fam <- Gamma(link = "log")
      }else if(guide[i,]$distribution == "poisson"){
        fam <- "poisson"
      }
                      

      m0 <- tryCatch({
                               
         glmmTMB(intercept_only_formula, data = dt_sub, family = fam)
                                 
            }, error = function(e) {cat("Model", i, "failed: ", e$message, "\n") 
                                 return(NULL) })
                            
      m <- tryCatch({
                               
         glmmTMB(formula, data = dt_sub, family = fam)
                               
           }, error = function(e) {cat("Model", i, "failed: ", e$message, "\n") 
                                 return(NULL) })
                             
     if(is.null(m)){next}
                             
     m_sum <- summary(m)
                             
     tmp <- data.table(response = filter_resp)
                             
     delta_aicc <- AICc(m0) - AICc(m)
     delta_aic <- AIC(m0) - AIC(m)
     delta_bic <- BIC(m0) - BIC(m)
                             
     rsq_m <-  as.numeric(r.squaredGLMM(m)[1])
     rsq_c <-  as.numeric(r.squaredGLMM(m)[2])
                             
                             
     tmp <- tmp %>% 
       mutate(
         rsq_m = round(rsq_m, 3),
         rsq_c = round(rsq_c, 3),
         formula = guide[i,]$formula, 
         response = guide[i,]$response,
         response_tier = guide[i,]$response_tier,
         predictor_tier = guide[i,]$predictor_tier,
         scale = guide[i,]$scale,
         formula_id = guide[i,]$formula_id, 
         delta_aicc = delta_aicc, 
         delta_aic = delta_aic, 
         delta_bic = delta_bic, 
         n = nrow(dt_sub))
                             
    dt_res <- rbind(dt_res, tmp)
                             
   ## extract estimates 
     tidy_m <- broom.mixed::tidy(m)
                             
                             
   ## bring in good shape 
     tmp_est <- tidy_m %>%
       filter(effect == "fixed") %>% 
       mutate(
         ci_ub = estimate + (std.error*1.96),
         ci_lb = estimate - (std.error*1.96), 
         response_tier = tier, 
         scale = scale, 
         formula_id = guide[i,]$formula_id,
         response = filter_resp) %>% 
       filter(!effect == "ran_pars") %>%
       filter(!grepl("ntercept", term)) %>% 
       mutate(group = NULL)
                             
      dt_estimates <- rbind(dt_estimates, tmp_est)
                             
      var_names <- tidy_m %>%
        dplyr::select(term) %>%
        filter(!grepl("ntercept", term) & term != "sd__Observation") %>%
        filter(!grepl("\\:", term))
                             
      for(j in 1:nrow(var_names)){
        
        var <- var_names[j,] %>% pull()
        
        clean_var = paste0(gsub("_scaled", "", var))
        
        p <- plot_model(m, term = var, type = "pred", ci.lvl = 0.95, show.values = T)
        
        # marg_tmp <- predict(m, 
        #                    newdata = dt_sub %>% mutate(reserve = NA, site_ID = NA), 
        #            se.fit = T) %>% as.data.frame() %>% 
        #   mutate(term = var,
        #                 clean_var = gsub("log_", "", term),
        #                 clean_var = gsub("_scaled", "", clean_var), 
        #          predicted = exp(fit), 
        #          std.error = se.fit, 
        #          var_value = dt_sub[[var]]) %>% 
        #   dplyr::select(-fit, -se.fit)
        
        marg_tmp <- p$data %>% 
          as.data.table() %>% 
          rename(var_value = x) %>% 
          mutate(term = var,
                 clean_var = gsub("log_", "", term),
                 clean_var = gsub("_scaled", "", clean_var)) %>% 
          dplyr::select(-group, -group_col) 
                                 
         if(n_distinct(dt_sub[[clean_var]]) > 10){
           
           marg_tmp$clean_var_value <-  seq(min(dt_sub[[clean_var]], na.rm = T),
                                            max(dt_sub[[clean_var]], na.rm = T), 
                                            length.out = nrow(marg_tmp))
           
           # marg_tmp <- marg_tmp %>% 
           #   mutate(clean_var_value = seq(min(dt_sub[[clean_var]], na.rm = T),
           #                                max(dt_sub[[clean_var]], na.rm = T), 
           #                                length.out = nrow(.)))
           
           }else{
             
             marg_tmp$clean_var_value <-  sort(unique(dt_sub[[clean_var]]))
             
             
             plot(seq(min(dt_sub[[clean_var]], na.rm = T),
                       max(dt_sub[[clean_var]], na.rm = T), 
                       length.out = nrow(marg_tmp)), sort(unique(dt_sub$n_trigger_events_day_reserve_scaled)))
             # marg_tmp <- marg_tmp %>% 
             #   mutate(clean_var_value = sort(unique(dt_sub[[clean_var]])))
             
             }
        if(j==1){
          marg <- marg_tmp}else{
            marg <- rbind(marg, marg_tmp)}
        }
      
      tmp_pred <- marg %>% 
        mutate(response_tier = tier, 
               scale = scale, 
               response = filter_resp,
               formula_id = formula_id)
      
      dt_pred <- rbind(dt_pred, tmp_pred)
      
      
  }
toc()
print("loop done")

dt_model_res <- dt_res %>% left_join(dt_estimates)

fwrite(dt_model_res, "builds/model_outputs/univariate_glmms_r1_model_results.csv")
fwrite(dt_pred, "builds/model_outputs/univariate_glmms_r1_model_predictions.csv")
