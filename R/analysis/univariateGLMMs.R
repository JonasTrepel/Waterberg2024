######## 
source("R/functions/partialPred.R")

library(data.table)
library(tidyverse)
library(MuMIn)
library(tidyr)
library(broom)
library(brms)
library(tidybayes)
library(glmmTMB)
library("sjPlot")


dt <- fread("data/processedData/cleanData/waterberg2024DataPrelim.csv")
names(dt)

## to dos: 

#### Compare each model to the respective intercept only model using AICc and BIC. 

########################### BUILD MODEL GUIDE ############################

dt.mod <- dt %>% 
  mutate(MAP_plot_scaled = as.numeric(scale(MAP_plot)), 
         MAP_site_scaled = as.numeric(scale(MAP_site)), 
         MAP_scaled = as.numeric(scale(MAP)), 
         
         herbi_biomass_ha_scaled = as.numeric(scale(herbi_biomass_ha)),
         herbi_fun_ent_scaled = as.numeric(scale(herbi_fun_ent)),
         meanBodyMassKg_scaled = as.numeric(scale(CW_mean_species_body_mass)),
         
         
         ## add half of mixed feeder biomass to grazers and half to browsers 
         grazer_mf_biomass_ha = grazer_biomass_ha + (mixed_feeder_biomass_ha/2),
         browser_mf_biomass_ha = browser_biomass_ha + (mixed_feeder_biomass_ha/2),
         grazer_mf_biomass_ha_scaled = as.numeric(scale(grazer_mf_biomass_ha)),
         browser_mf_biomass_ha_scaled = as.numeric(scale(browser_mf_biomass_ha)),
         
         nEventsDay_scaled = as.numeric(scale(nEventsDay)), 
         meanBodyMassKg_scaled = as.numeric(scale(meanBodyMassKg)), 
         nEventsDayReserve_scaled = as.numeric(scale(nEventsDayReserve)), 
         meanBodyMassKgReserve_scaled = as.numeric(scale(meanBodyMassKgReserve))
         
  ) %>% 
  rename(species_per_reserve = total_plant_species_richness_reserve, 
         species_per_site = total_plant_species_richness_site, 
         reserve_mean_beta_divq1 = mean_beta_divq1)


########################### Plot scale ####################################

responses.plot <- c(
  ## Diversity
  "species_per_plot",
  "shannon_plot",
  
  ## Life form diversity 
  "graminoids_per_plot",
  "forbs_per_plot",
  
  ## Resilience 
  "plot_plant_fun_red",
  "plot_plant_fun_div_distq1",
  "plot_max_cover",
  
  ## Structure
  "plot_lidar_adjusted_mean_3d",
  "plot_lidar_point_fraction",
  "plot_lidar_sd_adjusted_3d_partial"
)

vars.plot <- c(
  "MAP_plot_scaled",
  "herbi_biomass_ha_scaled",
  "herbi_fun_ent_scaled",
  # "herbi_biomass_ha_scaled*MAP_plot_scaled",
  # "herbi_fun_ent_scaled*MAP_plot_scaled",
  # "grazer_mf_biomass_ha_scaled",
  # "browser_mf_biomass_ha_scaled",
  # "meanBodyMassKg_scaled", 
  "nEventsDay_scaled"
)

## build
guide.plot <- CJ(vars = vars.plot, 
                 response = responses.plot, 
                 scale = "Plot") %>% 
  mutate(formula_id = paste0("plot_formula_", 1:nrow(.)), 
         formula = paste0(response, " ~ ", vars), 
         intercept_only_formula = paste0(response, " ~ 1"), 
         formula = paste0(response, " ~ ", vars, " + (1 | reserve/site_ID)"), 
         intercept_only_formula = paste0(response, " ~ 1 + (1 | reserve/site_ID)"), 
         response_tier = case_when(
           response %in% c("plot_plant_fun_red","plot_plant_fun_div_distq1", "plot_max_cover") ~ "Resilience",
           response %in% c("species_per_plot", "shannon_plot") ~ "Diversity",
           response %in% c( "graminoids_per_plot","forbs_per_plot") ~ "Life Form Specific Diversity",
           response %in% c(  "plot_lidar_adjusted_mean_3d",
                             "plot_lidar_point_fraction",
                             "plot_lidar_sd_adjusted_3d_partial") ~ "Structure"
         ),
         interaction = ifelse(grepl("\\*", vars), TRUE, FALSE),
         exclude = case_when(
           .default = "keep",
           vars %in% c("grazer_mf_biomass_ha_scaled","browser_mf_biomass_ha_scaled") 
           & response_tier %in% c("Resilience", "Diversity", "Structure") ~ "exclude"
         )) 

########################### Site scale ####################################

dt$site_mean_beta_divq1
unique(dt$total_plant_species_richness_reserve)

responses.site <- c(
  
  ## Diversity
  "species_per_site",
  "shannon_site",
  "site_sor_beta_div",
  
  ## Life form diversity 
  "graminoids_per_site",
  "forbs_per_site",
  "woodies_per_site",
  
  ## Resilience 
  "site_plant_fun_red",
  "site_plant_fun_div_distq1",
  "site_max_cover",
  "site_mean_beta_divq1",
  
  ## Structure
  "site_adj_mean_3d",
  "site_mean_return_fraction",
  "site_sd_adj_mean_3d"
)

vars.site <- c(
  "MAP_site_scaled",
  "herbi_biomass_ha_scaled",
  "herbi_fun_ent_scaled",
  # "herbi_biomass_ha_scaled*MAP_site_scaled",
  # "herbi_fun_ent_scaled*MAP_site_scaled",
  # "grazer_mf_biomass_ha_scaled",
  # "browser_mf_biomass_ha_scaled", 
  # "meanBodyMassKg_scaled", 
  "nEventsDay_scaled")

## build
guide.site <- CJ(vars = vars.site, 
                 response = responses.site, 
                 scale = "Site") %>% 
  mutate(formula_id = paste0("site_formula_", 1:nrow(.)), 
         formula = paste0(response, " ~ ", vars), 
         intercept_only_formula = paste0(response, " ~ 1"), 
         formula = paste0(response, " ~ ", vars, " + (1 | reserve)"), 
         intercept_only_formula = paste0(response, " ~ 1 + (1 | reserve)"),   
         response_tier = case_when(
           response %in% c("site_plant_fun_red","site_plant_fun_div_distq1", "site_max_cover", "site_mean_beta_divq1") ~ "Resilience",
           response %in% c("species_per_site", "shannon_site", "site_sor_beta_div") ~ "Diversity",
           response %in% c( "graminoids_per_site","forbs_per_site", "woodies_per_site") ~ "Life Form Specific Diversity",
           response %in% c(  "site_adj_mean_3d",
                             "site_mean_return_fraction",
                             "site_sd_adj_mean_3d") ~ "Structure"
         ),
         interaction = ifelse(grepl("\\*", vars), TRUE, FALSE),
         exclude = case_when(
           .default = "keep",
           vars %in% c("grazer_mf_biomass_ha_scaled","browser_mf_biomass_ha_scaled","grazer_mf_nb_fe_scaled","browser_mf_nb_fe_scaled") 
           & response_tier %in% c("Resilience", "Diversity", "Structure") ~ "exclude"
         )) 


########################### Reserve scale #################################

unique(dt$mean_beta_divq1)

responses.reserve <- c(
  
  ## Diversity
  "species_per_reserve",
  "shannon_reserve",
  "reserve_sor_beta_div",
  
  ## Life form diversity 
  "graminoids_per_reserve",
  "forbs_per_reserve",
  "woodies_per_reserve",
  
  
  ## Resilience 
  "reserve_plant_fun_red",
  "reserve_plant_fun_div_distq1",
  "reserve_max_cover",
  "reserve_mean_beta_divq1",
  
  ## Structure
  "reserve_adj_mean_3d",
  "reserve_mean_return_fraction",
  "reserve_sd_adj_mean_3d"
  
)

vars.reserve <- c(
  "MAP_scaled",
  "herbi_biomass_ha_scaled",
  "herbi_fun_ent_scaled",
  # "herbi_biomass_ha_scaled*MAP_scaled",
  # "herbi_fun_ent_scaled*MAP_scaled",
  # "grazer_mf_biomass_ha_scaled",
  # "browser_mf_biomass_ha_scaled", 
  #"meanBodyMassKgReserve_scaled", 
  "nEventsDayReserve_scaled")

## build
guide.reserve <- CJ(vars = vars.reserve, 
                    response = responses.reserve, 
                    scale = "Reserve") %>% 
  mutate(formula_id = paste0("reserve_formula_", 1:nrow(.)), 
         formula = paste0(response, " ~ ", vars), 
         intercept_only_formula = paste0(response, " ~ 1"), 
         response_tier = case_when(
           response %in% c("reserve_plant_fun_red","reserve_plant_fun_div_distq1", "reserve_max_cover", "reserve_mean_beta_divq1") ~ "Resilience",
           response %in% c("species_per_reserve", "shannon_reserve", "reserve_sor_beta_div") ~ "Diversity",
           response %in% c( "graminoids_per_reserve","forbs_per_reserve", "woodies_per_reserve") ~ "Life Form Specific Diversity",
           response %in% c(  "reserve_adj_mean_3d",
                             "reserve_mean_return_fraction",
                             "reserve_sd_adj_mean_3d") ~ "Structure"
         ),
         interaction = ifelse(grepl("\\*", vars), TRUE, FALSE),
         exclude = case_when(
           .default = "keep",
           vars %in% c("grazer_mf_biomass_ha_scaled","browser_mf_biomass_ha_scaled","grazer_mf_nb_fe_scaled","browser_mf_nb_fe_scaled") 
           & response_tier %in% c("Resilience", "Diversity", "Structure") ~ "exclude"
         ))


##### combine guides ####


guide <- rbind(guide.plot, guide.site, guide.reserve) %>% filter(exclude == "keep")

## test 

vars <- unique(guide$vars)
responses <- unique(guide$response)

cols <- c(vars, responses)
invalids <- c()
for(col in unique(cols)){
  
  dt.sub <- tryCatch(
    {
      dt.mod %>% dplyr::select(all_of(col))
    },
    error = function(e) {cat(col, ": No valid name ") 
      return(NULL)})
  if(is.null(dt.sub)){
    invalids <- c(invalids, col)
  }else{
    print(paste0(col, ": Valid name"))
  }
}
invalids


#### run models --------

############### create cluster ####################
library(doSNOW)
library(foreach)
library(tictoc)
# Number of cores to use
num_cores <- 2 #detectCores() - 2

# Create and register a cluster
clust <- makeCluster(num_cores)
registerDoSNOW(clust)

## progress bar 
iterations <- nrow(guide)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


res <- data.table()
estimates <- data.table() 
pred <- data.table()
pred.int <- data.table()

rbind.lists <- function(x, y) {combined.list <- list(res = rbind(x$res, y$res),
                                                     estimates = rbind(x$estimates, y$estimates),
                                                     pred = rbind(x$pred, y$pred),
                                                     pred.int = rbind(x$pred.int, y$pred.int)) 
return(combined.list)}

#i = 1
tic()
foreach.results <- foreach(i = 1:nrow(guide),
                           .packages = c('dplyr', 'brms', 'mgcv', 'broom',
                                         'ggplot2', 'tidyr', 'data.table', 'Metrics',
                                         'tidyverse', 'MuMIn' ,"MetBrewer", 'sjPlot',
                                         'grid', 'gridExtra','loo', 'glmmTMB'),
                           .options.snow = opts,
                           .inorder = FALSE,
                           .combine = rbind.lists) %dopar% {

                             
#for(i in 1:nrow(guide)){                                                     
                             tier <- guide[i,]$response_tier
                             scale <- guide[i,]$scale
                             formula.id <- guide[i,]$formula_id
                             var <- guide[i,]$var
                             
                             clean.var <- gsub("_scaled", "", var)
                             
                             print(paste0(i," tier: ", tier," at scale: ", scale))
                             
                             guide <- guide %>% data.table() %>% as_tibble() %>% as.data.table() 
                             
                             filter.resp <- unique(guide[i, ]$response)
                             
                             #get formulas 
                             formula <- as.formula(guide[i,]$formula)
                             
                             intercept_only_formula <- as.formula(guide[i,]$intercept_only_formula)
                             
                             #subset data 
                             if(scale == "Plot"){
                               dt.sub <- dt.mod %>% 
                                 dplyr::select(all_of(var), all_of(clean.var), plot_ID, site_ID, reserve, all_of(filter.resp)) %>% 
                                 unique() %>% filter(complete.cases(.))
                             }
                             
                             if(scale == "Site"){
                               dt.sub <- dt.mod %>% 
                                 dplyr::select(all_of(var), all_of(clean.var), site_ID, reserve, all_of(filter.resp)) %>% 
                                 unique() %>% filter(complete.cases(.))
                             }
                             
                             if(scale == "Reserve"){
                               dt.sub <- dt.mod %>% 
                                 dplyr::select(all_of(var), all_of(clean.var), reserve, all_of(filter.resp)) %>% 
                                 unique() %>% filter(complete.cases(.))
                             }
                             
                             #models
                             m0 <- tryCatch(
                               {glmmTMB(intercept_only_formula, data = dt.sub)},
                               error = function(e) {cat("Model", i, "failed: ", e$message, "\n") 
                                 return(NULL) })
                             
                             m <- tryCatch(
                               {glmmTMB(formula, data = dt.sub)},
                               error = function(e) {cat("Model", i, "failed: ", e$message, "\n") 
                                 return(NULL) })
                             
                             if(is.null(m)){next}
                               
                               m.sum <- summary(m)
                               
                               
                               tmp <- data.frame(r_squared = NA, 
                                                 waic = NA, 
                                                 loo = NA,
                                                 formula = NA, 
                                                 response = NA)
                               
                               deltaAicc<- AICc(m0) - AICc(m)
                               deltaAic<- AIC(m0) - AIC(m)
                               deltaBIC<- BIC(m0) - BIC(m)
                               
                               R2m <-  as.numeric(r.squaredGLMM(m)[1])
                               R2c <-  as.numeric(r.squaredGLMM(m)[2])
                               
                               
                               tmp <- tmp %>% 
                                 mutate(
                                   R2m = round(R2m, 3), 
                                   R2c = round(R2c, 3), 
                                   aic = AIC(m),  
                                   aicc = AICc(m), 
                                   bic = BIC(m), 
                                   formula = guide[i,]$formula, 
                                   response = guide[i,]$response, 
                                   tier = guide[i,]$tier,
                                   scale = guide[i,]$scale,
                                   formula_id = formula.id, 
                                   deltaAicc = deltaAicc, 
                                   deltaAic = deltaAic, 
                                   deltaBIC = deltaBIC
                                
                                 )
                               
                               
                               res <- rbind(res, tmp)
                               
                               tidy.m <- broom.mixed::tidy(m)
                               
                               #bring in good shape 
                               tmp.est <- tidy.m %>% 
                                 filter(effect == "fixed") %>% 
                                 mutate(ci.ub = estimate + (std.error*1.96), 
                                        ci.lb = estimate - (std.error*1.96), 
                                        response_tier = tier, 
                                        scale = scale, 
                                        formula_id = formula.id, 
                                        response = filter.resp, 
                                        sig = ifelse(p.value < 0.05, "significant", "non significant"), 
                                        interaction = guide[i, ]$interaction) %>% 
                                 filter(!effect == "ran_pars") %>%
                                 filter(!grepl("ntercept", term)) %>% 
                                 mutate(group = NULL)
                               
                                estimates <- rbind(estimates, tmp.est)
                               
                               var.names <- tidy.m %>% dplyr::select(term) %>% filter(!grepl("ntercept", term) & term != "sd__Observation") %>% filter(!grepl("\\:", term))
                               
                               ### loop through vars and get pred <
                               
                               if(guide[i, ]$interaction == FALSE){
                                 
                                 for(j in 1:nrow(var.names)){
                                   
                                   var <- var.names[j,] %>% pull()
                                   
        
                                   
                                   clean.var = paste0(gsub("_scaled", "", var))
                                   
                                   p <- plot_model(m, term = var, type = "pred")
                                   
                                   marg.tmp <- p$data %>% 
                                     as.data.table() %>% 
                                     rename(var_value = x) %>% 
                                     mutate(term = var,
                                            clean_var = gsub("log_", "", term),
                                            clean_var = gsub("_scaled", "", clean_var)) %>% 
                                     dplyr::select(-group, -group_col) 
                                   
                                   if(n_distinct(dt[[clean.var]]) > 10){
                                     marg.tmp <- marg.tmp %>% 
                                       mutate(clean_var_value = seq(min(dt[[clean.var]], na.rm = T),
                                                                    max(dt[[clean.var]], na.rm = T), 
                                                                    length.out = nrow(.)))
                                   }else{
                                     marg.tmp <- marg.tmp %>% 
                                       mutate(clean_var_value = sort(unique(dt[[clean.var]])))
                                   }
                                   
                                   # marg.tmp <- partialPred(model = m, response = filter.resp, var = var,
                                   #                         data = dt.sub,
                                   #                         newdata = dt.sub %>%
                                   #                           dplyr::select(-c(all_of(filter.resp))) %>% 
                                   #                           mutate(reserve = NA, 
                                   #                                  site = NA)) 
                                   # marg.tmp <- marg.tmp %>% rename(var_value = paste0(gsub("_scaled", "", var))) %>% mutate(term = var,
                                   #                                                                                          clean_var = gsub("log_", "", term),
                                   #                                                                                          clean_var = gsub("_scaled", "", clean_var), 
                                   #                                                                                          response_value = dt.sub %>% dplyr::select(c(all_of(filter.resp)))%>% pull())
                                   if(j==1){
                                     marg <- marg.tmp}else{
                                       marg <- rbind(marg, marg.tmp)}
                                 }
                                 
                                 tmp.pred <- marg %>% 
                                   mutate(response_tier = tier, 
                                          scale = scale, 
                                          response = filter.resp,
                                          formula_id = formula.id
                                   )
                                 
                                 pred <- rbind(tmp.pred, pred)
                                 
                               }else{
                                 
                                 for(j in 1:nrow(var.names)){
                                   
                                   var <- var.names[j,] %>% pull()
                                   
                                   moderator = case_when(
                                     scale == "Plot" ~ "MAP_plot_scaled", 
                                     scale == "Site" ~ "MAP_site_scaled",
                                     scale == "Reserve" ~ "MAP_scaled",
                                   )
                                   
                                   
                                   marg.tmp <- partialPred(model = m, response = filter.resp, var = var,
                                                                interaction = TRUE, moderator = moderator,
                                                                data = dt.mod, newdata = dt.mod %>% dplyr::select(-c(all_of(filter.resp))))
                                   
                                   moderator.clean = case_when(
                                     scale == "Plot" ~ "MAP_plot", 
                                     scale == "Site" ~ "MAP_site",
                                     scale == "Reserve" ~ "MAP",
                                   )
                                   
                                   marg.tmp <- marg.tmp %>% rename(var_value = paste0(gsub("_scaled", "", var))) %>% mutate(term = var,
                                                                                                                            clean_var = gsub("log_", "", term),
                                                                                                                            clean_var = gsub("_scaled", "", clean_var),
                                                                                                                            response_value = dt.mod %>% dplyr::select(c(all_of(filter.resp)))%>% pull(),
                                                                                                                            moderator_value = dt.mod %>% dplyr::select(c(all_of(moderator.clean)))%>% pull())
                                   if(j==1){
                                     marg <- marg.tmp}else{
                                       marg <- rbind(marg, marg.tmp)}
                                 }
                                 
                                 tmp.pred.int <- marg %>% 
                                   mutate(response_tier = tier, 
                                          scale = scale, 
                                          response = filter.resp,
                                          formula_id = formula.id)
                                 
               pred.int <- rbind(tmp.pred.int, pred.int)
                                 
          }
                               
                               
          res.list <- list(res = res, estimates = estimates, pred = pred, pred.int = pred.int)
          return(res.list)
}

print("loop done")

stopCluster(clust)

saveRDS(foreach.results, "builds/modelOutputs/univarGLMMsOct2024.Rds")

toc()
stop(pb)

res <- foreach.results$res %>% unique() %>% as.data.table()
estimates <- foreach.results$estimates %>% unique() %>% as.data.table()
pred <- foreach.results$pred %>% unique() %>% as.data.table()
pred.int <- foreach.results$pred.int %>% unique() %>% as.data.table()
