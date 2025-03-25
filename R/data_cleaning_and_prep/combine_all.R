#### Combine everything 

library(tidyverse)
library(data.table)

############## combine everything ################
res_meta <- fread("data/processed_data/data_fragments/reserve_meta_waterberg2024.csv")
plot_meta <- fread("data/processed_data/data_fragments/plot_meta_waterberg2024.csv")

dt_div <- fread("data/processed_data/data_fragments/species_numbers_per_plot_waterberg2024.csv") %>% mutate(
  species_richness_site = (forb_richness_site + graminoid_richness_site + woody_richness_site),
  species_richness_reserve = (forb_richness_reserve + graminoid_richness_reserve + woody_richness_reserve), 
) %>%
  rename(plant_richness_plot = species_richness_plot, 
         plant_richness_site = species_richness_site, 
         plant_richness_reserve = species_richness_reserve)

summary(dt_div)


dt_div_og <- fread("data/processed_data/data_fragments/species_numbers_per_plot_waterberg2024.csv")



#### load lidar #####
res_lid_raw <- fread("data/processed_data/data_fragments/LidarResultsWaterberg2024Radius10m.csv")
names(res_lid_raw)
res_lid_raw2 <- res_lid_raw %>% 
  dplyr::select(
    name, adjusted_mean_3d, adjusted_mean_3d_woody, adjusted_mean_3d_herb,
    point_fraction, fraction_points_woody, point_fraction_understory,
    sd_3d,  sd_3d_woody, sd_3d_herb, 
    sd_fraction_points_partial, sd_adjusted_3d_partial,
  )

names(res_lid_raw2) <- paste0("lidar_", names(res_lid_raw2), "_plot")

plot_lid <- res_lid_raw2 %>%
  rename(plot_ID = lidar_name_plot) %>% 
  mutate(
    site_ID = gsub("_P01", "", plot_ID),
    site_ID = gsub("_P02", "", site_ID),
    site_ID = gsub("_P03", "", site_ID),
    site_ID = gsub("_P04", "", site_ID),
    site_ID = gsub("_P05", "", site_ID),
    site_ID = ifelse(grepl("SU_S04", site_ID), "SU_S04", site_ID),
    reserve = case_when(
      grepl("LA", site_ID) ~ "Lapalala", 
      grepl("JE", site_ID) ~ "Jembisa", 
      grepl("WI", site_ID) ~ "Willowisp", 
      grepl("SY", site_ID) ~ "Syringa Sands", 
      grepl("SU", site_ID) ~ "Summerplace", 
      grepl("DA", site_ID) ~ "Dabchick", 
      grepl("AN", site_ID) ~ "Ant's Farm", 
      grepl("KA", site_ID) ~ "Kaingo", 
      grepl("SW", site_ID) ~ "Swebeswebe", 
      grepl("MA", site_ID) ~ "Marakele")
  ) %>% as.data.table() 


dt_lidar <- plot_lid %>% 
  group_by(site_ID) %>% 
  mutate(lidar_adjusted_mean_3d_site = mean(lidar_adjusted_mean_3d_plot, na.rm = T),
            lidar_sd_adjusted_mean_3d_site = sd(lidar_adjusted_mean_3d_plot, na.rm = T), 
            
            lidar_adjusted_mean_3d_woody_site = mean(lidar_adjusted_mean_3d_woody_plot, na.rm = T),
            lidar_sd_adjusted_mean_3d_woody_site = sd(lidar_adjusted_mean_3d_woody_plot, na.rm = T),
            lidar_adjusted_mean_3d_herb_site = mean(lidar_adjusted_mean_3d_herb_plot, na.rm = T),
            lidar_sd_adjusted_mean_3d_herb_site = sd(lidar_adjusted_mean_3d_herb_plot, na.rm = T),
            
            lidar_mean_return_fraction_site = mean(lidar_point_fraction_plot, na.rm = T),
            lidar_mean_return_fraction_woody_site = mean(lidar_fraction_points_woody_plot, na.rm = T),
            lidar_sd_return_fraction_woody_site = sd(lidar_fraction_points_woody_plot, na.rm = T),
            lidar_sd_return_fraction_site = sd(lidar_point_fraction_plot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(reserve) %>% 
  mutate(lidar_adjusted_mean_3d_reserve = mean(lidar_adjusted_mean_3d_plot, na.rm = T),
         lidar_sd_adjusted_mean_3d_reserve = sd(lidar_adjusted_mean_3d_plot, na.rm = T), 
         
         lidar_adjusted_mean_3d_woody_reserve = mean(lidar_adjusted_mean_3d_woody_plot, na.rm = T),
         lidar_sd_adjusted_mean_3d_woody_reserve = sd(lidar_adjusted_mean_3d_woody_plot, na.rm = T),
         lidar_adjusted_mean_3d_herb_reserve = mean(lidar_adjusted_mean_3d_herb_plot, na.rm = T),
         lidar_sd_adjusted_mean_3d_herb_reserve = sd(lidar_adjusted_mean_3d_herb_plot, na.rm = T),
         
         lidar_mean_return_fraction_reserve = mean(lidar_point_fraction_plot, na.rm = T),
         lidar_mean_return_fraction_woody_reserve = mean(lidar_fraction_points_woody_plot, na.rm = T),
         lidar_sd_return_fraction_woody_reserve = sd(lidar_fraction_points_woody_plot, na.rm = T),
         lidar_sd_return_fraction_reserve = sd(lidar_point_fraction_plot, na.rm = T)) %>% 
  ungroup() %>%
  as.data.table() %>% 
  mutate(plot_ID = case_when(
    .default = plot_ID, 
    plot_ID == "SU_S04_S01" ~ "SU_S04_P01", 
    plot_ID == "SU_S04_S02" ~ "SU_S04_P02", 
    plot_ID == "SU_S04_S03" ~ "SU_S04_P03", 
    plot_ID == "SU_S04_S04" ~ "SU_S04_P04", 
    plot_ID == "SU_S04_S05" ~ "SU_S04_P05"
  ))

setdiff(unique(dt_lidar$plot_ID), unique(dt_div$plot_ID))
setdiff(unique(dt_div$plot_ID), unique(dt_lidar$plot_ID))

#load camera trap data: 
dt_ct <- fread("data/processed_data/data_fragments/camera_trap_obs.csv")
names(dt_ct)

# load plant functional diversity 

dt_fd <- fread("data/processed_data/data_fragments/plant_functional_diversity.csv")
names(dt_fd)


dt_comb <- dt_div %>% 
  left_join(dt_fd) %>% 
  left_join(res_meta) %>% 
  left_join(plot_meta) %>% 
  left_join(dt_lidar) %>% 
  left_join(dt_div) %>% 
  left_join(dt_ct) %>% 
  unique() %>% mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)))
summary(dt_comb)


fwrite(dt_comb, "data/processed_data/clean_data/waterberg_2024_main_dataset.csv")
