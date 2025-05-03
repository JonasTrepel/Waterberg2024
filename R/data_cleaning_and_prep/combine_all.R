#### Combine everything 

library(tidyverse)
library(data.table)

############## combine everything ################
res_meta <- fread("data/processed_data/data_fragments/reserve_meta_waterberg2024.csv")
plot_meta <- fread("data/processed_data/data_fragments/plot_meta_waterberg2024.csv")

plot_level <- fread("data/raw_data/Waterberg2024_Plot_Metadata.csv") %>% 
  dplyr::select(plot_ID, tsq_t_shrub, tsq_t_tree, bare_ground, rock_cover)

dt_div <- fread("data/processed_data/data_fragments/species_numbers_per_plot_waterberg2024.csv") %>% mutate(
  species_richness_site = (forb_richness_site + graminoid_richness_site + woody_richness_site),
  species_richness_reserve = (forb_richness_reserve + graminoid_richness_reserve + woody_richness_reserve), 
) %>%
  rename(plant_richness_plot = species_richness_plot, 
         plant_richness_site = species_richness_site, 
         plant_richness_reserve = species_richness_reserve)

summary(dt_div)


dt_div_og <- fread("data/processed_data/data_fragments/species_numbers_per_plot_waterberg2024.csv")


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
  left_join(dt_div) %>% 
  left_join(dt_ct) %>% 
  left_join(plot_level) %>% 
  unique() %>% mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)))
summary(dt_comb)


fwrite(dt_comb, "data/processed_data/clean_data/waterberg_2024_main_dataset.csv")
names(dt_comb)
