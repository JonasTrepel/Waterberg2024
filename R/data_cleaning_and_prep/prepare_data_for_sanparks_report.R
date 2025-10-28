## get sanparks data 

library(data.table)
library(tidyverse)
library(sf)

coords <- st_read("data/spatial_data/plot_locations/plot_locations_clean_waterberg2024.gpkg") %>% 
  st_transform(crs = 4326) %>% 
  mutate(lon = st_coordinates(.)[,1], 
         lat = st_coordinates(.)[,2]) %>% 
  as.data.frame() %>% 
  mutate(geom = NULL)


dt_sp <- fread("data/processed_data/data_fragments/plot_species_waterberg2024.csv") %>% 
  filter(reserve == "Marakele") %>% 
  mutate(hairs = case_when(
    .default = hairs, 
    hairs %in% c("Absent", "absent") ~ "Absent", 
    hairs %in% c("stems", "stems and branches") ~ "Stems", 
    hairs %in% c("leaves") ~ "Leaves", 
    hairs %in% c("both") ~ "Leaves and Stems")) %>% 
  left_join(coords[, c("plot_ID", "lon", "lat")]) 

fwrite(dt_sp, "data/processed_data/data_fragments/marakele_plots_2024_species_lists_and_traits.csv")

summary(dt_sp)

#