## get plot metadata 
library(mapview)
library(terra) 
library(tidyverse)
library(data.table)
library(sf)


dt_sf <- read_sf("data/spatial_data/plot_locations/plot_locations_clean_waterberg2024.gpkg") %>% 
  dplyr::select(plot_ID, date_time, geom) %>% 
  st_transform(crs = 22235) %>% 
  st_buffer(dist = 25)

mapview(dt_sf)


### Canopy heigth (meta) -------------------------------
ch <- rast("../../../../resources/spatial/meta_canopy_height/canopy_height_10m_waterberg.tif")
res(ch)
max(values(ch))


dt_sf_ch <- st_transform(dt_sf, crs = crs(ch))

ch_extr <- exactextractr::exact_extract(ch, 
                                         dt_sf_ch, 
                                         append_cols = c("plot_ID"),
                                         fun = "mean")

setnames(ch_extr, c("mean"), c("canopy_height_plot"))



## elevation --------

ele <- rast("../../../../resources/spatial/Elevation_ZAF/Elevation_SA_90m.tif")
crs(ele) # 

dt_sf_ele <- st_transform(dt_sf, crs = crs(ele))

ele_extr <- exactextractr::exact_extract(ele, 
                                         dt_sf_ele, 
                                         append_cols = c("plot_ID"),
                                         fun = "mean")

setnames(ele_extr, "mean", "elevation_plot")

## MAT---------------

mat <- rast("../../../../resources/spatial/Chelsa_Climate/CHELSA_bio1_1981-2010_V.2.1.tif") 


dt_sf_mat <- st_transform(dt_sf, crs = crs(mat))

mat_extr <- exactextractr::exact_extract(mat, 
                                         dt_sf_mat, 
                                         append_cols = c("plot_ID"),
                                         fun = "mean")

setnames(mat_extr, "mean", "mat_plot")


## MAP---------------
map <- rast("../../../../resources/spatial/Chelsa_Climate/CHELSA_bio12_1981-2010_V.2.1.tif") 


dt_sf_map <- st_transform(dt_sf, crs = crs(map))

map_extr <- exactextractr::exact_extract(map, 
                                         dt_sf_map, 
                                         append_cols = c("plot_ID"),
                                         fun = "mean")

setnames(map_extr, "mean", "map_plot")

## Tree cover ---------------
tc <- rast("../../../../resources/spatial/meta_canopy_height/woody_cover_10m_waterberg.tif") 
plot(tc)

dt_sf_tc <- st_transform(dt_sf, crs = crs(tc))

tc_extr <- exactextractr::exact_extract(tc, 
                                         dt_sf_tc, 
                                         append_cols = c("plot_ID"),
                                         fun = "mean")

setnames(tc_extr, "mean", "tree_cover_plot")


## Tree cover reiner ---------
tc_r <- rast("../../../../resources/spatial/Africa_Tree_Cover_Reiner_et_al_2023/South_Africa_treecover_2019_v1_10m.tif") 
plot(tc_r)

dt_sf_tc_r <- st_transform(dt_sf, crs = crs(tc_r))

tc_r_extr <- exactextractr::exact_extract(tc_r, 
                                        dt_sf_tc_r, 
                                        append_cols = c("plot_ID"),
                                        fun = "mean")

setnames(tc_r_extr, "mean", "reiner_tree_cover_plot")

#### join all ------


dt_final <- dt_sf %>% 
  left_join(ch_extr) %>% 
  left_join(ele_extr) %>% 
  left_join(mat_extr) %>% 
  left_join(map_extr) %>% 
  left_join(tc_extr) %>% 
  left_join(tc_r_extr) %>%
  as.data.table() %>% 
  mutate(geom = NULL, 
    reserve = case_when(
    grepl("LA", plot_ID) ~ "Lapalala", 
    grepl("JE", plot_ID) ~ "Jembisa",
    grepl("WI", plot_ID) ~ "Willowisp",
    grepl("SY", plot_ID) ~ "Syringa Sands",
    grepl("SU", plot_ID) ~ "Summerplace",
    grepl("DA", plot_ID) ~ "Dabchick",
    grepl("AN", plot_ID) ~ "Ant's Farm",
    grepl("KA", plot_ID) ~ "Kaingo",
    grepl("SW", plot_ID) ~ "Swebeswebe",
    grepl("MA", plot_ID) ~ "Marakele"), 
    site_ID = gsub("_P01", "", plot_ID),
    site_ID = gsub("_P02", "", site_ID),
    site_ID = gsub("_P03", "", site_ID),
    site_ID = gsub("_P04", "", site_ID),
    site_ID = gsub("_P05", "", site_ID)) %>% 
  group_by(site_ID) %>% 
  mutate(canopy_height_site = mean(canopy_height_plot, na.rm = T),
         elevation_site = mean(elevation_plot, na.rm = T),
         mat_site = mean(mat_plot, na.rm = T),
         map_site = mean(map_plot, na.rm = T),
         tree_cover_site = mean(tree_cover_plot, na.rm = T),
         reiner_tree_cover_site = mean(reiner_tree_cover_plot, na.rm = T)) %>% 
  ungroup()
  

summary(dt_final)
fwrite(x = dt_final, file = "data/processed_data/data_fragments/plot_meta_waterberg2024.csv")
