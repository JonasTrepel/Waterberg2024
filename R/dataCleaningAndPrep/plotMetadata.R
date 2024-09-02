## get plot metadata 
library(mapview)
library(terra) 
library(tidyverse)
library(data.table)
library(sf)


dt.sf <- read_sf("data/spatialData/plotLocations/plot_locations_clean_waterberg2024.gpkg") %>% 
  dplyr::select(plot_ID, date_time, geom) %>% 
  st_transform(crs = 22235) %>% 
  st_buffer(dist = 25)

mapview(dt.sf)

source("R/functions/get.heterogeneity.R")

### Canopy heigth (meta) -------------------------------
ch <- rast("../../../../resources/spatial/meta_canopy_height/canopy_height_10m_waterberg.tif")
res(ch)
max(values(ch))

ch.exp <- get.heterogeneity(vector = dt.sf, grid = NULL, id.col = "plot_ID", raster = ch)
setnames(ch.exp, c("mean"), 
         c("canopy_height_mean_plot"))
ch.exp$geom <- NULL



dt.sf.cov1 <- dt.sf %>% 
  left_join(ch.exp, by = "plot_ID") %>%  
  as.data.table() %>% 
  mutate(geom = NULL) %>%
  unique() 



## elevation --------

ele <- rast("../../../../resources/spatial/Elevation_ZAF/Elevation_SA_90m.tif")
crs(ele) # 

ele.extr <- get.heterogeneity(vector = dt.sf, grid = NULL, id.col = "plot_ID", raster = ele)

ele.extr$geom<- NULL

setnames(ele.extr, c("mean"), 
         c("elevation_mean_plot"))
range(ele.extr$elevation_mean)

dt.sf.cov2 <- dt.sf.cov1 %>% 
  left_join(ele.extr, by = "plot_ID")

## MAT---------------

mat <- rast("../../../../resources/spatial/Chelsa_Climate/CHELSA_bio1_1981-2010_V.2.1.tif") 


mat.extr <- get.heterogeneity(vector = dt.sf, grid = NULL, id.col = "plot_ID", raster = mat)
setnames(mat.extr, c("mean"), 
         c("MAT_plot"))
mat.extr$geom <- NULL
range(mat.extr$MAT)

dt.sf.cov3 <- dt.sf.cov2 %>% 
  left_join(mat.extr, by = "plot_ID") 


## MAP---------------
map <- rast("../../../../resources/spatial/Chelsa_Climate/CHELSA_bio12_1981-2010_V.2.1.tif") 


map.extr <- get.heterogeneity(vector = dt.sf, grid = NULL, id.col = "plot_ID", raster = map)
setnames(map.extr, c("mean"), 
         c("MAP_plot"))
map.extr$geom <- NULL
range(map.extr$MAP)


dt.sf.cov4 <- dt.sf.cov3 %>% 
  left_join(map.extr, by = "plot_ID") 

## Tree cover ---------------
tc <- rast("../../../../resources/spatial/meta_canopy_height/woody_cover_10m_waterberg.tif") 
plot(tc)

tc.ext <- get.heterogeneity(vector = dt.sf, grid = NULL, id.col = "plot_ID", raster = tc)
setnames(tc.ext, c("mean"), 
         c("tree_cover_mean_plot"))
tc.ext$geom <- NULL
range(tc.ext$tree_cover_mean)


## Tree cover reiner ---------
tc.r <- rast("../../../../resources/spatial/Africa_Tree_Cover_Reiner_et_al_2023/South_Africa_treecover_2019_v1_10m.tif") 
plot(tc.r)

tc.r.ext <- get.heterogeneity(vector = dt.sf, grid = NULL, id.col = "plot_ID", raster = tc.r)
setnames(tc.r.ext, c("mean"), 
         c("reiner_tree_cover_plot"))


dt.sf.cov5 <- dt.sf.cov4 %>% 
  left_join(tc.ext, by = "plot_ID") %>% 
  left_join(tc.r.ext)


## Fire frequency ---------------
fir <- rast("../../../../resources/spatial/Fire/FireEventsBetween2001And2024_SA.tif")
#plot(fir)
range(values(fir), na.rm = T)

values(fir) <- ifelse(is.na(values(fir)), 0, values(fir))

fir.extr <- get.heterogeneity(vector = dt.sf, grid = NULL, id.col = "plot_ID", raster = fir)
setnames(fir.extr, c("mean"), 
         c("fire_events_since_2001_plot"))
quantile(fir.extr$fire_events_since_2001)


### Day since last fire

dsf <- rast("../../../../resources/spatial/Fire/DaysSinceLastFireMarch2024_SA.tif")
#plot(fir)
range(values(dsf), na.rm = T)

values(dsf) <- ifelse(values(dsf) == 0, max(values(dsf)) + 1, values(dsf))

dsf.extr <- get.heterogeneity(vector = dt.sf, grid = NULL, id.col = "plot_ID", raster = dsf)
setnames(dsf.extr, c("mean"), 
         c("days_since_last_fire"))
quantile(dsf.extr$days_since_last_fire)


dt.sf.cov6 <- dt.sf.cov5 %>% 
  left_join(fir.extr, by = "plot_ID") %>% 
  left_join(dsf.extr) %>% 
  mutate(reserve = case_when(
    grepl("LA", plot_ID) ~ "Lapalala", 
    grepl("JE", plot_ID) ~ "Jembisa",
    grepl("WI", plot_ID) ~ "Willowisp",
    grepl("SY", plot_ID) ~ "Syringa sands",
    grepl("SU", plot_ID) ~ "Summerplace",
    grepl("DA", plot_ID) ~ "Dabchick",
    grepl("AN", plot_ID) ~ "Ant's Farm",
    grepl("KA", plot_ID) ~ "Kaingo",
    grepl("SW", plot_ID) ~ "Swebeswebe",
    grepl("MA", plot_ID) ~ "Marakele"
  ), 
  site_ID = gsub("_P01", "", plot_ID),
  site_ID = gsub("_P02", "", site_ID),
  site_ID = gsub("_P03", "", site_ID),
  site_ID = gsub("_P04", "", site_ID),
  site_ID = gsub("_P05", "", site_ID))

dt.sf.cov7 <- dt.sf.cov6[, `:=` 
                         (days_since_last_fire_site = mean(days_since_last_fire, na.rm = T), 
                           fire_events_since_2001_site = mean(fire_events_since_2001_plot, na.rm = T), 
                           MAP_site = mean(MAP_plot, na.rm = T),
                           MAT_site = mean(MAT_plot, na.rm = T),
                           elevation_site = mean(elevation_mean_plot, na.rm = T),
                           tree_cover_site = mean(tree_cover_mean_plot, na.rm = T),
                           reiner_tree_cover_site = mean(reiner_tree_cover_plot, na.rm = T),
                           canopy_height_site = mean(canopy_height_mean_plot, na.rm = T)),
                         by = site_ID]

dt.final <- as.data.table(dt.sf.cov7)
dt.final$x <- NULL
dt.final$geom <- NULL

summary(dt.final)
fwrite(x = dt.final, file = "data/processedData/dataFragments/plot_meta_waterberg2024.csv")
