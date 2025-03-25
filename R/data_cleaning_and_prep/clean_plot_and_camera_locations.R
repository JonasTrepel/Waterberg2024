library(sf)
library(data.table)
library(mapview)
library(tidyverse)

sf <- st_read("data/spatial_data/plot_locations/plotLocationsRaw.gpkg") %>% 
  st_transform(crs = 22235)
mapview(sf)

cams <- sf[grepl("ECO", sf$plot_ID), ]

# 
cams <- sf %>%
  st_as_sf() %>%
  filter(grepl("ECO", plot_ID))

mapview(cams %>% filter(plot_ID %in% c("ECO_36", "ECO_40")))


st_write(cams, "data/spatial_data/camera_locations/camera_locations_econovo_waterberg2024.gpkg", append =FALSE)

st_write(cams, "data/spatial_data/camera_locations/camera_locations_econovo_waterberg2024.kml", append =FALSE)

st_write(cams %>% filter(plot_ID %in% c("ECO_36", "ECO_40")), "data/spatial_data/camera_locations/camera_locations_econovo_kaingo_eastern_bypass.gpkg", append =FALSE)



# plots


la.li <- sf[grepl("LA_B", sf$plot_ID), ]

st_write(la.li, "data/spatial_data/plot_locations/lele_lidar_2024_backup.gpkg", append =FALSE)



plots <- sf[!grepl("ECO", sf$plot_ID) & !grepl("LA_B", sf$plot_ID) & !grepl("Car", sf$plot_ID), ]

#clean the mess with the names.
plots[plots$plot_ID == "LA_S01_P01" & plots$photo == "DCIM/JPEG_20240217073142460.JPG", ]$plot_ID <- "LA_S03_P01"
plots[plots$plot_ID == "LA_S04_P01" & plots$photo == "DCIM/JPEG_20240218061937198.JPG", ]$plot_ID <- "LA_S04_P02"
plots[plots$plot_ID == "JE_S02_P01" & plots$photo == "DCIM/JPEG_20240222110115509.JPG", ]$plot_ID <- "JE_S02_P05"
plots[plots$plot_ID == "WI_S05_P05" & plots$photo == "DCIM/JPEG_20240225132037794.JPG", ]$plot_ID <- "WI_S05_P01"
plots[plots$plot_ID == "SY_S02_P02" & plots$photo == "DCIM/JPEG_20240228111346061.JPG", ]$plot_ID <- "SY_S03_P02"
plots[plots$plot_ID == "SY_S03_P03" & plots$photo == "DCIM/JPEG_20240229060127791.JPG", ]$plot_ID <- "SY_S05_P03"
plots[plots$plot_ID == "SU_S1_P03", ]$plot_ID <- "SU_S01_P03"
plots[plots$plot_ID == "SU_S01_P01" & plots$photo == "DCIM/JPEG_20240303051741808.JPG", ]$plot_ID <- "SU_S04_P01"
plots[plots$plot_ID == "DA_S03_P01" & plots$photo == "DCIM/JPEG_20240306121106073.JPG", ]$plot_ID <- "DA_S03_P04"
plots[plots$plot_ID == "AN_S01_P02" & plots$photo == "DCIM/JPEG_20240310061652811.JPG", ]$plot_ID <- "AN_S02_P02"

plots[plots$plot_ID == "SW_S02_S05_ACTUAL", ]$plot_ID <- "SW_S02_P05"

#removes the one duplicated point at SW_S02_P01 which was wrongly named SW_S01_P05
plots <- plots[!plots$photo == "DCIM/JPEG_20240320083744034.JPG", ] %>% 
  rename(plot_notes = notes.)


n_distinct(plots$plot_ID)
names(plots)

#should be fine now. 

st_write(plots, "data/spatial_data/plot_locations/plot_locations_clean_waterberg2024.gpkg", append = FALSE)
st_write(plots, "data/spatial_data/plot_locations/plot_locations_clean_waterberg2024.kml", append = FALSE)


### sites 

sites <- plots %>% 
  filter(grepl("P03", plot_ID)) %>% 
  mutate(site_ID = gsub("_P03", "", plot_ID)) %>% 
  st_buffer(100)

mapview(sites) + mapview(cams)

sites_and_cams <- sites %>% 
  st_join(cams[, "plot_ID"] %>% rename(cameraID = plot_ID)) %>% 
  dplyr::select(site_ID, cameraID, plot_ID) %>% 
  as.data.table() %>% 
  mutate(reserve = case_when(
    grepl("LA", site_ID) ~ "Lapalala", 
    grepl("JE", site_ID) ~ "Jembisa", 
    grepl("WI", site_ID) ~ "Willowisp", 
    grepl("SY", site_ID) ~ "Syringa Sands", 
    grepl("SU", site_ID) ~ "Summerplace", 
    grepl("DA", site_ID) ~ "Dabchick", 
    grepl("AN", site_ID) ~ "Ant's Farm", 
    grepl("KA", site_ID) ~ "Kaingo", 
    grepl("SW", site_ID) ~ "Swebeswebe", 
    grepl("MA", site_ID) ~ "Marakele", 
  ), geometry = NULL,
  cameraID = ifelse(plot_ID == "SY_S03_P03", "ECO_18", cameraID)
  )
table(sites_and_cams$cameraID)

fwrite(sites_and_cams, "data/processed_data/data_fragments/sites_and_cams.csv")

# reserve cameras 

cams.swebe <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_41", "ECO_42", "ECO_43", "ECO_44", "ECO_45"))

mapview(cams.swebe)

st_write(cams.swebe, "data/spatial_data/camera_locations/swebeswebe_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.swebe, "data/spatial_data/camera_locations/swebeswebe_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.mara <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_46", "ECO_47", "ECO_48", "ECO_49", "ECO_50"))

mapview(cams.mara)

t <- st_coordinates(cams.mara) %>% as.data.frame() %>% cbind(cams.mara[, "plot_ID"]) %>% mutate(geometry = NULL)
t

st_write(cams.mara, "data/spatial_data/camera_locations/marakele_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.mara, "data/spatial_data/camera_locations/marakele_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.jemb <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_06", "ECO_07", "ECO_08", "ECO_09", "ECO_10", "ECO_11", "ECO_12", "ECO_13", "ECO_14", "ECO_15"))

mapview(cams.jemb)

st_write(cams.jemb, "data/spatial_data/camera_locations/jembisa_and_willowisp_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.jemb, "data/spatial_data/camera_locations/jembisa_and_willowisp_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.syr <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_16", "ECO_17", "ECO_18", "ECO_19", "ECO_20"))

mapview(cams.syr)

st_write(cams.syr, "data/spatial_data/camera_locations/syringa_sands_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.syr, "data/spatial_data/camera_locations/syringa_sands_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.summer <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_21", "ECO_22", "ECO_23", "ECO_24", "ECO_25"))

mapview(cams.summer)

st_write(cams.summer, "data/spatial_data/camera_locations/summerplace_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.summer, "data/spatial_data/camera_locations/summerplace_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.dabchick <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_26", "ECO_27", "ECO_28", "ECO_29", "ECO_30"))

mapview(cams.dabchick)

st_write(cams.dabchick, "data/spatial_data/camera_locations/dabchick_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.dabchick, "data/spatial_data/camera_locations/dabchick_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.ants <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_31", "ECO_32", "ECO_33", "ECO_34", "ECO_35"))

mapview(cams.ants)

st_write(cams.ants, "data/spatial_data/camera_locations/ants_farm_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.ants, "data/spatial_data/camera_locations/ants_farm_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.kaingo <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_36", "ECO_37", "ECO_38", "ECO_39", "ECO_40"))

mapview(cams.kaingo)

st_write(cams.kaingo, "data/spatial_data/camera_locations/kaingo_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.kaingo, "data/spatial_data/camera_locations/kaingo_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.lapalala <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_01", "ECO_02", "ECO_03", "ECO_04", "ECO_05"))

mapview(cams.lapalala)

st_write(cams.lapalala, "data/spatial_data/camera_locations/lapalala_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.lapalala, "data/spatial_data/camera_locations/lapalala_camera_locations_econovo_waterberg2024.kml", append=FALSE)


