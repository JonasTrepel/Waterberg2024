library(sf)
library(data.table)
library(mapview)
library(tidyverse)

sf <- st_read("data/spatialData/plotLocations/plotLocationsRaw.gpkg")
mapview(sf)

cams <- sf[grepl("ECO", sf$plot_ID), ]

# 
cams <- sf %>%
  st_as_sf() %>%
  filter(grepl("ECO", plot_ID))

mapview(cams)


st_write(cams, "data/spatialData/cameraLocations/camera_locations_econovo_waterberg2024.gpkg", append =FALSE)

st_write(cams, "data/spatialData/cameraLocations/camera_locations_econovo_waterberg2024.kml", append =FALSE)


# plots


la.li <- sf[grepl("LA_B", sf$plot_ID), ]

st_write(la.li, "data/spatialData/plotLocations/lele_lidar_2024_backup.gpkg", append =FALSE)



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
plots <- plots[!plots$photo == "DCIM/JPEG_20240320083744034.JPG", ]


n_distinct(plots$plot_ID)
names(plots)

#should be fine now. 

st_write(plots, "data/spatialData/plotLocations/plot_locations_clean_waterberg2024.gpkg", append = FALSE)
st_write(plots, "data/spatialData/plotLocations/plot_locations_clean_waterberg2024.kml", append = FALSE)


# reserve cameras 

cams.swebe <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_41", "ECO_42", "ECO_43", "ECO_44", "ECO_45"))

mapview(cams.swebe)

st_write(cams.swebe, "data/spatialData/cameraLocations/swebeswebe_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.swebe, "data/spatialData/cameraLocations/swebeswebe_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.mara <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_46", "ECO_47", "ECO_48", "ECO_49", "ECO_50"))

mapview(cams.mara)

t <- st_coordinates(cams.mara) %>% as.data.frame() %>% cbind(cams.mara[, "plot_ID"]) %>% mutate(geometry = NULL)
t

st_write(cams.mara, "data/spatialData/cameraLocations/marakele_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.mara, "data/spatialData/cameraLocations/marakele_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.jemb <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_06", "ECO_07", "ECO_08", "ECO_09", "ECO_10", "ECO_11", "ECO_12", "ECO_13", "ECO_14", "ECO_15"))

mapview(cams.jemb)

st_write(cams.jemb, "data/spatialData/cameraLocations/jembisa_and_willowisp_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.jemb, "data/spatialData/cameraLocations/jembisa_and_willowisp_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.syr <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_16", "ECO_17", "ECO_18", "ECO_19", "ECO_20"))

mapview(cams.syr)

st_write(cams.syr, "data/spatialData/cameraLocations/syringa_sands_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.syr, "data/spatialData/cameraLocations/syringa_sands_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.summer <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_21", "ECO_22", "ECO_23", "ECO_24", "ECO_25"))

mapview(cams.summer)

st_write(cams.summer, "data/spatialData/cameraLocations/summerplace_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.summer, "data/spatialData/cameraLocations/summerplace_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.dabchick <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_26", "ECO_27", "ECO_28", "ECO_29", "ECO_30"))

mapview(cams.dabchick)

st_write(cams.dabchick, "data/spatialData/cameraLocations/dabchick_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.dabchick, "data/spatialData/cameraLocations/dabchick_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.ants <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_31", "ECO_32", "ECO_33", "ECO_34", "ECO_35"))

mapview(cams.ants)

st_write(cams.ants, "data/spatialData/cameraLocations/ants_farm_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.ants, "data/spatialData/cameraLocations/ants_farm_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.kaingo <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_36", "ECO_37", "ECO_38", "ECO_39", "ECO_40"))

mapview(cams.kaingo)

st_write(cams.kaingo, "data/spatialData/cameraLocations/kaingo_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.kaingo, "data/spatialData/cameraLocations/kaingo_camera_locations_econovo_waterberg2024.kml", append=FALSE)


cams.lapalala <- sf %>%
  st_as_sf() %>%
  filter(plot_ID %in% c("ECO_01", "ECO_02", "ECO_03", "ECO_04", "ECO_05"))

mapview(cams.lapalala)

st_write(cams.lapalala, "data/spatialData/cameraLocations/lapalala_camera_locations_econovo_waterberg2024.gpkg", append=FALSE)
st_write(cams.lapalala, "data/spatialData/cameraLocations/lapalala_camera_locations_econovo_waterberg2024.kml", append=FALSE)


