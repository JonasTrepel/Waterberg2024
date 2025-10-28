library(rgee)
library(data.table)
library(tidyverse)
library(googledrive)
library(terra)
library(sf)
#ee_install_upgrade()
ee_Initialize(project = "ee-jonastrepel", drive = TRUE)
drive_auth(email = "jonas.trepel@bio.au.dk")

npp_img <- ee$ImageCollection('MODIS/061/MOD17A3HGF')$
  select('Npp')$
  mean()

Map$addLayer(
  npp_img,
  list(min = 0, max = 65535, palette = c('brown', 'green', 'yellow')),
  'NPP'
)


res <- st_read("data/spatialData/reserveLocations/waterberg2024_reserves.gpkg")

npp_extr <- ee_extract(x = npp_img, y = res, sf = FALSE, fun = ee$Reducer$mean())

mean(npp_extr$Npp)
sd(npp_extr$Npp)

