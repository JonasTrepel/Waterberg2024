## Study Area Map


library(tidyverse)
library(lubridate)
library(ggplot2)
library(MetBrewer)
library(data.table)
library(ggridges)
library(gridExtra)
library(sf)
library(ggspatial)
library(mapview)
library(rnaturalearth)

reserves <- read_sf("data/spatialData/reserveLocations/waterberg2024_reserves.gpkg")
mapview(reserves)


as.character(met.brewer("Archambault", n = 10))
#[1] "#88A0DC" "#5C5698" "#3E1E62" "#63396C" "#905877" "#CE8185" "#DB7B71" "#B6443A" "#C05029" "#E17C29" "#EFA738" "#F9D14A"

p.loc <- ggplot() +
  geom_sf(data = reserves,  aes(fill = reserve)) +
  annotation_scale( location = "bl",bar_cols = c("grey75", "grey25")) +
  scale_fill_manual(values = c("#88A0DC", "#52468A", "#4E2A67", "#7C4B73","#C77D83", "#D6746A", "#AB3329", "#D36929", "#ED9D34", "#F9D14A")) +
  scale_color_met_d(name = "Cassatt2") +
  theme_void() +
  labs(x = "Longitude", y = "Latitude", fill = "Reserve") +
  theme(legend.position = c(0.15, 0.4))
p.loc

ggsave(plot = p.loc, "builds/plots/inkscape/reserveLocations.svg", dpi = 600)


africa <- ne_countries(scale = 50, continent = "Africa") %>% st_transform(crs = 4326)
mapview::mapview(africa)

p.africa <- ggplot() +
  geom_sf(data = africa, fill = "grey95", color = "grey50") + 
  geom_sf(data = reserves, fill = "black" ) +
  # xlim(c(-10, 25)) +
  # ylim(c(37, 65)) +
  theme_void()
p.africa
ggsave(plot = p.africa, "builds/plots/inkscape/africa.png", dpi = 600)


south.africa <- ne_countries(scale = 10, country = "South Africa") %>% st_transform(crs = 4326)
mapview::mapview(south.africa)

p.south.africa <- ggplot() +
  geom_sf(data = south.africa, fill = "grey95", color = "grey50") + 
  geom_sf(data = reserves, fill = "black" ) +
  xlim(c(17, 34)) +
  ylim(c(35, 22)) +
  theme_void()
p.south.africa
ggsave(plot = p.south.africa, "builds/plots/inkscape/southafrica.png", dpi = 600)



