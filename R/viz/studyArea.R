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
library(scico)

reserves <- read_sf("data/spatialData/reserveLocations/waterberg2024_reserves.gpkg")
wbr <- read_sf("data/spatialData/randomShapefiles/WaterbergBiosphereReserve/WDPA_WDOECM_Oct2024_Public_900554_shp-polygons.shp")
mapview(wbr) +mapview(reserves)



as.character(met.brewer("Archambault", n = 10))
#[1] "#88A0DC" "#5C5698" "#3E1E62" "#63396C" "#905877" "#CE8185" "#DB7B71" "#B6443A" "#C05029" "#E17C29" "#EFA738" "#F9D14A"

p.loc <- ggplot() +
  geom_sf(data = wbr, color = NA) +
  geom_sf(data = reserves,  aes(fill = reserve)) +
  annotation_scale(location = "bl",bar_cols = c("grey75", "grey25")) +
  scale_fill_manual(values = c("Ants Farm" = "#011959",
                               "Dabchick" = "#FACCFA",
                               "Jembisa" = "#828231",
                               "Kaingo" = "#226061",
                               "Lapalala" = "#F19D6B",
                               "Marakele" = "#114360" ,
                               "Summerplace" = "#FDB4B4",
                               "Swebeswebe" = "#4D734D",
                               "Syringa Sands" = "#C09036",
                               "Willowisp" = "#677B3E")) +
 # scale_fill_scico_d(palette = "nuuk") +
 # scale_color_met_d(name = "Cassatt2") +
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
  geom_sf(data = wbr, fill = "grey80" ) +
  geom_sf(data = reserves, fill = "black" ) +
  xlim(c(17, 34)) +
  ylim(c(35, 22)) +
  theme_void()
p.south.africa
ggsave(plot = p.south.africa, "builds/plots/inkscape/southafrica.png", dpi = 600)

##########################################################

dt <- fread("data/processedData/cleanData/waterberg2024DataPrelim.csv") %>% 
  rename(species_per_reserve = total_plant_species_richness_reserve, 
         species_per_site = total_plant_species_richness_site, 
         reserve_mean_beta_divq1 = mean_beta_divq1) 


dt %>% filter(grepl("P03", plot_ID)) %>% ggplot() +geom_point(aes(x = tree_cover_mean_plot, y = nEventsDayReserve))
library(ggridges)

p1 <- dt %>% 
  ggplot() +
  geom_point(aes(x = herbi_biomass_ha, y = n_herbi_sp_reserve, color = reserve), size = 5) +
  scale_color_manual(values = c("Ant's Farm" = "#011959",
                               "Dabchick" = "#FACCFA",
                               "Jembisa" = "#828231",
                               "Kaingo" = "#226061",
                               "Lapalala" = "#F19D6B",
                               "Marakele" = "#114360" ,
                               "Summerplace" = "#FDB4B4",
                               "Swebeswebe" = "#4D734D",
                               "Syringa Sands" = "#C09036",
                               "Willowisp" = "#677B3E")) +
  labs(x = "Herbivore Biomass (kg/ha)", y = "Herbivore Species Richness") +
  theme_classic() +
  theme(legend.position = "none")
p1

p2 <- dt %>% 
  rename("Plant Species Richness\nPlot Scale" = ) %>% 
  pivot_longer(cols = c("species_per_site", "graminoids_per_site", "forbs_per_site", "woodies_per_site"), 
               names_to = "VarName", values_to = "VarValue") %>% 
  mutate(VarName = case_when(
    VarName == "species_per_site" ~ "Plant Species Richness", 
    VarName == "graminoids_per_site" ~ "Graminoid Richness",
    VarName == "forbs_per_site" ~ "Forb Richness",
    VarName == "woodies_per_site" ~ "Woody Species Richness"
  )) %>% 
  ggplot() +
  geom_density_ridges(aes(x = VarValue, y = reserve, fill = reserve), alpha = 0.9) +
  facet_wrap(~VarName, scales = "free_x", ncol = 4) +
  scale_fill_manual(values = c("Ant's Farm" = "#011959",
                                "Dabchick" = "#FACCFA",
                                "Jembisa" = "#828231",
                                "Kaingo" = "#226061",
                                "Lapalala" = "#F19D6B",
                                "Marakele" = "#114360" ,
                                "Summerplace" = "#FDB4B4",
                                "Swebeswebe" = "#4D734D",
                                "Syringa Sands" = "#C09036",
                                "Willowisp" = "#677B3E")) +
  labs(x = "", y = "") +
  theme_bw() + 
  theme(legend.position = "none", 
                       legend.box="vertical",
                       legend.margin=margin(),
                       legend.text = element_text(size = 12),
                       plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                       panel.grid = element_blank(), 
                       axis.title.x = element_blank(), 
                       axis.text = element_text(size = 10), 
                       panel.border = element_rect(color = NA), 
                       panel.background = element_rect(fill = "snow"), 
                       strip.text.x = element_text(size = 12), 
                       strip.text.y = element_text(size = 12, face = "bold"), 
                       strip.background = element_rect(color = "grey85"),
  ) 

p2

library(gridExtra)
p3 <- grid.arrange(p2, p1, widths = c(4, 1.3))

ggsave(plot = p3, "builds/plots/inkscape/mapAddOns.png", dpi = 600, height = 3, width = 12)

names(dt)

