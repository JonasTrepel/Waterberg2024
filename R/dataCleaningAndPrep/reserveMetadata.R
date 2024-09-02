library(data.table)
library(tidyverse)
library(sf)


dt.raw <- fread("data/rawData/reserves_metadata_waterberg2024.csv")
dt <- dt.raw %>% mutate( species = case_when(
               .default = species, 
               species == "Bos taurus africanus" ~ "Bos primigenius", 
               species == "Taurotragus oryx" ~ "Tragelaphus oryx",
               species == "Capra hircus" ~ "Capra caucasica",
               species == "Alcelaphus lichtensteinii" ~ "Alcelaphus buselaphus",
               species == "Felis lybica" ~ "Felis silvestris",
               species == "Tragelaphus sylvaticus" ~ "Tragelaphus imberbis",
               species == "Galerella pulverulenta" ~ "Herpestes pulverulentus",
               species == "Lupulella mesomelas" ~ "Canis mesomelas", 
               species  == "Giraffa giraffa" ~ "Giraffa camelopardalis"))

traits <- fread("data/processedData/dataFragments/mammal_traits.csv")


traits[species == "Antidorcas marsupialis", ]


dt.traits <- dt %>% 
  left_join(traits, by = "species") %>% 
  filter(mass_kg > 10) 


dt.int <- dt.traits %>% pivot_longer(
  cols = c("Lapalala", "Jembisa", "Willowisp", 
           "Syringa Sands", "Summerplace", "Dabchick", "Ant's Farm", "Kaingo", "Swebeswebe", "Marakele"),
  names_to = "reserve", 
  values_to = "Population_Size") 

dt.int[dt.int$reserve == "Ant's Farm", ]

dt.sf <- st_read("data/spatialData/reserveLocations/waterberg2024_reserves.shp")

mapview::mapview(dt.sf)

sf_use_s2(FALSE)
dt.sf2 <- dt.sf %>% 
  mutate(area_ha = as.numeric(st_area(dt.sf)/10000), 
         reserve = ifelse(reserve == "Ants Farm", "Ant's Farm", reserve))


unique(dt.int[dt.int$species == "Ceratotherium simum", ]$Population_Size)

dt.int2 <- dt.int %>% 
  left_join(dt.sf2) %>% 
  filter(mass_kg > 45) %>% 
  mutate(Population_Size = ifelse(is.na(Population_Size), 0, Population_Size), 
         density_ha = Population_Size/area_ha, 
         biomass_ha = density_ha*mass_kg,
         ### rhino biomass not reported for Kaingo and Lapalala, we assign the biomass of Ant's
         biomass_ha = ifelse(species == "Ceratotherium simum" & reserve %in% c("Kaingo", "Lapalala"), 9, biomass_ha),
         metabolic_biomass_ha = density_ha*mr_mass_kg,
         Herbi = ifelse(feeding_type == "Predator", "Predator", "Herbivore"),
         one_per_row = 1,
         geometry = NULL, 
         maximum_prey_size = case_when(
           .default = maximum_prey_size, 
           Herbi == "Predator" & mass_kg < 15 ~ mass_kg,
           Herbi != "Predator" ~ 0, 
           species == "Orycteropus afer" ~ 0.1),
         largest_accessible_prey_size = 
           case_when(
             .default = largest_accessible_prey_size, 
             Herbi == "Predator" & mass_kg < 15 ~ mass_kg,
             Herbi != "Predator" ~ 0, 
             species == "Orycteropus afer" ~ 0.1)
  )  %>% 
  filter(!Population_Size == 0) %>% unique() %>% as.data.table()

mean(dt.int2[species == "Ceratotherium simum" & reserve == "Jembisa",]$biomass_ha)

unique(dt.int2[is.na(dt.int2$maximum_prey_size), ]$species)
unique(dt.int2[dt.int2$reserve == "Ant's Farm", ]$species)


dt.int2[, n_species_reserve := uniqueN(species), by = reserve]
dt.int2[, total_biomass_ha := sum(biomass_ha, na.rm = TRUE), by = reserve]
dt.int2[, total_m_biomass_ha := sum(metabolic_biomass_ha, na.rm = TRUE), by = reserve]



dt.int2[, maximum_prey_size_farm := max(maximum_prey_size, na.rm = TRUE), by = reserve]
quantile(dt.int2$maximum_prey_size_farm)
hist(dt.int2$n_species_reserve)


quantile(dt.int2$largest_accessible_prey_size)
dt.int2[, largest_accessible_prey_size_farm := max(largest_accessible_prey_size), by = reserve]
quantile(dt.int2$largest_accessible_prey_size_farm)

#split in herbivores and predators 

#### carnivores -----
wat.pred <- dt.int2[!Herbi == "Herbivore", ]
wat.pred[, predator_biomass_ha := sum(biomass_ha, na.rm = TRUE), by = reserve]
wat.pred2 <- unique(wat.pred[!is.na(predator_biomass_ha),.(predator_biomass_ha, reserve, species)])


#### herbivores ----
wat.herbi <- dt.int2[Herbi == "Herbivore", ]
wat.herbi[, herbi_biomass_ha := sum(biomass_ha, na.rm = TRUE), by = reserve]
wat.herbi[, m_herbi_biomass_ha := sum(metabolic_biomass_ha, na.rm = TRUE), by = reserve]
wat.herbi[, n_herbi_sp_reserve := uniqueN(species, na.rm = TRUE), by = reserve]



wat.herbi2 <- wat.herbi %>% mutate(
  regular_predator_present = ifelse(mass_kg < largest_accessible_prey_size_farm, 1, 0), 
  occasional_predator_present =  ifelse(mass_kg < maximum_prey_size_farm, 1, 0), 
) %>% filter(!is.na(herbi_biomass_ha))


wat.herbi2[regular_predator_present == 1, biomass_w_regular_predator := sum(biomass_ha), by = reserve]
wat.herbi2.1 <- unique(wat.herbi2[!is.na(biomass_w_regular_predator),.(biomass_w_regular_predator, reserve)])

wat.herbi2[occasional_predator_present == 1, biomass_w_occasional_predator := sum(biomass_ha), by = reserve]
wat.herbi2.2 <- unique(wat.herbi2[!is.na(biomass_w_occasional_predator),.(biomass_w_occasional_predator, reserve)])

wat.herbi2[, species_w_occasional_predator := sum(occasional_predator_present), by = reserve]
wat.herbi2.3 <- unique(wat.herbi2[!is.na(species_w_occasional_predator),.(species_w_occasional_predator, reserve)])

wat.herbi2[, species_w_regular_predator := sum(regular_predator_present), by = reserve]
wat.herbi2.4 <- unique(wat.herbi2[!is.na(species_w_regular_predator),.(species_w_regular_predator, reserve)])

wat.herbi2[, herbis_per_farm := sum(one_per_row), by = reserve]
wat.herbi2.5 <- unique(wat.herbi2[!is.na(herbis_per_farm),.(herbis_per_farm, reserve)])


wat.herbi3 <- wat.herbi %>% 
  left_join(wat.herbi2.1, by = "reserve") %>% 
  left_join(wat.herbi2.2, by = "reserve") %>% 
  left_join(wat.herbi2.3, by = "reserve") %>% 
  left_join(wat.herbi2.4, by = "reserve") %>% 
  left_join(wat.herbi2.5, by = "reserve")

summary(wat.herbi3)

wat.herbi4 <- wat.herbi3 %>% mutate(
  prop_species_w_regular_predator = species_w_regular_predator/herbis_per_farm, 
  prop_species_w_occasional_predator = species_w_occasional_predator/herbis_per_farm, 
  prop_biomass_w_regular_predator = ifelse(is.na(biomass_w_regular_predator), NA, biomass_w_regular_predator/herbi_biomass_ha), 
  prop_biomass_w_occasional_predator = ifelse(is.na(biomass_w_occasional_predator), NA, biomass_w_occasional_predator/herbi_biomass_ha), 
) %>% mutate(
  relative_biomass = biomass_ha/herbi_biomass_ha, 
  relative_mass_kg := mass_kg*relative_biomass) %>%
  dplyr::select(
    c(species, prop_species_w_regular_predator, prop_species_w_occasional_predator,
      prop_biomass_w_regular_predator, prop_biomass_w_occasional_predator,
      herbi_biomass_ha, relative_biomass, relative_mass_kg, m_herbi_biomass_ha,
      n_herbi_sp_reserve,
      reserve)
  ) %>% as.data.table() %>% 
  filter(!reserve == "#N/A") %>% 
  units::drop_units() %>% 
  unique()

summary(wat.herbi4)

names(dt.int2)
#bind together 

nrow(dt.int2)
nrow(wat.herbi4)
rel.herbi.biomass <- wat.herbi4 %>% dplyr::select(relative_biomass, relative_mass_kg, species, reserve)

wat.herbi5 <- wat.herbi4 %>% dplyr::select(-c(relative_biomass, relative_mass_kg, species)) %>% unique()

dt.int3 <- left_join(dt.int2, wat.herbi4)
dt.int3.5 <- left_join(dt.int3, rel.herbi.biomass)
dt.int4 <- left_join(dt.int3.5, wat.pred2)

unique(dt.int4[dt.int2$reserve == "Ant's Farm", ]$species)



dt.int4[, `:=` (max_species_body_mass = max(mass_kg, na.rm = TRUE),
                mean_species_body_mass = mean(mass_kg, na.rm = TRUE),
                median_species_body_mass = median(mass_kg, na.rm = TRUE),
                CW_max_species_body_mass = max(relative_mass_kg, na.rm = TRUE),
                CW_mean_species_body_mass = mean(relative_mass_kg, na.rm = TRUE),
                CW_median_species_body_mass = median(relative_mass_kg, na.rm = TRUE),
                herbivore_fg_own = as.numeric(n_distinct(herbivore_fg_own, na.rm = TRUE)),
                carnivore_fg_own = n_distinct(carnivore_fg_own, na.rm = TRUE),
                hempson_fg = n_distinct(hempson_herbivore_group, na.rm = TRUE),
                trophic_groups = n_distinct(trophic_level, na.rm = TRUE),
                trophic_groups_hempson = n_distinct(trophic_level_hempson, na.rm = TRUE),
                carnivore_fg_own_simple = n_distinct(trophic_level_carni, na.rm = TRUE),
                herbivore_fg_own_simple = n_distinct(trophic_level_herbivore, na.rm = TRUE)), by = reserve]

wat.browsers <- dt.int4[guild_only_herbivory == "Browser"]
wat.browsers[, browser_biomass_ha := sum(biomass_ha), by = reserve ]
wat.browsers <- wat.browsers[!is.na(wat.browsers$browser_biomass_ha),.(browser_biomass_ha, reserve)]
wat.browsers <- unique(wat.browsers)

wat.grazers <- dt.int4[guild_only_herbivory == "Grazer"]
wat.grazers[, grazer_biomass_ha := sum(biomass_ha), by = reserve ]
wat.grazers <- wat.grazers[!is.na(wat.grazers$grazer_biomass_ha),.(grazer_biomass_ha, reserve)]
wat.grazers <- unique(wat.grazers)


wat.mf <- dt.int4[guild_only_herbivory == "Mixed Feeder"]
wat.mf[, mixed_feeder_biomass_ha := sum(biomass_ha), by = reserve ]
wat.mf <- wat.mf[!is.na(wat.mf$mixed_feeder_biomass_ha),.(mixed_feeder_biomass_ha, reserve)]
quantile(wat.mf$mixed_feeder_biomass_ha, na.rm = TRUE)
sum(is.na(wat.mf$mixed_feeder_biomass_ha))
wat.mf <- unique(wat.mf)



dt.int5 <- dt.int4 %>% 
  left_join(wat.browsers) %>% 
  left_join(wat.grazers) %>% 
  left_join(wat.mf) %>% 
  mutate(
    grazer_biomass_ha = ifelse(!is.na(grazer_biomass_ha), grazer_biomass_ha, 0), 
    browser_biomass_ha = ifelse(!is.na(browser_biomass_ha), browser_biomass_ha, 0), 
    mixed_feeder_biomass_ha = ifelse(!is.na(mixed_feeder_biomass_ha), mixed_feeder_biomass_ha, 0)
  ) %>%
  mutate(grazer_browser_ratio = ifelse(grazer_biomass_ha == 0 | browser_biomass_ha == 0, NA, grazer_biomass_ha/browser_biomass_ha),
         grazer_mixed_ratio = ifelse(grazer_biomass_ha == 0 | mixed_feeder_biomass_ha == 0, NA, grazer_biomass_ha/mixed_feeder_biomass_ha),
         browser_mixed_ratio = ifelse(mixed_feeder_biomass_ha == 0 & browser_biomass_ha == 0, NA, browser_biomass_ha/mixed_feeder_biomass_ha))

hist(dt.int5$grazer_browser_ratio)

glimpse(dt.int5)

### add functional diversity ------------------------
library(mFD)

### categorical traits have to be coded as factor 
mam.traits2 <- traits[] %>% 
  mutate(feeding_type = as.factor(feeding_type), 
         fermentation_type = as.factor(fermentation_type), 
         herbi_body_mass_bins = as.factor(herbi_body_mass_bins))


### assign low biomass (i.e., weight) to the species for which we have no data 
### and subset to herbivores only 
herbis <- dt.int5[,] %>% 
  filter(Herbi == "Herbivore") %>% ## subset to only herbivores 
  dplyr::select(species, biomass_ha, density_ha, reserve) %>% 
  mutate(biomass_ha = ifelse(is.na(biomass_ha), 0.1, biomass_ha),
         density_ha = ifelse(is.na(density_ha), 0.1, density_ha))


### build trait data frame 
sp_tr <-  mam.traits2 %>% dplyr::select(species, feeding_type, fermentation_type, herbi_body_mass_bins) %>% 
  remove_rownames %>% 
  column_to_rownames(var="species")

### build trait categories 
tr_cat <- data.table(
  trait_name = c("feeding_type", "fermentation_type", "herbi_body_mass_bins"), 
  trait_type = c("N", "N", "N"), 
  fuzzy_name = NA
)

### compute functional entities 
fe <- mFD::sp.to.fe(sp_tr = sp_tr, tr_cat = tr_cat)

#### let's try with the gower distance for now 

tr_cat_fdist <- tr_cat
tr_cat_fdist$trait_weight <- c(1, 1, 2) ## weigh body mass double because it's so important
fdist <- funct.dist(sp_tr = sp_tr, tr_cat = tr_cat_fdist, metric = "gower")


###          all herbis             ###

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight.mat <- herbis %>% 
  dplyr::select(reserve, biomass_ha, species) %>% 
  pivot_wider(names_from = "species", values_from = "biomass_ha") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("reserve"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="reserve") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ <- mFD::asb.sp.summary(asb_sp_w = weight.mat) 
asb_sp_occ <- asb_sp_summ$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res <- alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd <- alpha.fd.res$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")


### plot for example reserve 
alpha.fd.fe.plot(alpha.fd.res, plot_asb_nm = "Lapalala")
herbis

## distance based functional diversity metrics. 


#### distance based functional diversity metric 
?alpha.fd.hill
fd.hill.res <- alpha.fd.hill(asb_sp_w = weight.mat, sp_dist = fdist)

## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)

## extract functional diversity metrics 
dt.dbfd <- fd.hill.res$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")

dt.fd <- dt.dbfd %>% left_join(dt.sfd) %>% 
  as.data.table() %>% 
  rename(herbi_fun_ovred = fored,
         herbi_fun_red = fred,
         herbi_fun_div_distq1 = FD_q1, 
         herbi_fun_ent = nb_fe) %>% 
  dplyr::select(herbi_fun_ovred, herbi_fun_red, herbi_fun_div_distq1, herbi_fun_ent, reserve)


dt.int5.5 <- dt.int5 %>% 
  filter(!is.na(herbi_biomass_ha)) %>% 
  left_join(dt.fd) %>% 
  dplyr::select(reserve, max_species_body_mass, mean_species_body_mass, median_species_body_mass, 
               CW_max_species_body_mass, CW_mean_species_body_mass, CW_median_species_body_mass, 
               herbi_fun_red, herbi_fun_div_distq1, herbi_fun_ent, n_species_reserve,
               n_herbi_sp_reserve, 
               browser_biomass_ha, grazer_biomass_ha, mixed_feeder_biomass_ha,
               grazer_browser_ratio, grazer_mixed_ratio, browser_mixed_ratio,
               herbi_biomass_ha, prop_biomass_w_occasional_predator, prop_biomass_w_regular_predator,
               prop_species_w_occasional_predator, prop_species_w_regular_predator,
               predator_biomass_ha, area_ha) %>% 
  unique()

####### grazer and mixed feeder only #####

grazer <- dt.int5[feeding_type %in% c("Grazer", "Mixed Feeder"),] %>% 
  filter(Herbi == "Herbivore") %>% ## subset to only herbivores 
  dplyr::select(species, biomass_ha, density_ha, reserve) %>% 
  mutate(biomass_ha = ifelse(is.na(biomass_ha), 0.1, biomass_ha),
         density_ha = ifelse(is.na(density_ha), 0.1, density_ha))

weight.mat.grazer <- grazer[] %>% 
  dplyr::select(reserve, biomass_ha, species) %>% 
  pivot_wider(names_from = "species", values_from = "biomass_ha") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("reserve"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="reserve") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ.grazer <- mFD::asb.sp.summary(asb_sp_w = weight.mat.grazer) 
asb_sp_occ.grazer <- asb_sp_summ.grazer$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res.grazer <- alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd.grazer <- alpha.fd.res.grazer$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")


### plot for example reserve 
alpha.fd.fe.plot(alpha.fd.res.grazer, plot_asb_nm = "Lapalala")


#### distance based functional diversity metric 
fd.hill.res.grazer <- alpha.fd.hill(asb_sp_w = weight.mat.grazer, sp_dist = fdist)

## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)

## extract functional diversity metrics 
dt.dbfd.grazer <- fd.hill.res.grazer$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")

dt.fd.grazer <- dt.dbfd.grazer %>% left_join(dt.sfd.grazer) %>% 
  as.data.table() %>% 
  dplyr::select(reserve, FD_q1, nb_fe, nb_sp, fred, fored, fvuln)

names(dt.fd.grazer) <- ifelse(names(dt.fd.grazer) == "reserve", paste0(names(dt.fd.grazer)), paste0("grazer_mf_", names(dt.fd.grazer)))

####### browser and mixed feeder only #####

browser <- dt.int5[feeding_type %in% c("Browser", "Mixed Feeder"),] %>% 
  filter(Herbi == "Herbivore") %>% ## subset to only herbivores 
  dplyr::select(species, biomass_ha, density_ha, reserve) %>% 
  mutate(biomass_ha = ifelse(is.na(biomass_ha), 0.1, biomass_ha),
         density_ha = ifelse(is.na(density_ha), 0.1, density_ha))

weight.mat.browser <- browser[] %>% 
  dplyr::select(reserve, biomass_ha, species) %>% 
  pivot_wider(names_from = "species", values_from = "biomass_ha") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("reserve"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="reserve") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ.browser <- mFD::asb.sp.summary(asb_sp_w = weight.mat.browser) 
asb_sp_occ.browser <- asb_sp_summ.browser$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res.browser <- alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd.browser <- alpha.fd.res.browser$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")


### plot for example reserve 
alpha.fd.fe.plot(alpha.fd.res.browser, plot_asb_nm = "Lapalala")


#### distance based functional diversity metric 
fd.hill.res.browser <- alpha.fd.hill(asb_sp_w = weight.mat.browser, sp_dist = fdist)

## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)

## extract functional diversity metrics 
dt.dbfd.browser <- fd.hill.res.browser$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")

dt.fd.browser <- dt.dbfd.browser %>% left_join(dt.sfd.browser) %>% 
  as.data.table() %>% 
  dplyr::select(reserve, FD_q1, nb_fe, nb_sp, fred, fored, fvuln)

names(dt.fd.browser) <- ifelse(names(dt.fd.browser) == "reserve", paste0(names(dt.fd.browser)), paste0("browser_mf_", names(dt.fd.browser)))


#### combine ####

dt.int6 <- dt.int5.5 %>% 
  left_join(dt.fd.browser) %>% 
  left_join(dt.fd.grazer)


############# EXTRACT COVS -------------------

library(mapview)
mapview(dt.sf)

source("R/functions/get.heterogeneity.R")

dt.sf <- st_transform(dt.sf, crs = 4326)
dt.utm <- st_transform(dt.sf, crs = 22235)


grid1000 <- st_make_grid(dt.utm, cellsize = c(1000, 1000), what = "polygons", square = TRUE) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  st_join(dt.sf) %>% 
  filter(!is.na(reserve)) 

grid1000 <- grid1000 %>%
  mutate(grid_ID = paste0("Grid_", 1:nrow(grid1000))) %>% dplyr::select(c("grid_ID"))


### Canopy heigth (meta) -------------------------------
library(terra) 

ch <- rast("../../../../resources/spatial/meta_canopy_height/canopy_height_10m_waterberg.tif")
res(ch)

g.1000 <- get.heterogeneity(vector = dt.sf, grid = grid1000, id.col = "reserve", raster = ch)
setnames(g.1000, c("id.col", "grid.cv", "grid.sd", "grid.mean", "mean"), 
         c("reserve", "canopy_height_cv_1000", "canopy_height_sd_1000", "canopy_height_mean_1000", "canopy_height_mean"))
g.1000$geometry <- NULL
g.1000$x <- NULL



dt.sf.cov1 <- dt.sf %>% 
  left_join(g.1000, by = "reserve") %>%  
  as.data.table() %>% 
  mutate(geometry = NULL, x = NULL) %>%
  unique() 



## elevation --------

ele <- rast("../../../../resources/spatial/Elevation_ZAF/Elevation_SA_90m.tif")
crs(ele) # 
res(ele)

ele.extr <- get.heterogeneity(vector = dt.sf, grid = grid1000, id.col = "reserve", raster = ele)

ele.extr$x <- NULL


setnames(ele.extr, c("id.col", "grid.cv", "grid.sd", "grid.mean", "mean"), 
         c("reserve", "elevation_cv_1000", "elevation_sd_1000", "elevation_mean_1000", "elevation_mean"))

dt.sf.cov2 <- dt.sf.cov1 %>% 
  left_join(ele.extr, by = "reserve") %>% dplyr::select(-elevation_mean_1000)

## MAT---------------

mat <- rast("../../../../resources/spatial/Chelsa_Climate/CHELSA_bio1_1981-2010_V.2.1.tif") 


mat.extr <- get.heterogeneity(vector = dt.sf, grid = NULL, id.col = "reserve", raster = mat)
setnames(mat.extr, c("mean"), 
         c("MAT"))


dt.sf.cov3 <- dt.sf.cov2 %>% 
  left_join(mat.extr, by = "reserve") 


## MAP---------------
map <- rast("../../../../resources/spatial/Chelsa_Climate/CHELSA_bio12_1981-2010_V.2.1.tif") 


map.extr <- get.heterogeneity(vector = dt.sf, grid = NULL, id.col = "reserve", raster = map)
setnames(map.extr, c("mean"), 
         c("MAP"))

dt.sf.cov4 <- dt.sf.cov3 %>% 
  left_join(map.extr, by = "reserve") 

## Tree cover ---------------
tc <- rast("../../../../resources/spatial/meta_canopy_height/woody_cover_10m_waterberg.tif") 

tc.g.1000 <- get.heterogeneity(vector = dt.sf, grid = grid1000, id.col = "reserve", raster = tc)
setnames(tc.g.1000, c("id.col", "grid.cv", "grid.sd", "grid.mean", "mean"), 
         c("reserve", "tree_cover_cv_1000", "tree_cover_sd_1000", "tree_cover_mean_1000", "tree_cover_mean"))
tc.g.1000$x <- NULL


## Tree cover reiner ---------
tc.r <- rast("../../../../resources/spatial/Africa_Tree_Cover_Reiner_et_al_2023/South_Africa_treecover_2019_v1_10m.tif") 
plot(tc.r)

tc.r.ext <- get.heterogeneity(vector = dt.sf, grid = NULL, id.col = "reserve", raster = tc.r)
setnames(tc.r.ext, c("mean"), 
         c("reiner_tree_cover_reserve"))


dt.sf.cov5 <- dt.sf.cov4 %>% 
  left_join(tc.g.1000, by = "reserve") %>% 
  left_join(tc.r.ext)


## Fire frequency ---------------
fir <- rast("../../../../resources/spatial/Fire/FireEventsBetween2001And2024_SA.tif")
#plot(fir)
range(values(fir), na.rm = T)


values(fir) <- ifelse(is.na(values(fir)), 0, values(fir))

fir.extr <- get.heterogeneity(vector = dt.sf, grid = NULL, id.col = "reserve", raster = fir)
setnames(fir.extr, c("mean"), 
         c("fire_events_since_2001"))

dt.sf.cov6 <- dt.sf.cov5 %>% 
  left_join(fir.extr, by = "reserve")



dt.sf.cov7 <- dt.sf.cov6 %>% 
  mutate(reserve = ifelse(reserve == "Ants Farm", "Ant's Farm", reserve)) %>% 
  left_join(dt.int6)




dt.final <- as.data.table(dt.sf.cov7)
dt.final$x <- NULL


fwrite(x = dt.final, file = "data/processedData/dataFragments/reserve_meta_waterberg2024.csv")

