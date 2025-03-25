library(data.table)
library(tidyverse)
library(sf)


dt_raw <- fread("data/raw_data/reserves_metadata_waterberg2024.csv")
dt <- dt_raw %>% mutate( species = case_when(
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

traits <- fread("data/processed_data/data_fragments/mammal_traits.csv")




dt_traits <- dt %>% 
  left_join(traits, by = "species") %>% 
  filter(mass_kg > 10) 


dt_int <- dt_traits %>% pivot_longer(
  cols = c("Lapalala", "Jembisa", "Willowisp", 
           "Syringa Sands", "Summerplace", "Dabchick", "Ant's Farm", "Kaingo", "Swebeswebe", "Marakele"),
  names_to = "reserve", 
  values_to = "Population_Size") 

dt_int[dt_int$reserve == "Ant's Farm", ]

dt_sf <- st_read("data/spatial_data/reserve_locations/waterberg2024_reserves.shp")

mapview::mapview(dt_sf)

sf_use_s2(FALSE)
dt_sf2 <- dt_sf %>% 
  mutate(area_ha = as.numeric(st_area(dt_sf)/10000), 
         reserve = ifelse(reserve == "Ants Farm", "Ant's Farm", reserve))


unique(dt_int[dt_int$species == "Ceratotherium simum", ]$Population_Size)

n_distinct(dt_int$species)
n_distinct(dt_int[dt_int$mass_kg > 10 & !dt_int$feeding_type == "Predator", ]$species)
n_distinct(dt_int[dt_int$mass_kg > 45 & !dt_int$feeding_type == "Predator", ]$species)

unique(dt_int$species_common)

dt_int2 <- dt_int %>% 
  left_join(dt_sf2) %>% 
  filter(!species_common == "Baboon troops") %>% 
  filter(mass_kg >= 45) %>% 
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

mean(dt_int2[species == "Ceratotherium simum" & reserve == "Jembisa",]$biomass_ha)

unique(dt_int2[is.na(dt_int2$maximum_prey_size), ]$species)
unique(dt_int2[dt_int2$reserve == "Ant's Farm", ]$species)


dt_int2[, n_species_reserve := uniqueN(species), by = reserve]
dt_int2[, total_biomass_ha := sum(biomass_ha, na.rm = TRUE), by = reserve]
dt_int2[, total_m_biomass_ha := sum(metabolic_biomass_ha, na.rm = TRUE), by = reserve]



dt_int2[, maximum_prey_size_farm := max(maximum_prey_size, na.rm = TRUE), by = reserve]
quantile(dt_int2$maximum_prey_size_farm)
hist(dt_int2$n_species_reserve)


quantile(dt_int2$largest_accessible_prey_size)
dt_int2[, largest_accessible_prey_size_farm := max(largest_accessible_prey_size), by = reserve]
quantile(dt_int2$largest_accessible_prey_size_farm)

#split in herbivores and predators 

#### carnivores -----
wat_pred <- dt_int2[!Herbi == "Herbivore", ]
wat_pred[, predator_biomass_ha := sum(biomass_ha, na.rm = TRUE), by = reserve]
wat_pred2 <- unique(wat_pred[!is.na(predator_biomass_ha),.(predator_biomass_ha, reserve)])


#### herbivores ----
wat_herbi <- dt_int2[Herbi == "Herbivore", ]
wat_herbi[, herbivore_biomass_kg_ha := sum(biomass_ha, na.rm = TRUE), by = reserve]
wat_herbi[, m_herbivore_biomass_kg_ha := sum(metabolic_biomass_ha, na.rm = TRUE), by = reserve]
wat_herbi[, herbivore_species_richness := uniqueN(species, na.rm = TRUE), by = reserve]



wat_herbi2 <- wat_herbi %>% mutate(
  regular_predator_present = ifelse(mass_kg < largest_accessible_prey_size_farm, 1, 0), 
  occasional_predator_present =  ifelse(mass_kg < maximum_prey_size_farm, 1, 0), 
) %>% filter(!is.na(herbivore_biomass_kg_ha))


wat_herbi2[regular_predator_present == 1, biomass_w_regular_predator := sum(biomass_ha), by = reserve]
wat_herbi2.1 <- unique(wat_herbi2[!is.na(biomass_w_regular_predator),.(biomass_w_regular_predator, reserve)])

wat_herbi2[occasional_predator_present == 1, biomass_w_occasional_predator := sum(biomass_ha), by = reserve]
wat_herbi2.2 <- unique(wat_herbi2[!is.na(biomass_w_occasional_predator),.(biomass_w_occasional_predator, reserve)])

wat_herbi2[, species_w_occasional_predator := sum(occasional_predator_present), by = reserve]
wat_herbi2.3 <- unique(wat_herbi2[!is.na(species_w_occasional_predator),.(species_w_occasional_predator, reserve)])

wat_herbi2[, species_w_regular_predator := sum(regular_predator_present), by = reserve]
wat_herbi2.4 <- unique(wat_herbi2[!is.na(species_w_regular_predator),.(species_w_regular_predator, reserve)])

wat_herbi2[, herbis_per_farm := sum(one_per_row), by = reserve]
wat_herbi2.5 <- unique(wat_herbi2[!is.na(herbis_per_farm),.(herbis_per_farm, reserve)])


wat_herbi3 <- wat_herbi %>% 
  left_join(wat_herbi2.1, by = "reserve") %>% 
  left_join(wat_herbi2.2, by = "reserve") %>% 
  left_join(wat_herbi2.3, by = "reserve") %>% 
  left_join(wat_herbi2.4, by = "reserve") %>% 
  left_join(wat_herbi2.5, by = "reserve")

summary(wat_herbi3)

wat_herbi4 <- wat_herbi3 %>% mutate(
  prop_species_w_regular_predator = species_w_regular_predator/herbis_per_farm, 
  prop_species_w_occasional_predator = species_w_occasional_predator/herbis_per_farm, 
  prop_biomass_w_regular_predator = ifelse(is.na(biomass_w_regular_predator), NA, biomass_w_regular_predator/herbivore_biomass_kg_ha), 
  prop_biomass_w_occasional_predator = ifelse(is.na(biomass_w_occasional_predator), NA, biomass_w_occasional_predator/herbivore_biomass_kg_ha), 
) %>% mutate(
  relative_biomass = biomass_ha/herbivore_biomass_kg_ha, 
  relative_mass_kg := mass_kg*relative_biomass) %>%
  dplyr::select(
    c(species, prop_species_w_regular_predator, prop_species_w_occasional_predator,
      prop_biomass_w_regular_predator, prop_biomass_w_occasional_predator,
      herbivore_biomass_kg_ha, relative_biomass, relative_mass_kg, m_herbivore_biomass_kg_ha,
      herbivore_species_richness,
      reserve)
  ) %>% as.data.table() %>% 
  filter(!reserve == "#N/A") %>% 
  units::drop_units() %>% 
  unique()

summary(wat_herbi4)

names(dt_int2)
#bind together 

nrow(dt_int2)
nrow(wat_herbi4)
rel_herbivore_biomass_kg_ha <- wat_herbi4 %>% dplyr::select(relative_biomass, relative_mass_kg, species, reserve)

wat_herbi5 <- wat_herbi4 %>% dplyr::select(-c(relative_biomass, relative_mass_kg, species)) %>% unique()

dt_int3 <- left_join(dt_int2, wat_herbi4)
dt_int3.5 <- left_join(dt_int3, rel_herbivore_biomass_kg_ha)
dt_int4 <- left_join(dt_int3.5, wat_pred2)

unique(dt_int4[dt_int2$reserve == "Ant's Farm", ]$species)



dt_int4[, `:=` (max_species_body_mass = max(mass_kg, na.rm = TRUE),
                mean_species_body_mass = mean(mass_kg, na.rm = TRUE),
                median_species_body_mass = median(mass_kg, na.rm = TRUE),
                cw_max_species_body_mass = max(relative_mass_kg, na.rm = TRUE),
                cw_mean_species_body_mass = mean(relative_mass_kg, na.rm = TRUE),
                cw_median_species_body_mass = median(relative_mass_kg, na.rm = TRUE),
                herbivore_fg_own = as.numeric(n_distinct(herbivore_fg_own, na.rm = TRUE)),
                carnivore_fg_own = n_distinct(carnivore_fg_own, na.rm = TRUE),
                hempson_fg = n_distinct(hempson_herbivore_group, na.rm = TRUE),
                trophic_groups = n_distinct(trophic_level, na.rm = TRUE),
                trophic_groups_hempson = n_distinct(trophic_level_hempson, na.rm = TRUE),
                carnivore_fg_own_simple = n_distinct(trophic_level_carni, na.rm = TRUE),
                herbivore_fg_own_simple = n_distinct(trophic_level_herbivore, na.rm = TRUE)), by = reserve]

wat_browsers <- dt_int4[guild_only_herbivory == "Browser"]
wat_browsers[, browser_biomass_ha := sum(biomass_ha), by = reserve ]
wat_browsers <- wat_browsers[!is.na(wat_browsers$browser_biomass_ha),.(browser_biomass_ha, reserve)]
wat_browsers <- unique(wat_browsers)

wat_grazers <- dt_int4[guild_only_herbivory == "Grazer"]
wat_grazers[, grazer_biomass_ha := sum(biomass_ha), by = reserve ]
wat_grazers <- wat_grazers[!is.na(wat_grazers$grazer_biomass_ha),.(grazer_biomass_ha, reserve)]
wat_grazers <- unique(wat_grazers)


wat_mf <- dt_int4[guild_only_herbivory == "Mixed Feeder"]
wat_mf[, mixed_feeder_biomass_ha := sum(biomass_ha), by = reserve ]
wat_mf <- wat_mf[!is.na(wat_mf$mixed_feeder_biomass_ha),.(mixed_feeder_biomass_ha, reserve)]
quantile(wat_mf$mixed_feeder_biomass_ha, na.rm = TRUE)
sum(is.na(wat_mf$mixed_feeder_biomass_ha))
wat_mf <- unique(wat_mf)



dt_int5 <- dt_int4 %>% 
  left_join(wat_browsers) %>% 
  left_join(wat_grazers) %>% 
  left_join(wat_mf) %>% 
  mutate(
    grazer_biomass_ha = ifelse(!is.na(grazer_biomass_ha), grazer_biomass_ha, 0), 
    browser_biomass_ha = ifelse(!is.na(browser_biomass_ha), browser_biomass_ha, 0), 
    mixed_feeder_biomass_ha = ifelse(!is.na(mixed_feeder_biomass_ha), mixed_feeder_biomass_ha, 0)
  ) %>%
  mutate(grazer_browser_ratio = ifelse(grazer_biomass_ha == 0 | browser_biomass_ha == 0, NA, grazer_biomass_ha/browser_biomass_ha),
         grazer_mixed_ratio = ifelse(grazer_biomass_ha == 0 | mixed_feeder_biomass_ha == 0, NA, grazer_biomass_ha/mixed_feeder_biomass_ha),
         browser_mixed_ratio = ifelse(mixed_feeder_biomass_ha == 0 & browser_biomass_ha == 0, NA, browser_biomass_ha/mixed_feeder_biomass_ha))

hist(dt_int5$grazer_browser_ratio)

glimpse(dt_int5)

### add functional diversity ------------------------
# library(mFD)
# 
# ### categorical traits have to be coded as factor 
# mam.traits2 <- traits[] %>% 
#   mutate(feeding_type = as.factor(feeding_type), 
#          fermentation_type = as.factor(fermentation_type), 
#          herbi_body_mass_bins = as.factor(herbi_body_mass_bins))
# 
# 
# ### assign low biomass (i.e., weight) to the species for which we have no data 
# ### and subset to herbivores only 
# herbis <- dt_int5[,] %>% 
#   filter(Herbi == "Herbivore") %>% ## subset to only herbivores 
#   dplyr::select(species, biomass_ha, density_ha, reserve) %>% 
#   mutate(biomass_ha = ifelse(is.na(biomass_ha), 0.1, biomass_ha),
#          density_ha = ifelse(is.na(density_ha), 0.1, density_ha))
# 
# 
# ### build trait data frame 
# sp_tr <-  mam.traits2 %>% dplyr::select(species, feeding_type, fermentation_type, herbi_body_mass_bins) %>% 
#   remove_rownames %>% 
#   column_to_rownames(var="species")
# 
# ### build trait categories 
# tr_cat <- data.table(
#   trait_name = c("feeding_type", "fermentation_type", "herbi_body_mass_bins"), 
#   trait_type = c("N", "N", "N"), 
#   fuzzy_name = NA
# )
# 
# ### compute functional entities 
# fe <- mFD::sp.to.fe(sp_tr = sp_tr, tr_cat = tr_cat)
# 
# #### let's try with the gower distance for now 
# 
# tr_cat_fdist <- tr_cat
# tr_cat_fdist$trait_weight <- c(1, 1, 2) ## weigh body mass double because it's so important
# fdist <- funct.dist(sp_tr = sp_tr, tr_cat = tr_cat_fdist, metric = "gower")
# 
# 
# ###          all herbis             ###
# 
# ### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
# weight.mat <- herbis %>% 
#   dplyr::select(reserve, biomass_ha, species) %>% 
#   pivot_wider(names_from = "species", values_from = "biomass_ha") %>% 
#   as.data.table() %>% 
#   mutate(across(where(is.list), ~ sapply(., toString)),
#          across(where(is.character) & !all_of("reserve"), ~ as.numeric(.)),
#          across(everything(), ~ ifelse(is.na(.), 0, .))
#   ) %>% 
#   remove_rownames %>% 
#   column_to_rownames(var="reserve") %>% 
#   as.matrix()
# 
# ### Get the occurrence dataframe:
# asb_sp_summ <- mFD::asb.sp.summary(asb_sp_w = weight.mat) 
# asb_sp_occ <- asb_sp_summ$'asb_sp_occ'
# 
# 
# ### compute metrics for alpha functional diversity 
# alpha.fd.res <- alpha.fd.fe(
#   asb_sp_occ       = asb_sp_occ, 
#   sp_to_fe         = fe,
#   ind_nm           = c('fred', 'fored', 'fvuln'),
#   check_input      = TRUE, 
#   details_returned = TRUE)
# 
# 
# ## extract simple functional diversity metrics
# dt_sfd <- alpha.fd.res$asb_fdfe %>% 
#   as.data.frame() %>% 
#   rownames_to_column(var = "reserve")
# 
# 
# ### plot for example reserve 
# alpha.fd.fe.plot(alpha.fd.res, plot_asb_nm = "Lapalala")
# herbis
# 
# ## distance based functional diversity metrics. 
# 
# 
# #### distance based functional diversity metric 
# ?alpha.fd.hill
# fd.hill.res <- alpha.fd.hill(asb_sp_w = weight.mat, sp_dist = fdist)
# 
# ## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)
# 
# ## extract functional diversity metrics 
# dt.dbfd <- fd.hill.res$asb_FD_Hill %>% 
#   as.data.frame() %>% 
#   rownames_to_column(var = "reserve")
# 
# dt.fd <- dt.dbfd %>% left_join(dt_sfd) %>% 
#   as.data.table() %>% 
#   rename(herbi_fun_ovred = fored,
#          herbi_fun_red = fred,
#          herbi_fun_div_distq1 = FD_q1, 
#          herbi_fun_ent = nb_fe) %>% 
#   dplyr::select(herbi_fun_ovred, herbi_fun_red, herbi_fun_div_distq1, herbi_fun_ent, reserve)
# 
# 
# dt_int5.5 <- dt_int5 %>% 
#   filter(!is.na(herbivore_biomass_kg_ha)) %>% 
#   left_join(dt.fd) %>% 
#   dplyr::select(reserve, max_species_body_mass, mean_species_body_mass, median_species_body_mass, 
#                cw_max_species_body_mass, cw_mean_species_body_mass, cw_median_species_body_mass, 
#                herbi_fun_red, herbi_fun_div_distq1, herbi_fun_ent, n_species_reserve,
#                herbivore_species_richness, 
#                browser_biomass_ha, grazer_biomass_ha, mixed_feeder_biomass_ha,
#                grazer_browser_ratio, grazer_mixed_ratio, browser_mixed_ratio,
#                herbivore_biomass_kg_ha, prop_biomass_w_occasional_predator, prop_biomass_w_regular_predator,
#                prop_species_w_occasional_predator, prop_species_w_regular_predator,
#                predator_biomass_ha, area_ha) %>% 
#   unique()



dt_int6 <- dt_int5 %>% 
  filter(!is.na(herbivore_biomass_kg_ha)) %>%
  dplyr::select(reserve, max_species_body_mass, mean_species_body_mass, median_species_body_mass, 
                cw_max_species_body_mass, cw_mean_species_body_mass, cw_median_species_body_mass,
                n_species_reserve,  herbivore_species_richness,
                browser_biomass_ha, grazer_biomass_ha, mixed_feeder_biomass_ha,
                grazer_browser_ratio, grazer_mixed_ratio, browser_mixed_ratio,
                herbivore_biomass_kg_ha, prop_biomass_w_occasional_predator, prop_biomass_w_regular_predator,
                prop_species_w_occasional_predator, prop_species_w_regular_predator,
                predator_biomass_ha, area_ha) %>%
  unique()

############# EXTRACT COVS -------------------

library(mapview)
mapview(dt_sf)


dt_sf <- st_transform(dt_sf, crs = 4326)
dt_utm <- st_transform(dt_sf, crs = 22235)




### Canopy heigth (meta) -------------------------------
library(terra) 
library(exactextractr)


## elevation --------

ele <- rast("../../../../resources/spatial/Elevation_ZAF/Elevation_SA_90m.tif")
crs(ele) # 
res(ele)

dt_sf_ele <- st_transform(dt_sf2, crs = crs(ele))

ele_extr <- exactextractr::exact_extract(ele, 
                                         dt_sf_ele, 
                                     append_cols = c("reserve"),
                                     fun = "mean")

setnames(ele_extr, "mean", "elevation_reserve")

## MAT---------------

mat <- rast("../../../../resources/spatial/Chelsa_Climate/CHELSA_bio1_1981-2010_V.2.1.tif") 


dt_sf_mat <- st_transform(dt_sf2, crs = crs(mat))

mat_extr <- exactextractr::exact_extract(mat, 
                                         dt_sf_mat, 
                                         append_cols = c("reserve"),
                                         fun = "mean")

setnames(mat_extr, "mean", "mat_reserve")


## MAP---------------
map <- rast("../../../../resources/spatial/Chelsa_Climate/CHELSA_bio12_1981-2010_V.2.1.tif") 

dt_sf_map <- st_transform(dt_sf2, crs = crs(map))

map_extr <- exactextractr::exact_extract(map, 
                                         dt_sf_map, 
                                         append_cols = c("reserve"),
                                         fun = "mean")

setnames(map_extr, "mean", "map_reserve")

#### combine and write 



dt_final <- dt_int6 %>% 
  left_join(ele_extr) %>% 
  left_join(mat_extr) %>% 
  left_join(map_extr)

summary(dt_final)


fwrite(x = dt_final, file = "data/processed_data/data_fragments/reserve_meta_waterberg2024.csv")

