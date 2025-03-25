#rm(list = ls())

get_mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

library(data.table)
library(tidyverse)
library(gridExtra)
library(mFD)
library(tidylog)

# Transects --------------------------------------------
tv <- fread("data/raw_data/Waterberg2024_Transect_Vegetation.csv")
tm <- fread("data/raw_data/Waterberg2024_Transect_Metadata.csv")

tm_1 <- tm %>% 
  dplyr::select(c(transect_ID, reserve, date, weather, site_ID, n_wildlife_paths)) %>% 
  unique() %>% 
  mutate(plot_ID = gsub("T", "P", transect_ID)) %>% 
  filter(!(transect_ID == "JE_S03_T01" & is.na(n_wildlife_paths))) %>% 
  mutate(site_ID = ifelse(transect_ID == "DA_S03_T03", "DA_S03", site_ID))


table(tm_1$site_ID)
  

dt_t <-  tv %>% 
left_join(tm_1) %>% unique() %>% arrange(species) %>% 
  mutate(id_level = case_when(
    .default = "species_level", 
    species %in% c("3 whorled, pronounced side veins", 
                   "Corky bark", 
                   "Fake buxifolia", 
                   "Red stem soft leaf shrub"
                   ) ~ "no")) %>% 
  group_by(plot_ID) %>% 
  mutate(woody_richness_plot = n_distinct(species), 
         woody_individuals_transect = n()) %>% 
  ungroup() %>% 
  group_by(site_ID) %>% 
  mutate(woody_richness_site = n_distinct(species), 
         woody_individuals_site = n()) %>% 
  ungroup() %>% 
  group_by(reserve) %>% 
  mutate(woody_richness_reserve = n_distinct(species),
         woody_individuals_reserve = n()) %>% 
  ungroup() %>% 
  as.data.table() %>% mutate(life_form = case_when(
    grepl("shrub", growth_form_transect) ~ "Shrub",
    grepl("tree", growth_form_transect) ~ "Tree",
  ))
  
n_distinct(dt_t$species) #112 trees 
unique(dt_t$species) 

n_distinct(dt_t[id_level == "species_level", ]$species) # 108 trees 
n_distinct(dt_t[!id_level == "species_level", ]$species) # 4 unidentified #96 % of the trees identified 
4/108


# Plots -------------------------------------------------- 
pv <- fread("data/raw_data/Waterberg2024_Plot_Vegetation.csv")

pm <- fread("data/raw_data/Waterberg2024_Plot_Metadata.csv") %>% 
  mutate(
    site_ID = gsub("_P01", "", plot_ID),
    site_ID = gsub("_P02", "", site_ID),
    site_ID = gsub("_P03", "", site_ID),
    site_ID = gsub("_P04", "", site_ID),
    site_ID = gsub("_P05", "", site_ID)
  ) 
table(pm$site_ID)

dt_p <-  pv %>% 
  left_join(pm) %>%
  unique() %>% arrange(species) %>% 
  mutate(id_level = case_when(
    .default = "species_level", 
    species %in% c("Limeum maybe", 
                   "Long spiky leaves",
                   "Milky green-blue leaves", 
                   "Opposite leaves hairy margin",
                   "Purple seed, pale stem",
                   "Red stem soft leaf shrub", 
                   "Skinny plant, thick leaves", 
                   "Smells like basil",
                   "Soft jatropha",
                   "Stone-arrow leaf",
                   "Straight long leaved forb",
                   "Tall, slender, thin leaves",
                   "Thin leaf, thin stem",
                   "Thin rough plant",
                   "Tiny 4 petal green flower",
                   "Tiny leaves, tiny white flower",
                   "Toothed 5 leaflets",
                   "Twisted leaf pairs",
                   "Underleaf raised veins",
                   "Very hairy everything", 
                   "Boring leaf",
                   "Boring opposite leaf hairy stem", 
                   "Browsed thick leaf pairs", 
                   "Dark green thin leaves",  
                   "Flat heart leaf", 
                   "Hairy leaf margin", 
                   "Hairy underleaf", 
                   "Corky bark", 
                   "Obvious veins and hairs under") ~ "no",
    species %in% c("Acanthaceae sp", 
                   "Chlorophytum sp", 
                   "Sticky fabaceae",
                   "Combretum sp", 
                   "Commelina sp", 
                   "Cyperus sp (ant's nest)",
                   "Fabaceae sp",
                   "Hypoxis sp",
                   "Ledebouria sp") ~ "higher_level"), 
    growth_form = case_when(
      growth_form == "rf (Round forb)" ~ "Round forb", 
      growth_form == "mss (Shrub multi-stemmed)" ~ "Shrub multi-stemmed", 
      growth_form == "at (Angry tussock graminoid)" ~ "Angry tussock graminoid", 
      growth_form == "sss (Shrub single-stemmed)" ~ "Shrub single-stemmed", 
      growth_form == "sf (Straight forb)" ~ "Straight forb", 
      growth_form == "mf (Messy forb)" ~ "Messy forb", 
      growth_form == "cg (Creeping graminoid)" ~ "Creeping graminoid", 
      growth_form == "mst (Tree multi-stemmed)" ~ "Tree multi-stemmed", 
      growth_form == "sst (Tree single-stemmed)" ~ "Tree single-stemmed", 
      growth_form == "cuf (Cussion forb)" ~ "Cussion forb", 
      growth_form == "rt (Relaxed tussock graminoid)" ~ "Relaxed tussock graminoid", 
      growth_form == "cf (Creeping forb)" ~ "Creeping forb", 
      growth_form == "ug (Sparse upward graminoid)" ~ "Sparse upward graminoid", 
      growth_form == "lg (Large graminoid)" ~ "Large graminoid", 
      growth_form == "" ~ NA)) %>% 
  group_by(plot_ID) %>% 
  mutate(species_richness_plot = n_distinct(species), 
         tsq_x_shrub_plot = mean(tsq_x_shrub, na.rm = T),
         tsq_t_shrub_plot = mean(tsq_t_shrub, na.rm = T),
         tsq_x_tree_plot = mean(tsq_x_tree, na.rm = T),
         tsq_t_tree_plot = mean(tsq_t_tree, na.rm = T),
         reproductive_height_plot = mean(reproductive_height, na.rm = T),
         bulk_density_plot = get_mode(bulk_density),
         leaf_size_plot = get_mode(leaf_size),
         hairs_plot = get_mode(hairs),
         width_cm_plot = mean(width_cm, na.rm = T),
         length_cm_plot = mean(as.numeric(length_cm), na.rm = T),
         height_cm_plot = mean(as.numeric(height_cm), na.rm = T),
         growth_form_plot = get_mode(growth_form),
         life_form_plot = get_mode(life_form)) %>% 
  ungroup() %>% 
  group_by(site_ID) %>% 
  mutate(species_richness_site = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(reserve) %>% 
  mutate(species_richness_reserve = n_distinct(species)) %>% 
  ungroup() %>% 
  as.data.table()

n_distinct(dt_p$species) # 435
unique(dt_p$species) 

n_distinct(dt_p[id_level == "species_level", ]$species) # 399 on species level
n_distinct(dt_p[!id_level == "species_level", ]$species) # 36 unidentified #90 % of plants identified 

n_distinct(dt_p[life_form %in% c("Forb", "Graminoid"), ]$species) 
n_distinct(dt_p[!life_form %in% c("Forb", "Graminoid"), ]$species) 

### Get life form specific richness ----------------

dt_forb <- dt_p %>% 
  filter(life_form == "Forb") %>% 
  group_by(plot_ID) %>% 
  mutate(forb_richness_plot = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(site_ID) %>% 
  mutate(forb_richness_site = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(reserve) %>% 
  mutate(forb_richness_reserve = n_distinct(species)) %>% 
  ungroup()  %>% 
  dplyr::select(plot_ID, forb_richness_plot, forb_richness_site, forb_richness_reserve) %>% 
  unique()

dt_graminoid <- dt_p %>% 
  filter(life_form == "Graminoid") %>% 
  group_by(plot_ID) %>% 
  mutate(graminoid_richness_plot = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(site_ID) %>% 
  mutate(graminoid_richness_site = n_distinct(species)) %>% 
  ungroup() %>% 
  group_by(reserve) %>% 
  mutate(graminoid_richness_reserve = n_distinct(species)) %>% 
  ungroup() %>% 
  dplyr::select(plot_ID, graminoid_richness_plot, graminoid_richness_site, graminoid_richness_reserve) %>% 
  unique()

dt_woody <- dt_t  %>% 
  dplyr::select(plot_ID, woody_richness_plot, woody_richness_site, woody_richness_reserve) %>% 
  unique()

dt_div <- dt_p %>%
  dplyr::select(plot_ID, site_ID, reserve, species_richness_plot, species_richness_site, species_richness_reserve) %>% 
  unique() %>% 
  left_join(dt_forb) %>% 
  left_join(dt_graminoid) %>% 
  left_join(dt_woody) %>% 
  group_by(site_ID) %>% 
  mutate(woody_richness_site = mean(woody_richness_site, na.rm = T), 
         woody_richness_reserve = mean(woody_richness_reserve, na.rm = T)) %>% 
  ungroup()
 
summary(dt_div) 
summary(dt_woody)


fwrite(dt_div, "data/processed_data/data_fragments/species_numbers_per_plot_waterberg2024.csv")

dt_sp <- dt_p %>%
  dplyr::select(plot_ID, site_ID, reserve, species, cover_percent, life_form, id_level, 
                growth_form, height_cm, hairs, leaf_size, bulk_density, reproductive_height) %>% 
  unique()
fwrite(dt_sp, "data/processed_data/data_fragments/plot_species_waterberg2024.csv")


### Test invasive species ------

### load invasive species data ----------

griis_tax_raw <- fread("data/raw_data/GRIIS_checklist_South_Africa/taxon.txt") %>%
  mutate(species = sapply(strsplit(scientificName, " "), function(x) paste(x[1:min(2, length(x))], collapse = " "))) 

griis_prof_raw <- fread("data/raw_data/GRIIS_checklist_South_Africa/speciesprofile.txt")
griis_dist_raw <- fread("data/raw_data/GRIIS_checklist_South_Africa/distribution.txt")

griis_sp <- griis_tax_raw %>% 
  left_join(griis_prof_raw) %>% 
  filter(isInvasive == "invasive") %>% 
  dplyr::select(species) %>% 
  pull()


dt_p %>% filter(species %in% griis_sp) %>% dplyr::select(species, reserve) %>% unique()
dt_t %>% filter(species %in% griis_sp) %>% dplyr::select(species, reserve) %>% unique()

# species       reserve
# <char>        <char>
#   1:           Achyranthes aspera    Ant's Farm
# 2: Campuloclinium macrocephalum   Syringa Sands
# 1:               Duranta erecta   Dabchick
# 3:               Lantana camara      Dabchick
# 4:       Solanum elaeagnifolium      Lapalala
# 5:               Tagetes minuta Syringa Sands
# 6:               Tagetes minuta   Summerplace
# 7:               Tagetes minuta      Dabchick
# 8:               Tagetes minuta    Ant's Farm
# 9:             Zinnia peruviana   Summerplace


#### get species lists ----------------------------


n_distinct(dt_p$species) # 435
unique(dt_p$species) 

n_distinct(dt_p[id_level == "species_level", ]$species) # 399 on species level
n_distinct(dt_p[!id_level == "species_level", ]$species) # 36 unidentified #90 % of plants identified 

n_distinct(dt_p[life_form %in% c("Forb", "Graminoid"), ]$species) 
n_distinct(dt_p[!life_form %in% c("Forb", "Graminoid"), ]$species) 


unique(dt_p$life_form)
pu <- unique(dt_p[, .(species, id_level, life_form)])
pu_res <- unique(dt_p[, .(species, id_level, life_form, reserve)])

tu <- unique(dt_t[, .(species, id_level, life_form)])
su <- unique(rbind(pu, tu))
su

### include reserves 
pu_res <- unique(dt_p[, .(species, id_level, life_form, reserve)])
tu_res <- unique(dt_t[, .(species, id_level, life_form, reserve)])

su_res <- unique(rbind(pu_res, tu_res)) %>% mutate(year = 2024)
openxlsx::write.xlsx(su_res, "builds/species_lists/species_list_waterberg2024.xlsx")

unique_identified_species <- su_res %>%
  filter(id_level == "species_level") %>%
  dplyr::select(species) %>%
  unique() 

openxlsx::write.xlsx(unique_identified_species, "builds/species_lists/unique_identified_species_waterberg2024.xlsx")

#submitted to: https://tnrs.biendata.org

dt_fam_raw <- fread("data/processed_data/data_fragments/tnrs_result.csv") %>% 
  dplyr::select(Name_submitted, Accepted_family) %>% 
  unique() %>% 
  group_by(Name_submitted) %>% 
  mutate(n = n()) %>% 
  ungroup() 


dt_fam <- dt_fam_raw %>% 
  filter(!Name_submitted %in% c("Corky bark", "Obvious veins and hairs under")) %>% 
  mutate(Accepted_family = case_when(
    .default = Accepted_family,
    Name_submitted == "Bergia decumbescens" ~ "Elatinaceae", #Bergia decumbens
    Name_submitted == "Boscia alba" ~ "Capparaceae", # Boscia albitrunca
    Name_submitted == "Satara pumila" ~ "Poaceae", # Setaria pumila
    Name_submitted == "Convolvulus sagittatus" ~ "Convolvulaceae",
    Name_submitted == "Gisekia pharna" ~ "Gisekiaceae", #Gisekia pharnaceoides
    Name_submitted == "Pappea capensis" ~ "Sapindaceae",
    Name_submitted == "Sida cordifolia" ~ "Malvaceae",
    Name_submitted == "Commiphora mollis" ~ "Burseraceae",
    Name_submitted == "Strychnos pungens" ~ "Loganiaceae",
    Name_submitted == "Turrea obtusifolia" ~ "Meliaceae",
  )) %>% 
  dplyr::select(species = Name_submitted, family = Accepted_family) %>% 
  unique() %>% 
  group_by(family) %>% 
  mutate(n_family = n()) %>% 
  ungroup()

table(dt_fam$family)
n_distinct(dt_fam[dt_fam$n_family >= 10, "family"])

fwrite(dt_fam, "data/processed_data/data_fragments/species_families.csv")


#### plot species 
pu_res2 <- dt_p[, .(plot_ID, species, id_level, life_form, reserve)] %>% 
  rbind(dt_t[, .(plot_ID, species, id_level, life_form, reserve)]) %>% unique()

dt_p_all <- pu_res2 %>% mutate(year = 2024) %>% arrange(plot_ID)
openxlsx::write.xlsx(dt_p_all, "builds/species_lists/plot_species_list_waterberg2024.xlsx")

dt_enc <- dt_p[, .(plot_ID, species, reserve)] %>%
  group_by(species) %>% 
  mutate(n_encounters = n()) %>% 
  ungroup() %>% 
  dplyr::select(n_encounters, species) %>% 
  unique()

n_distinct(dt_enc[dt_enc$n_encounters > 0, ]$species) #435

n_distinct(dt_enc[dt_enc$n_encounters > 1, ]$species) #287
n_distinct(dt_enc[dt_enc$n_encounters > 2, ]$species) #220
n_distinct(dt_enc[dt_enc$n_encounters > 3, ]$species) #173
287/437 #66 % more than once --> measured on 6 individuals 
220/437 #50 % more than twice --> measured at 9 individuals 
173/437 #40 % more than three tomes --> measured at 12 individuals  

p_enc <- dt_enc %>% 
  ggplot() +
  geom_histogram(aes(x = n_encounters)) +
  labs(x = "Number of plots a species was found in") +
  theme_classic()

### subset 

sp_la <- su_res %>% filter(reserve == "Lapalala")
openxlsx::write.xlsx(sp_la, "builds/species_lists/lapalala_species_list.xlsx")

sp_je <- su_res %>% filter(reserve == "Jembisa")
openxlsx::write.xlsx(sp_je, "builds/species_lists/jembisa_species_list.xlsx")

sp_wi <- su_res %>% filter(reserve == "Willowisp")
openxlsx::write.xlsx(sp_wi, "builds/species_lists/willowisp_species_list.xlsx")

sp_sy <- su_res %>% filter(reserve == "Syringa Sands")
openxlsx::write.xlsx(sp_sy, "builds/species_lists/syringa_sands_quella_species_list.xlsx")

sp_su <- su_res %>% filter(reserve == "Summerplace")
openxlsx::write.xlsx(sp_su, "builds/species_lists/summerplace_species_list.xlsx")

sp_da <- su_res %>% filter(reserve == "Dabchick")
openxlsx::write.xlsx(sp_da, "builds/species_lists/dabchick_species_list.xlsx")

sp_an <- su_res %>% filter(reserve == "Ant's Farm")
openxlsx::write.xlsx(sp_an, "builds/species_lists/ants_farm_species_list.xlsx")

sp_ka <- su_res %>% filter(reserve == "Kaingo")
openxlsx::write.xlsx(sp_ka, "builds/species_lists/kaingo_species_list.xlsx")

sp_sw <- su_res %>% filter(reserve == "Swebeswebe")
openxlsx::write.xlsx(sp_sw, "builds/species_lists/swebeswebe_species_list.xlsx")

sp_ma <- su_res %>% filter(reserve == "Marakele")
openxlsx::write.xlsx(sp_ma, "builds/species_lists/marakele_fenced_part_species_list.xlsx")


n_distinct(su$species)
n_distinct(su[id_level == "species_level", ]$species) #449
n_distinct(su[!id_level == "species_level", ]$species) #39

n_distinct(su[id_level == "species_level", ]$species) #449
ws <- unique(su[life_form %in% c("Tree", "Shrub"), ]$species)

n_distinct(su[life_form == "Graminoid", ]$species) #449
n_distinct(su[life_form %in% c("Tree", "Shrub"), ]$species) #449
n_distinct(su[species %in% c(ws) & life_form %in% c("Forb"), ]$species) #449

n_distinct(su[!species %in% c(ws) & life_form %in% c("Forb"), ]$species) #449
n_distinct(su[!species %in% c(ws) & life_form %in% c("Graminoid"), ]$species) #449

table(su$life_form)
39/449

449/488 #92%identified




#ggsave(plot = p.divs, "builds/exploratory_plots/diversity_relationsips.png", dpi = 600, height = 8, width = 12)  
