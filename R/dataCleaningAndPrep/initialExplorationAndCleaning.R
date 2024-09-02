rm(list = ls())

library(data.table)
library(tidyverse)
library(gridExtra)
library(mFD)

### load invasive species data ----------

griis.tax.raw <- fread("data/rawData/GRIIS_checklist_South_Africa/taxon.txt") %>%
  mutate(species = sapply(strsplit(scientificName, " "), function(x) paste(x[1:min(2, length(x))], collapse = " "))) 

griis.prof.raw <- fread("data/raw_data/GRIIS_checklist_South_Africa/speciesprofile.txt")
table(griis.prof.raw$isInvasive)
griis.dist.raw <- fread("data/raw_data/GRIIS_checklist_South_Africa/distribution.txt")

griis.sp <- griis.tax.raw %>% 
  left_join(griis.prof.raw) %>% 
  filter(isInvasive == "invasive") %>% 
  dplyr::select(species) %>% 
  pull()

# Transects --------------------------------------------
tv <- fread("data/rawData/Waterberg2024_Transect_Vegetation.csv")
tm <- fread("data/rawData/Waterberg2024_Transect_Metadata.csv")

tm.1 <- tm %>% 
  dplyr::select(c(transect_ID, reserve, site_ID, date, weather, n_wildlife_paths)) %>% 
  unique()


dt.t <-  tv %>% 
left_join(tm.1) %>% unique() %>% arrange(species) %>% 
  mutate(identification = case_when(
    .default = "species_level", 
    species %in% c("3 whorled, pronounced side veins", 
                   "Corky bark", 
                   "Fake buxifolia", 
                   "Red stem soft leaf shrub"
                   ) ~ "no"))


n_distinct(dt.t$species) #112 trees 
unique(dt.t$species) 

dt.t %>% filter(species %in% griis.sp) %>% dplyr::select(species) %>% unique()
#that's really only two species, both at Dabchick 


n_distinct(dt.t[identification == "species_level", ]$species) # 108 trees 
n_distinct(dt.t[!identification == "species_level", ]$species) # 4 unidentified #96 % of the trees identified 


4/108




dt.t[, species_per_reserve := uniqueN(species), by = reserve]
unique(dt.t[, .(species_per_reserve, reserve)])
dt.t[is.na(dt.t$reserve), transect_ID]

dt.t[, species_per_site := uniqueN(species), by = site_ID]
unique(dt.t[, .(species_per_site, reserve)])
dt.t[, indiv_per_transect := .N, by = transect_ID]
dt.t[, woody_indiv_per_site := .N, by = site_ID]

dt.t[, woody_indiv_per_reserve := .N, by = reserve]



t1 <- ggplot(data = dt.t, aes(y = species_per_site, x = reserve, fill = reserve), alpha = 0.5) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_viridis_d() +
  geom_point(size = 3, alpha = 0.9) +
  labs(x = "Reserve", y = "Species per site", title = "Woody species per site") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 10))

t2 <- ggplot(data = dt.t, aes(y = species_per_reserve, x = reserve, color = reserve), alpha = 0.5) +
  geom_point(size = 5, alpha = 0.9) +
  scale_color_viridis_d() +
  geom_point(size = 3, alpha = 0.9) +
  labs(x = "Reserve", y = "Species per reserve", title = "Woody species per reserve") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 10))

t3 <- ggplot(data = dt.t, aes(y = woody_indiv_per_reserve, x = reserve, color = reserve), alpha = 0.5) +
  geom_point(size = 5, alpha = 0.9) +
  scale_color_viridis_d() +
  geom_point(size = 3, alpha = 0.9) +
  labs(x = "Reserve", y = "Woody individuals per reserve", title = "Woody individuals per reserve") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 10))

t4 <- ggplot(data = dt.t, aes(y = woody_indiv_per_site, x = reserve, fill = reserve), alpha = 0.5) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_viridis_d() +
  geom_point(size = 3, alpha = 0.9) +
  labs(x = "Reserve", y = "Woody individuals per site", title = "Woody individuals per site") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 10))

pt <- grid.arrange(t1, t2, t3, t4)

#ggsave(plot = pt, "builds/exploratory_plots/transect_boxplots.png", dpi = 600, height = 9, width = 13)  


# Plots -------------------------------------------------- 
pv <- fread("data/rawData/Waterberg2024_Plot_Vegetation.csv")
pm <- fread("data/rawData/Waterberg2024_Plot_Metadata.csv")

dt.p <-  pv %>% 
  left_join(pm) %>%
  unique() %>% arrange(species) %>% 
  mutate(identification = case_when(
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
                   "Hairy underleaf") ~ "no",
    species %in% c("Acanthaceae sp", 
                   "Chlorophytum sp", 
                   "Sticky fabaceae",
                   "Combretum sp", 
                   "Commelina sp", 
                   "Cyperus sp (ant's nest)",
                   "Fabaceae sp",
                   "Hypoxis sp",
                   "Ledebouria sp") ~ "higher_level"))


table(dt.p$identification)


n_distinct(dt.p$species) # 435 - given that we likely have a lot of weird spellings in there it's probably something in the direction of 300
unique(dt.p$species) 

n_distinct(dt.p[identification == "species_level", ]$species) # 399 on species level
n_distinct(dt.p[!identification == "species_level", ]$species) # 36 unidentified #90 % of plants identified 

n_distinct(dt.p[life_form %in% c("Forb", "Graminoid"), ]$species) 
n_distinct(dt.p[!life_form %in% c("Forb", "Graminoid"), ]$species) 


dt.t <- dt.t %>% mutate(life_form = case_when(
  grepl("shrub", growth_form_transect) ~ "Shrub",
  grepl("tree", growth_form_transect) ~ "Tree",
))

unique(dt.p$life_form)
pu <- unique(dt.p[, .(species, identification, life_form)])
pu.res <- unique(dt.p[, .(species, identification, life_form, reserve)])

tu <- unique(dt.t[, .(species, identification, life_form)])
su <- unique(rbind(pu, tu))
su
### include reserves 
pu.res <- unique(dt.p[, .(species, identification, life_form, reserve)])
tu.res <- unique(dt.t[, .(species, identification, life_form, reserve)])

su.res <- unique(rbind(pu.res, tu.res)) %>% filter(!identification == "no") %>% mutate(year = 2024)

openxlsx::write.xlsx(su.res, "builds/specieslists/speciesListWaterberg2024.xlsx")

#### plot species 
pu.res2 <- unique(dt.p[, .(plot_ID, species, identification, life_form, reserve)])
dt.p.all <- pu.res2 %>% filter(!identification == "no") %>% mutate(year = 2024) %>% arrange(plot_ID)
openxlsx::write.xlsx(dt.p.all, "builds/specieslists/speciesListWaterberg2024.xlsx")

### subest 

sp.la <- su.res %>% filter(reserve == "Lapalala")
openxlsx::write.xlsx(sp.la, "builds/specieslists/lapalala_species_list.xlsx")

sp.je <- su.res %>% filter(reserve == "Jembisa")
openxlsx::write.xlsx(sp.je, "builds/specieslists/jembisa_species_list.xlsx")

sp.wi <- su.res %>% filter(reserve == "Willowisp")
openxlsx::write.xlsx(sp.wi, "builds/specieslists/willowisp_species_list.xlsx")

sp.sy <- su.res %>% filter(reserve == "Syringa Sands")
openxlsx::write.xlsx(sp.sy, "builds/specieslists/syringa_sands_quella_species_list.xlsx")

sp.su <- su.res %>% filter(reserve == "Summerplace")
openxlsx::write.xlsx(sp.su, "builds/specieslists/summerplace_species_list.xlsx")

sp.da <- su.res %>% filter(reserve == "Dabchick")
openxlsx::write.xlsx(sp.da, "builds/specieslists/dabchick_species_list.xlsx")

sp.an <- su.res %>% filter(reserve == "Ant's Farm")
openxlsx::write.xlsx(sp.an, "builds/specieslists/ants_farm_species_list.xlsx")

sp.ka <- su.res %>% filter(reserve == "Kaingo")
openxlsx::write.xlsx(sp.ka, "builds/specieslists/kaingo_species_list.xlsx")

sp.sw <- su.res %>% filter(reserve == "Swebeswebe")
openxlsx::write.xlsx(sp.sw, "builds/specieslists/swebeswebe_species_list.xlsx")

sp.ma <- su.res %>% filter(reserve == "Marakele")
openxlsx::write.xlsx(sp.ma, "builds/specieslists/marakele_fenced_part_species_list.xlsx")


n_distinct(su$species)
n_distinct(su[identification == "species_level", ]$species) #449
n_distinct(su[!identification == "species_level", ]$species) #39

n_distinct(su[identification == "species_level", ]$species) #449
ws <- unique(su[life_form %in% c("Tree", "Shrub"), ]$species)

n_distinct(su[life_form == "Graminoid", ]$species) #449
n_distinct(su[life_form %in% c("Tree", "Shrub"), ]$species) #449
n_distinct(su[species %in% c(ws) & life_form %in% c("Forb"), ]$species) #449

n_distinct(su[!species %in% c(ws) & life_form %in% c("Forb"), ]$species) #449
n_distinct(su[!species %in% c(ws) & life_form %in% c("Graminoid"), ]$species) #449

table(su$life_form)
39/449

449/488 #92%identified


dt.p[, species_per_reserve := uniqueN(species), by = reserve]
unique(dt.p[, .(species_per_reserve, reserve)])
dt.p[is.na(dt.p$reserve), plot_ID]


dt.p[, species_per_plot := .N, by = plot_ID]
dt.p[, species_per_site := uniqueN(species), by = site_ID]
unique(dt.p[, .(species_per_site, reserve, site_ID)])
unique(dt.p[site_ID == "SU_S05", .(species)])

dt.p[, growth_forms_per_site := uniqueN(growth_form), by = site_ID]
dt.p[, growth_forms_per_plot := uniqueN(growth_form), by = plot_ID]

unique(dt.p[, .(growth_forms_per_site, reserve, site_ID)])
names(dt.p)



fwrite(dt.p, "data/processedData/dataFragments/species_numbers_per_plot_waterberg2024.csv")

p1 <- ggplot(data = dt.p, aes(y = species_per_site, x = reserve, fill = reserve)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(size = 3, alpha = 0.9) +
  scale_fill_viridis_d() +
  labs(x = "Reserve", y = "Species per site", title = "Species per site") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 10))

p1

p2 <- ggplot(data = dt.p, aes(y = species_per_plot, x = reserve, fill = reserve), alpha = 0.5) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_viridis_d() +
  geom_point(size = 3, alpha = 0.9) +
  labs(x = "Reserve", y = "Species per plot", title = "Species per plot") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 10))

p2


p3 <- ggplot(data = dt.p, aes(y = species_per_reserve, x = reserve, color = reserve), alpha = 0.5) +
  geom_point(size = 5, alpha = 0.9) +
  scale_color_viridis_d() +
  labs(x = "Reserve", y = "Species per reserve", title = "Species per reserve") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 10))

p3

p4 <- ggplot(data = dt.p, aes(y = growth_forms_per_site, x = reserve, fill = reserve), alpha = 0.5) +
  geom_boxplot(alpha = 0.5) +  
  scale_fill_viridis_d() +
  geom_point(size = 3, alpha = 0.9) +
  labs(x = "Reserve", y = "Growth forms per site", title = "Growth forms per site") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 10))

p4


pp <- grid.arrange(p1, p2, p3, p4)
#ggsave(plot = pp, "builds/exploratory_plots/plot_species_first_five_reserves.png", dpi = 600, height = 9, width = 13)  


# next: graminoid richness from plots 
# forb: forb richness from plots 
# woody richness from transects
# both per site and per reserve

## join the different species richness values 

dt.gram <-  dt.p[dt.p$life_form == "Graminoid", ]

dt.gram[, graminoids_per_reserve := uniqueN(species), by = reserve]
unique(dt.gram[, .(graminoids_per_reserve, reserve)])
dt.gram[, graminoids_per_site := uniqueN(species), by = site_ID] #missing JE_S03 for some odd reason
dt.gram[, graminoids_per_plot := uniqueN(species), by = plot_ID] 

dt.gram.sites <- dt.gram[, .(plot_ID, graminoids_per_reserve, graminoids_per_site,graminoids_per_plot, site_ID, reserve)] %>% unique 


dt.forb <-  dt.p[dt.p$life_form == "Forb", ]

dt.forb[, forbs_per_reserve := uniqueN(species), by = reserve]
unique(dt.forb[, .(forbs_per_reserve, reserve)])
dt.forb[, forbs_per_site := uniqueN(species), by = site_ID]
dt.forb[, forbs_per_plot := uniqueN(species), by = plot_ID] 


dt.forb.sites <- dt.forb[, .(plot_ID, forbs_per_reserve, forbs_per_site, forbs_per_plot, site_ID, reserve)] %>% unique 


dt.p.sites <- dt.p[, .(species_per_site, species_per_reserve, site_ID, reserve)] %>% unique



dt.t[, woodies_per_reserve := uniqueN(species), by = reserve]

dt.t[, woodies_per_site := uniqueN(species), by = site_ID]
dt.t[, woodies_per_transect := uniqueN(species), by = transect_ID]

dt.t.sites <- dt.t[, .(woodies_per_site, woodies_per_reserve, woody_indiv_per_site, woody_indiv_per_reserve, woodies_per_transect, site_ID, reserve)] %>% unique

max(dt.p.sites$species_per_site)
max(dt.t.sites$woodies_per_site)

###### get growth form diversity 

t.gf <- dt.t %>% dplyr::select(site_ID, growth_form_transect, reserve) %>% 
  rename(growth_form = growth_form_transect) %>% 
  mutate(growth_form = case_when(
    growth_form == "multi-stemmed shrub" ~ "Shrub multi-stemmed",
    growth_form == "single-stemmed shrub" ~ "Shrub single-stemmed",
    growth_form == "single-stemmed tree" ~ "Tree single-stemmed",
    growth_form == "multi-stemmed tree" ~ "Tree multi-stemmed",
  ))

p.gf <- dt.p %>% dplyr::select(site_ID, growth_form, reserve) %>% 
  mutate(growth_form = case_when(
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
    growth_form == "" ~ NA
    
  ))

dt.gf.reserve <- p.gf %>% rbind(t.gf) %>% 
  group_by(reserve) %>% 
  summarise(growth_forms_per_reserve = n_distinct(growth_form))


dt.gf <- p.gf %>% rbind(t.gf) %>% 
  group_by(site_ID) %>% 
  summarise(growth_forms_per_site = n_distinct(growth_form))



unique(p.gf$growth_form)
unique(t.gf$growth_form)

unique(dt.gram.sites$site_ID)

dt.p[, .(plot_ID, site_ID, species_per_plot)]


###

dt.p %>% filter(species %in% griis.sp) %>% dplyr::select(species, reserve) %>% unique()
dt.t %>% filter(species %in% griis.sp) %>% dplyr::select(species, reserve) %>% unique()

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

##### combine -----------

dt.div <- dt.t.sites %>% 
  dplyr::select(-woodies_per_transect) %>% 
  left_join(dt.p.sites) %>% 
  left_join(dt.forb.sites %>% dplyr::select(-forbs_per_plot, -plot_ID) %>% unique()) %>% 
  left_join(dt.gram.sites %>% dplyr::select(-graminoids_per_plot, -plot_ID) %>% unique()) %>% 
  left_join(dt.gf) %>% 
  left_join(dt.gf.reserve) %>% 
  rename(plot_species_per_site = species_per_site,
         plot_species_per_reserve = species_per_reserve) %>% unique()


fwrite(dt.div, "data/processedData/dataFragments/species_numbers_per_site_waterberg2024.csv")

### plot scale --------

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

mean(as.numeric(dt.p$length_cm), na.rm = T)

dt.p.bu <- dt.p %>%   mutate(
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
    growth_form == "" ~ NA
  ))
  

dt.p.dom <- dt.p.bu[, `:=` (cumulative_cover = sum(cover_percent, na.rm = T)), 
             by = c("growth_form", "plot_ID")] %>% dplyr::select(growth_form, cumulative_cover, cover_percent, plot_ID)

dominant.growth.form <- dt.p.dom %>%
  group_by(plot_ID) %>%
  slice_max(order_by = cumulative_cover, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(plot_ID, growth_form) %>% 
  rename(dominant_growth_form_plot = growth_form)

table(dominant.growth.form$dominant_growth_form_plot)


dt.p2 <- dt.p.bu[,`:=` (
       tsq_x_shrub = mean(tsq_x_shrub, na.rm = T),
       tsq_t_shrub = mean(tsq_t_shrub, na.rm = T),
       tsq_x_tree = mean(tsq_x_tree, na.rm = T),
       tsq_t_tree = mean(tsq_t_tree, na.rm = T),
       reproductive_height = mean(reproductive_height, na.rm = T),
       bulk_density = Mode(bulk_density),
       leaf_size = Mode(leaf_size),
       hairs = Mode(hairs),
       width_cm = mean(width_cm, na.rm = T),
       length_cm = mean(as.numeric(length_cm), na.rm = T),
       height_cm = mean(as.numeric(height_cm), na.rm = T),
       growth_form = Mode(growth_form),
       life_form = Mode(life_form)
       ), by = plot_ID] %>%   mutate(
         growth_form = case_when(
           .default = growth_form,
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
         growth_form == "lg (Large graminoid)" ~ "Large graminoid"
       )) %>% 
  dplyr::select(bare_ground, rock_cover, tsq_x_shrub, tsq_t_shrub, tsq_x_tree, tsq_t_tree,
                reproductive_height, bulk_density, leaf_size, hairs, 
                width_cm, length_cm, height_cm, growth_form, life_form, growth_forms_per_plot,
                plot_ID, site_ID, species_per_plot) %>% unique() %>% 
  left_join(dominant.growth.form)

dt.div.plot <- dt.p2 %>% 
  left_join(dt.forb.sites[,.(plot_ID, forbs_per_plot)]) %>% 
  left_join(dt.gram.sites[,.(plot_ID, graminoids_per_plot)])
  

fwrite(dt.div.plot, "data/processedData/dataFragments/plot_traits_and_div_waterberg2024.csv")


p.div1 <- ggplot(data = dt.div, aes(x = woodies_per_site, y = graminoids_per_site)) + 
  geom_point(aes(color = reserve)) +
  scale_color_viridis_d() +
  geom_smooth(method = "lm", color = "grey25") +
  labs(x = "Woody species per site", y = "Graminoids per site") + 
  theme_classic() +
  theme(legend.position = "none")
p.div1

p.div2 <- ggplot(data = dt.div, aes(x = woodies_per_site, y = forbs_per_site)) + 
  geom_point(aes(color = reserve)) +
  scale_color_viridis_d() +
  geom_smooth(method = "lm", color = "grey25") +
  labs(x = "Woody species per site", y = "Forbs per site") + 
  theme_classic() +
  theme(legend.position = "none")
p.div2

p.div3 <- ggplot(data = dt.div, aes(x = graminoids_per_site, y = forbs_per_site)) + 
  geom_point(aes(color = reserve)) +
  scale_color_viridis_d() +
  geom_smooth(method = "lm", color = "grey25") +
  labs(x = "Graminoids per site", y = "Forbs per site") + 
  theme_classic() +
  theme(legend.position = "none")
p.div3

p.div4 <- ggplot(data = dt.div, aes(x = woodies_per_reserve, y = graminoids_per_reserve)) + 
  geom_point(aes(color = reserve)) +
  scale_color_viridis_d() +
  geom_smooth(method = "lm", color = "grey25") +
  labs(x = "Woody species per reserve", y = "Graminoids per reserve") + 
  theme_classic() +
  theme(legend.position = "none")
p.div4

p.div5 <- ggplot(data = dt.div, aes(x = woodies_per_reserve, y = forbs_per_reserve)) + 
  geom_point(aes(color = reserve)) +
  scale_color_viridis_d() +
  geom_smooth(method = "lm", color = "grey25") +
  labs(x = "Woody species per reserve", y = "Forbs per reserve") + 
  theme_classic() +
  theme(legend.position = "none")
p.div5

p.div6 <- ggplot(data = dt.div, aes(x = graminoids_per_reserve, y = forbs_per_reserve)) + 
  geom_point(aes(color = reserve)) +
  scale_color_viridis_d() +
  geom_smooth(method = "lm", color = "grey25") +
  labs(x = "Graminoids per reserve", y = "Forbs per reserve") + 
  theme_classic() +
  theme(legend.position = "none")
p.div6


p.divs <- grid.arrange(p.div1, p.div2, p.div3, p.div4, p.div5, p.div6, ncol = 3)
#ggsave(plot = p.divs, "builds/exploratory_plots/diversity_relationsips.png", dpi = 600, height = 8, width = 12)  
