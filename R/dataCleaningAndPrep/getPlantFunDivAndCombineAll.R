### calculate plot functional diversity 


library(mFD)
library(tidyverse)
library(data.table)
library(vegan)


dt.p <- fread("data/processedData/dataFragments/species_numbers_per_plot_waterberg2024.csv") %>% 
  mutate(
    site_ID = gsub("_P01", "", plot_ID),
    site_ID = gsub("_P02", "", site_ID),
    site_ID = gsub("_P03", "", site_ID),
    site_ID = gsub("_P04", "", site_ID),
    site_ID = gsub("_P05", "", site_ID)
  ) %>% 
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
  ))

dt.plot <- dt.p
quantile(dt.plot$cover_percent, na.rm = T)
dt.p$site_ID

### calc max cover #######


max.cover.plot <- dt.plot %>% 
  dplyr::select(plot_ID, site_ID, reserve, cover_percent) %>% 
  group_by(plot_ID) %>% 
  slice_max(cover_percent) %>% unique() %>% 
  rename(plot_max_cover = cover_percent)

max.cover.site <- dt.plot %>% 
  dplyr::select(site_ID, species, cover_percent, site_ID, reserve) %>% 
  group_by(reserve, site_ID, species) %>% 
  summarize(cover_percent = sum(cover_percent, na.rm = T)) %>% 
  dplyr::select(-species) %>% 
  slice_max(cover_percent) %>% unique()  %>% 
  rename(site_max_cover = cover_percent) %>% 
  mutate(site_max_cover = site_max_cover/5)

max.cover.reserve <- dt.plot %>% 
  dplyr::select(site_ID, species, cover_percent, site_ID, reserve) %>% 
  group_by(reserve, species) %>% 
  summarize(cover_percent = sum(cover_percent, na.rm = T)) %>% 
  slice_max(cover_percent) %>% unique()  %>% 
  rename(reserve_max_cover = cover_percent) %>% 
  mutate(reserve_max_cover = reserve_max_cover/25)

dt.max.cover <- max.cover.plot %>% 
  left_join(max.cover.site) %>% 
  left_join(max.cover.reserve)

hist(max.cover.site$site_max_cover)

###### calculate FD -----------------------------------


### categorical traits have to be coded as factor 
# sample size here using only terrestrial , extant herbivores > 10kg, 
n <- 435 #number of species
#number of breakpoints using Sturges rule
k <- ceiling(log2(n) + 1)

traits <- dt.p[,] %>% 
  dplyr::select(plot_ID, species, growth_form, height_cm, hairs, leaf_size, bulk_density, reproductive_height) %>% 
  mutate(growth_form = as.factor(growth_form), 
         height_cm = as.numeric(height_cm), 
       #  height_bins = as.factor(cut_number(height_cm, n = k)),
         hairs = as.factor(hairs), 
         leaf_size = as.factor(leaf_size),
         bulk_density = as.factor(bulk_density), 
         reproductive_height = as.numeric(reproductive_height))


Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

glimpse(traits)
traits2 <- traits[, `:=` (
  growth_form = Mode(growth_form),
  height_cm = max(height_cm, na.rm = T), 
  reproductive_height = max(reproductive_height, na.rm = T), 
  hairs = Mode(hairs), 
  leaf_size = Mode(leaf_size), 
  bulk_density = Mode(bulk_density)
), by = species] %>% mutate(height_bins = as.factor(cut_number(height_cm, n = k)))

sum(!is.na(traits2$reproductive_height))

sum(!is.na(traits$reproductive_height))

## remove reproductive height for now as we don't have any expectations 
trait.data <- traits2 %>% dplyr::select(-plot_ID, -reproductive_height, -height_cm) %>% unique() %>% filter(complete.cases(.))
summary(trait.data)
n_distinct(trait.data$height_bins)

#### trait data: 
trait.data 

### build trait data frame for functions 
sp_tr <-  trait.data %>% dplyr::select(species, growth_form, height_bins, hairs, leaf_size, bulk_density) %>% 
  filter(complete.cases(.)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="species") 


### build trait categories 
tr_cat <- data.table(
  trait_name = c("growth_form","height_bins", "hairs", "leaf_size", "bulk_density"), 
  trait_type = c("N", "N", "N", "N","N"), 
  fuzzy_name = NA
)
### compute functional entities 
fe <- mFD::sp.to.fe(sp_tr = sp_tr, tr_cat = tr_cat)

## distance based functional diversity metrics. 
#### as we're dealing with categorical traits we have to use the gower distance 
tr_cat_fdist <- tr_cat
tr_cat_fdist$trait_weight <- c(1, 1, 1, 1, 1) ## 
fdist <- mFD::funct.dist(sp_tr = sp_tr, tr_cat = tr_cat_fdist, metric = "gower")

#### get functional spaces 

fspaces_quality <- mFD::quality.fspaces(
  sp_dist = fdist,
)

#### get matrix of species coordinates 

sp_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"

#### Load Data again ####

dt.p <- fread("data/processedData/dataFragments/species_numbers_per_plot_waterberg2024.csv") %>% 
  mutate(
    site_ID = gsub("_P01", "", plot_ID),
    site_ID = gsub("_P02", "", site_ID),
    site_ID = gsub("_P03", "", site_ID),
    site_ID = gsub("_P04", "", site_ID),
    site_ID = gsub("_P05", "", site_ID)
  ) %>% 
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
  ))

##################### alpha diversity ########################

######------------------ plot level --------------------######

# ---------------------- all life forms -------------------- #

plants <- dt.p[,] %>% 
  dplyr::select(species, cover_percent, plot_ID) %>% 
  ## assign low cover to some of the species
  mutate(cover_percent = ifelse(is.na(cover_percent), 1, cover_percent))

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight.mat <- plants %>% 
  dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
  dplyr::select(plot_ID, cover_percent, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover_percent") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("plot_ID"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="plot_ID") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ <- mFD::asb.sp.summary(asb_sp_w = weight.mat) 
asb_sp_occ <- asb_sp_summ$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd <- alpha.fd.res$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "plot_ID")

summary(dt.sfd)
### plot for example reserve 
mFD::alpha.fd.fe.plot(alpha.fd.res, plot_asb_nm = "KA_S03_P05")

## distance based functional diversity metrics. 


#### as we're dealing with categorical traits we have to use the gower distance 
tr_cat_fdist <- tr_cat
tr_cat_fdist$trait_weight <- c(1, 1, 1, 1, 1) ## 
fdist <- mFD::funct.dist(sp_tr = sp_tr, tr_cat = tr_cat_fdist, metric = "gower")


#### distance based functional diversity metric 
fd.hill.res <- alpha.fd.hill(asb_sp_w = weight.mat, sp_dist = fdist)

## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)

#### get functional spaces 

fspaces_quality <- mFD::quality.fspaces(
  sp_dist = fdist,
)
round(fspaces_quality$"quality_fspaces", 3) 
### lowest number is the best one

#### get matrix of species coordinates 

sp_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight.mat,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


#alpha_fd_indices$functional_diversity_indices

## extract functional diversity metrics 
d.afdi <- alpha_fd_indices$functional_diversity_indices %>% 
  rownames_to_column(var = "plot_ID") %>% 
  as.data.table() %>% 
  dplyr::select("plot_ID", "fdis", "fmpd", "fnnd", "feve", "fori", 
                "fspe")

names(d.afdi) <- ifelse(names(d.afdi) == "plot_ID", paste0(names(d.afdi)), paste0("plot_", names(d.afdi)))

dt.dbfd <- fd.hill.res$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "plot_ID")

dt.fd.plot.level.raw <- dt.dbfd %>% 
  left_join(dt.sfd) %>% 
  left_join(d.afdi) %>% 
  as.data.table() %>% 
  rename(plot_plant_fun_red = fred,
         plot_plant_fun_ovred = fored,
         plot_plant_fun_div_distq1 = FD_q1, 
         plot_plant_fun_ent = nb_fe, 
         plot_plant_fvuln = fvuln) %>% 
  dplyr::select(plot_ID, plot_plant_fun_red, plot_plant_fun_ovred, plot_plant_fun_div_distq1, plot_plant_fun_ent, plot_feve, plot_plant_fvuln) %>% 
  mutate(
    site_ID = gsub("_P01", "", plot_ID),
    site_ID = gsub("_P02", "", site_ID),
    site_ID = gsub("_P03", "", site_ID),
    site_ID = gsub("_P04", "", site_ID),
    site_ID = gsub("_P05", "", site_ID)
  )

dt.p.fin <- dt.p %>% left_join(dt.fd.plot.level.raw)

# ---------------------- herbacous -------------------- #

herbs <- dt.p[life_form %in% c("Forb", "Graminoid"),] %>% 
  dplyr::select(species, cover_percent, plot_ID) %>% 
  ## assign low cover to some of the species
  mutate(cover_percent = ifelse(is.na(cover_percent), 1, cover_percent))

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight.mat.herbs <- herbs %>% 
  dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
  dplyr::select(plot_ID, cover_percent, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover_percent") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("plot_ID"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="plot_ID") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ_herbs <- mFD::asb.sp.summary(asb_sp_w = weight.mat.herbs) 
asb_sp_occ_herbs <- asb_sp_summ_herbs$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res.herbs <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ_herbs, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd.herbs <- alpha.fd.res.herbs$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "plot_ID")
names(dt.sfd.herbs) <- ifelse(names(dt.sfd.herbs) == "plot_ID", paste0(names(dt.sfd.herbs)), paste0("plot_herb_", names(dt.sfd.herbs)))


#### distance based functional diversity metric 
fd.hill.res.herbs <- alpha.fd.hill(asb_sp_w = weight.mat.herbs, sp_dist = fdist)

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices_herbs <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight.mat.herbs,
  ind_vect         = c("fdis", "fmpd", "fnnd", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


#alpha_fd_indices_herbs$functional_diversity_indices

## extract functional diversity metrics 
d.afdi.herbs <- alpha_fd_indices_herbs$functional_diversity_indices %>% 
  rownames_to_column(var = "plot_ID") %>% 
  as.data.table() %>% 
  dplyr::select("plot_ID", "fdis", "fmpd", "fnnd", "fori", 
                "fspe")

names(d.afdi.herbs) <- ifelse(names(d.afdi.herbs) == "plot_ID", paste0(names(d.afdi.herbs)), paste0("plot_herb_", names(d.afdi.herbs)))

dt.dbfd_herb <- fd.hill.res.herbs$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "plot_ID")

names(dt.dbfd_herb) <- ifelse(names(dt.dbfd_herb) == "plot_ID", paste0(names(dt.dbfd_herb)), paste0("plot_herb_", names(dt.dbfd_herb)))

## herb results 
d.afdi.herbs
dt.dbfd_herb
dt.sfd.herbs

# ---------------------- graminoids -------------------- #

graminoids <- dt.p[life_form %in% c("Graminoid"),] %>% 
  dplyr::select(species, cover_percent, plot_ID) %>% 
  ## assign low cover to some of the species
  mutate(cover_percent = ifelse(is.na(cover_percent), 1, cover_percent))

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight.mat.graminoids <- graminoids %>% 
  dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
  dplyr::select(plot_ID, cover_percent, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover_percent") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("plot_ID"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="plot_ID") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ_graminoids <- mFD::asb.sp.summary(asb_sp_w = weight.mat.graminoids) 
asb_sp_occ_graminoids <- asb_sp_summ_graminoids$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res.graminoids <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ_graminoids, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd.graminoids <- alpha.fd.res.graminoids$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "plot_ID")
names(dt.sfd.graminoids) <- ifelse(names(dt.sfd.graminoids) == "plot_ID", paste0(names(dt.sfd.graminoids)), paste0("plot_graminoid_", names(dt.sfd.graminoids)))


#### distance based functional diversity metric 
fd.hill.res.graminoids <- alpha.fd.hill(asb_sp_w = weight.mat.graminoids, sp_dist = fdist)

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices_graminoids <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight.mat.graminoids,
  ind_vect         = c("fdis", "fmpd", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


#alpha_fd_indices_graminoids$functional_diversity_indices

## extract functional diversity metrics 
d.afdi.graminoids <- alpha_fd_indices_graminoids$functional_diversity_indices %>% 
  rownames_to_column(var = "plot_ID") %>% 
  as.data.table() %>% 
  dplyr::select("plot_ID", "fdis", "fmpd", "fori", 
                "fspe")

names(d.afdi.graminoids) <- ifelse(names(d.afdi.graminoids) == "plot_ID", paste0(names(d.afdi.graminoids)), paste0("plot_graminoid_", names(d.afdi.graminoids)))

dt.dbfd_graminoid <- fd.hill.res.graminoids$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "plot_ID")

names(dt.dbfd_graminoid) <- ifelse(names(dt.dbfd_graminoid) == "plot_ID", paste0(names(dt.dbfd_graminoid)), paste0("plot_graminoid_", names(dt.dbfd_graminoid)))

## graminoid results 
d.afdi.graminoids
dt.dbfd_graminoid
dt.sfd.graminoids

# ---------------------- forbs -------------------- #

forbs <- dt.p[life_form %in% c("Forb"),] %>% 
  dplyr::select(species, cover_percent, plot_ID) %>% 
  ## assign low cover to some of the species
  mutate(cover_percent = ifelse(is.na(cover_percent), 1, cover_percent))

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight.mat.forbs <- forbs %>% 
  dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
  dplyr::select(plot_ID, cover_percent, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover_percent") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("plot_ID"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="plot_ID") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ_forbs <- mFD::asb.sp.summary(asb_sp_w = weight.mat.forbs) 
asb_sp_occ_forbs <- asb_sp_summ_forbs$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res.forbs <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ_forbs, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd.forbs <- alpha.fd.res.forbs$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "plot_ID")
names(dt.sfd.forbs) <- ifelse(names(dt.sfd.forbs) == "plot_ID", paste0(names(dt.sfd.forbs)), paste0("plot_forb_", names(dt.sfd.forbs)))


#### distance based functional diversity metric 
fd.hill.res.forbs <- alpha.fd.hill(asb_sp_w = weight.mat.forbs, sp_dist = fdist)

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices_forbs <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight.mat.forbs,
  ind_vect         = c("fdis", "fmpd",  "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


#alpha_fd_indices_forbs$functional_diversity_indices

## extract functional diversity metrics 
d.afdi.forbs <- alpha_fd_indices_forbs$functional_diversity_indices %>% 
  rownames_to_column(var = "plot_ID") %>% 
  as.data.table() %>% 
  dplyr::select("plot_ID", "fdis", "fmpd", "fori", 
                "fspe")

names(d.afdi.forbs) <- ifelse(names(d.afdi.forbs) == "plot_ID", paste0(names(d.afdi.forbs)), paste0("plot_forb_", names(d.afdi.forbs)))

dt.dbfd_forb <- fd.hill.res.forbs$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "plot_ID")

names(dt.dbfd_forb) <- ifelse(names(dt.dbfd_forb) == "plot_ID", paste0(names(dt.dbfd_forb)), paste0("plot_forb_", names(dt.dbfd_forb)))

## forb results 
setDT(d.afdi.forbs)
setDT(dt.dbfd_forb)
      setDT(dt.sfd.forbs)

# herb results 
setDT(d.afdi.herbs)
      setDT(dt.dbfd_herb)
            setDT(dt.sfd.herbs)

## graminoid results 
setDT(d.afdi.graminoids)
      setDT(dt.dbfd_graminoid)
            setDT(dt.sfd.graminoids)
### plot level combine 

dt.fd.plot.level <- dt.fd.plot.level.raw %>% 
  left_join(dt.dbfd_forb[,.(plot_ID, plot_forb_FD_q1)]) %>% 
  left_join(dt.dbfd_herb[,.(plot_ID, plot_herb_FD_q1)]) %>% 
  left_join(dt.dbfd_graminoid[,.(plot_ID, plot_graminoid_FD_q1)]) %>% 
  left_join(dt.sfd.forbs[,.(plot_ID, plot_forb_nb_fe, plot_forb_fred, plot_forb_fored, plot_forb_fvuln)]) %>% 
  left_join(dt.sfd.herbs[,.(plot_ID, plot_herb_nb_fe, plot_herb_fred, plot_herb_fored, plot_herb_fvuln)]) %>% 
  left_join(dt.sfd.graminoids[,.(plot_ID, plot_graminoid_nb_fe, plot_graminoid_fred, plot_graminoid_fored, plot_graminoid_fvuln)]) 
  
#### Load Data again ####
dt.p <- fread("data/processedData/dataFragments/species_numbers_per_plot_waterberg2024.csv") %>% 
  mutate(
    site_ID = gsub("_P01", "", plot_ID),
    site_ID = gsub("_P02", "", site_ID),
    site_ID = gsub("_P03", "", site_ID),
    site_ID = gsub("_P04", "", site_ID),
    site_ID = gsub("_P05", "", site_ID)
  ) %>% 
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
  ))


##################### alpha diversity ########################
######------------------ site level --------------------######

dt.p.site.raw <- dt.p %>% 
  dplyr::select(life_form, site_ID, species, cover_percent)

dt.p.site <- dt.p.site.raw[, cover_percent := sum(cover_percent, na.rm = T), by = c("species", "site_ID")] %>% 
  unique() %>% 
  mutate(cover_percent = ifelse(is.na(cover_percent), 1, cover_percent),
         cover_percent = ifelse(cover_percent == 0, 1, cover_percent)) 

min(dt.p.site$cover_percent)

#---------------------all plants ---------------------------#

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight.mat.site <- dt.p.site %>% 
  dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
  dplyr::select(site_ID, cover_percent, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover_percent", values_fn = mean) %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("site_ID"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="site_ID") %>% 
  as.matrix()

v2 <- names(weight.mat.site)
v1 <- unique(trait.data$species)

## identify cols that contain only 0
cols_only_zeros <- apply(weight.mat.site, 2, function(col) all(col == 0))
sum(cols_only_zeros, na.rm = T)
colnames_only_zeros <- colnames(weight.mat.site)[cols_only_zeros]
print(colnames_only_zeros)

setdiff(v1, v2)
setdiff(v2, v1)

### Get the occurrence dataframe:
asb_sp_summ.site <- mFD::asb.sp.summary(asb_sp_w = weight.mat.site) 
asb_sp_occ.site <- asb_sp_summ.site$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res.site <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ.site, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd.site <- alpha.fd.res.site$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "site_ID")

summary(dt.sfd.site)
### plot for example reserve 
mFD::alpha.fd.fe.plot(alpha.fd.res.site, plot_asb_nm = "WI_S05")


#### distance based functional diversity metric 

fd.hill.res.site <- alpha.fd.hill(asb_sp_w = weight.mat.site, sp_dist = fdist)

## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices.site <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight.mat.site,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


#alpha_fd_indices.site$functional_diversity_indices

## extract functional diversity metrics 
d.afdi.site <- alpha_fd_indices.site$functional_diversity_indices %>% 
  rownames_to_column(var = "site_ID") %>% 
  as.data.table() %>% 
  dplyr::select("site_ID", "fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                "fspe")

names(d.afdi.site) <- ifelse(names(d.afdi.site) == "site_ID", paste0(names(d.afdi.site)), paste0("site_", names(d.afdi.site)))

dt.dbfd.site <- fd.hill.res.site$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "site_ID")

dt.fd.site.level.raw <- dt.dbfd.site %>% 
  left_join(dt.sfd.site) %>% 
  left_join(d.afdi.site) %>% 
  as.data.table() %>% 
  rename(site_plant_fun_red = fred,
         site_plant_fun_ovred = fored,
         site_plant_fun_div_distq1 = FD_q1, 
         site_plant_fun_ent = nb_fe, 
         site_plant_fvuln = fvuln) %>% 
  dplyr::select(site_ID, site_plant_fun_red, site_plant_fvuln, site_plant_fun_ovred, site_plant_fun_div_distq1, site_plant_fun_ent, site_fric, site_feve) 
  

#-------------------------- herbacous ----------------------------#

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight.mat.site.herb <- dt.p.site[life_form %in% c("Forb", "Graminoid"),] %>% 
  dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
  dplyr::select(site_ID, cover_percent, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover_percent") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("site_ID"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="site_ID") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ.site.herb <- mFD::asb.sp.summary(asb_sp_w = weight.mat.site.herb) 
asb_sp_occ.site.herb <- asb_sp_summ.site.herb$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res.site.herb <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ.site.herb, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd.site.herb <- alpha.fd.res.site.herb$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "site_ID")

names(dt.sfd.site.herb) <- ifelse(names(dt.sfd.site.herb) == "site_ID", paste0(names(dt.sfd.site.herb)), paste0("site_herb_", names(dt.sfd.site.herb)))


summary(dt.sfd.site.herb)

#### distance based functional diversity metric 

fd.hill.res.site.herb <- alpha.fd.hill(asb_sp_w = weight.mat.site.herb, sp_dist = fdist)

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices.site.herb <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight.mat.site.herb,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


alpha_fd_indices.site.herb$functional_diversity_indices

## extract functional diversity metrics 
d.afdi.site.herb <- alpha_fd_indices.site.herb$functional_diversity_indices %>% 
  rownames_to_column(var = "site_ID") %>% 
  as.data.table() %>% 
  dplyr::select("site_ID", "fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                "fspe")

names(d.afdi.site.herb) <- ifelse(names(d.afdi.site.herb) == "site_ID", paste0(names(d.afdi.site.herb)), paste0("site_herb_", names(d.afdi.site.herb)))

dt.dbfd.site.herb <- fd.hill.res.site.herb$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "site_ID")

names(dt.dbfd.site.herb) <- ifelse(names(dt.dbfd.site.herb) == "site_ID", paste0(names(dt.dbfd.site.herb)), paste0("site_herb_", names(dt.dbfd.site.herb)))


## herb res 
setDT(dt.dbfd.site.herb)
setDT(d.afdi.site.herb)
setDT(dt.sfd.site.herb)

#-------------------------- forb ----------------------------#

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight.mat.site.forb <- dt.p.site[life_form %in% c("Forb"),] %>% 
  dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
  dplyr::select(site_ID, cover_percent, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover_percent") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("site_ID"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="site_ID") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ.site.forb <- mFD::asb.sp.summary(asb_sp_w = weight.mat.site.forb) 
asb_sp_occ.site.forb <- asb_sp_summ.site.forb$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res.site.forb <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ.site.forb, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd.site.forb <- alpha.fd.res.site.forb$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "site_ID")

names(dt.sfd.site.forb) <- ifelse(names(dt.sfd.site.forb) == "site_ID", paste0(names(dt.sfd.site.forb)), paste0("site_forb_", names(dt.sfd.site.forb)))

summary(dt.sfd.site.forb)

#### distance based functional diversity metric 

fd.hill.res.site.forb <- alpha.fd.hill(asb_sp_w = weight.mat.site.forb, sp_dist = fdist)

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices.site.forb <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight.mat.site.forb,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


alpha_fd_indices.site.forb$functional_diversity_indices

## extract functional diversity metrics 
d.afdi.site.forb <- alpha_fd_indices.site.forb$functional_diversity_indices %>% 
  rownames_to_column(var = "site_ID") %>% 
  as.data.table() %>% 
  dplyr::select("site_ID", "fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                "fspe")

names(d.afdi.site.forb) <- ifelse(names(d.afdi.site.forb) == "site_ID", paste0(names(d.afdi.site.forb)), paste0("site_forb_", names(d.afdi.site.forb)))

dt.dbfd.site.forb <- fd.hill.res.site.forb$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "site_ID")

names(dt.dbfd.site.forb) <- ifelse(names(dt.dbfd.site.forb) == "site_ID", paste0(names(dt.dbfd.site.forb)), paste0("site_forb_", names(dt.dbfd.site.forb)))


## forb res 
setDT(dt.dbfd.site.forb)
setDT(d.afdi.site.forb)
setDT(dt.sfd.site.forb)

#-------------------------- graminoid ----------------------------#

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight.mat.site.graminoid <- dt.p.site[life_form %in% c("Graminoid"),] %>% 
  dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
  dplyr::select(site_ID, cover_percent, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover_percent") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("site_ID"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="site_ID") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ.site.graminoid <- mFD::asb.sp.summary(asb_sp_w = weight.mat.site.graminoid) 
asb_sp_occ.site.graminoid <- asb_sp_summ.site.graminoid$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res.site.graminoid <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ.site.graminoid, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd.site.graminoid <- alpha.fd.res.site.graminoid$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "site_ID")

names(dt.sfd.site.graminoid) <- ifelse(names(dt.sfd.site.graminoid) == "site_ID", paste0(names(dt.sfd.site.graminoid)), paste0("site_graminoid_", names(dt.sfd.site.graminoid)))

summary(dt.sfd.site.graminoid)

#### distance based functional diversity metric 

fd.hill.res.site.graminoid <- alpha.fd.hill(asb_sp_w = weight.mat.site.graminoid, sp_dist = fdist)

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices.site.graminoid <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight.mat.site.graminoid,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


alpha_fd_indices.site.graminoid$functional_diversity_indices

## extract functional diversity metrics 
d.afdi.site.graminoid <- alpha_fd_indices.site.graminoid$functional_diversity_indices %>% 
  rownames_to_column(var = "site_ID") %>% 
  as.data.table() %>% 
  dplyr::select("site_ID", "fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                "fspe")

names(d.afdi.site.graminoid) <- ifelse(names(d.afdi.site.graminoid) == "site_ID", paste0(names(d.afdi.site.graminoid)), paste0("site_graminoid_", names(d.afdi.site.graminoid)))

dt.dbfd.site.graminoid <- fd.hill.res.site.graminoid$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "site_ID")

names(dt.dbfd.site.graminoid) <- ifelse(names(dt.dbfd.site.graminoid) == "site_ID", paste0(names(dt.dbfd.site.graminoid)), paste0("site_graminoid_", names(dt.dbfd.site.graminoid)))


## graminoid res 
setDT(dt.dbfd.site.graminoid)
setDT(d.afdi.site.graminoid)
setDT(dt.sfd.site.graminoid)

## forb res 
setDT(dt.dbfd.site.forb)
setDT(d.afdi.site.forb)
setDT(dt.sfd.site.forb)

## herb res 
setDT(dt.dbfd.site.herb)
setDT(d.afdi.site.herb)
setDT(dt.sfd.site.herb)

dt.fd.site.level <- dt.fd.site.level.raw %>% 
  left_join(dt.sfd.site.herb[,.(site_ID, site_herb_nb_fe, site_herb_fred, site_herb_fored, site_herb_fvuln)]) %>% 
  left_join(dt.sfd.site.forb[,.(site_ID, site_forb_nb_fe, site_forb_fred, site_forb_fored, site_forb_fvuln)]) %>% 
  left_join(dt.sfd.site.graminoid[,.(site_ID, site_graminoid_nb_fe, site_graminoid_fred, site_graminoid_fored, site_graminoid_fvuln)]) %>% 
  left_join(d.afdi.site.herb[, .(site_ID, site_herb_feve, site_herb_fric)]) %>% 
  left_join(d.afdi.site.forb[, .(site_ID, site_forb_feve, site_forb_fric)]) %>% 
  left_join(d.afdi.site.graminoid[, .(site_ID, site_graminoid_feve, site_graminoid_fric)]) %>% 
  left_join(dt.dbfd.site.herb[, .(site_ID, site_herb_FD_q1)]) %>% 
  left_join(dt.dbfd.site.forb[, .(site_ID, site_forb_FD_q1)]) %>% 
  left_join(dt.dbfd.site.graminoid[, .(site_ID, site_graminoid_FD_q1)]) 
  
#### Load data again ####

dt.p <- fread("data/processedData/dataFragments/species_numbers_per_plot_waterberg2024.csv") %>% 
  mutate(
    site_ID = gsub("_P01", "", plot_ID),
    site_ID = gsub("_P02", "", site_ID),
    site_ID = gsub("_P03", "", site_ID),
    site_ID = gsub("_P04", "", site_ID),
    site_ID = gsub("_P05", "", site_ID)
  ) %>% 
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
  ))


##################### alpha diversity ########################
######------------------ reserve level -------------------#####

dt.p.reserve.raw <- dt.p %>% 
  dplyr::select(life_form, reserve, species, cover_percent)

dt.p.reserve <- dt.p.reserve.raw[, cover_percent := sum(cover_percent, na.rm = T), by = c("species", "reserve")] %>% 
  unique() %>% 
  mutate(cover_percent = as.numeric(ifelse(is.na(cover_percent), 1, cover_percent)),
         cover_percent = as.numeric(ifelse(cover_percent == 0, 1, cover_percent))) %>% unique()
min(dt.p.reserve$cover_percent)
#---------------------all plants ---------------------------#

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
which(duplicated(dt.p.reserve))

weight.mat.reserve <- dt.p.reserve %>% 
  dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
  dplyr::select(reserve, cover_percent, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover_percent", values_fn = mean) %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("reserve"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="reserve") %>% 
  as.matrix()

## identify cols that contain only 0
cols_only_zeros <- apply(weight.mat.reserve, 2, function(col) all(col == 0))
sum(cols_only_zeros, na.rm = T)
colnames_only_zeros <- colnames(weight.mat.reserve)[cols_only_zeros]
print(colnames_only_zeros)


### Get the occurrence dataframe:
asb_sp_summ.reserve <- mFD::asb.sp.summary(asb_sp_w = weight.mat.reserve) 
asb_sp_occ.reserve <- asb_sp_summ.reserve$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res.reserve <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ.reserve, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd.reserve <- alpha.fd.res.reserve$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")

summary(dt.sfd.reserve)


#### distance based functional diversity metric 

fd.hill.res.reserve <- alpha.fd.hill(asb_sp_w = weight.mat.reserve, sp_dist = fdist)

## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices.reserve <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight.mat.reserve,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


alpha_fd_indices.reserve$functional_diversity_indices

## extract functional diversity metrics 
d.afdi.reserve <- alpha_fd_indices.reserve$functional_diversity_indices %>% 
  rownames_to_column(var = "reserve") %>% 
  as.data.table() %>% 
  dplyr::select("reserve", "fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                "fspe")

names(d.afdi.reserve) <- ifelse(names(d.afdi.reserve) == "reserve", paste0(names(d.afdi.reserve)), paste0("reserve_", names(d.afdi.reserve)))

dt.dbfd.reserve <- fd.hill.res.reserve$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")

dt.fd.reserve.level.raw <- dt.dbfd.reserve %>% 
  left_join(dt.sfd.reserve) %>% 
  left_join(d.afdi.reserve) %>% 
  as.data.table() %>% 
  rename(reserve_plant_fun_red = fred,
         reserve_plant_fun_ovred = fored,
         reserve_plant_fun_div_distq1 = FD_q1, 
         reserve_plant_fun_ent = nb_fe, 
         reserve_plant_fvuln = fvuln) %>% 
  dplyr::select(reserve, reserve_plant_fun_red, reserve_plant_fvuln, reserve_plant_fun_ovred, reserve_plant_fun_div_distq1, reserve_plant_fun_ent, reserve_fric, reserve_feve) 


#-------------------------- herbacous ----------------------------#

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight.mat.reserve.herb <- dt.p.reserve[life_form %in% c("Forb", "Graminoid"),] %>% 
  dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
  dplyr::select(reserve, cover_percent, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover_percent") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("reserve"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="reserve") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ.reserve.herb <- mFD::asb.sp.summary(asb_sp_w = weight.mat.reserve.herb) 
asb_sp_occ.reserve.herb <- asb_sp_summ.reserve.herb$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res.reserve.herb <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ.reserve.herb, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd.reserve.herb <- alpha.fd.res.reserve.herb$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")

names(dt.sfd.reserve.herb) <- ifelse(names(dt.sfd.reserve.herb) == "reserve", paste0(names(dt.sfd.reserve.herb)), paste0("reserve_herb_", names(dt.sfd.reserve.herb)))


summary(dt.sfd.reserve.herb)

#### distance based functional diversity metric 

fd.hill.res.reserve.herb <- alpha.fd.hill(asb_sp_w = weight.mat.reserve.herb, sp_dist = fdist)

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices.reserve.herb <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight.mat.reserve.herb,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


alpha_fd_indices.reserve.herb$functional_diversity_indices

## extract functional diversity metrics 
d.afdi.reserve.herb <- alpha_fd_indices.reserve.herb$functional_diversity_indices %>% 
  rownames_to_column(var = "reserve") %>% 
  as.data.table() %>% 
  dplyr::select("reserve", "fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                "fspe")

names(d.afdi.reserve.herb) <- ifelse(names(d.afdi.reserve.herb) == "reserve", paste0(names(d.afdi.reserve.herb)), paste0("reserve_herb_", names(d.afdi.reserve.herb)))

dt.dbfd.reserve.herb <- fd.hill.res.reserve.herb$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")

names(dt.dbfd.reserve.herb) <- ifelse(names(dt.dbfd.reserve.herb) == "reserve", paste0(names(dt.dbfd.reserve.herb)), paste0("reserve_herb_", names(dt.dbfd.reserve.herb)))


## herb res 
setDT(dt.dbfd.reserve.herb)
setDT(d.afdi.reserve.herb)
setDT(dt.sfd.reserve.herb)

#-------------------------- forb ----------------------------#

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight.mat.reserve.forb <- dt.p.reserve[life_form %in% c("Forb"),] %>% 
  dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
  dplyr::select(reserve, cover_percent, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover_percent") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("reserve"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="reserve") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ.reserve.forb <- mFD::asb.sp.summary(asb_sp_w = weight.mat.reserve.forb) 
asb_sp_occ.reserve.forb <- asb_sp_summ.reserve.forb$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res.reserve.forb <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ.reserve.forb, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd.reserve.forb <- alpha.fd.res.reserve.forb$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")

names(dt.sfd.reserve.forb) <- ifelse(names(dt.sfd.reserve.forb) == "reserve", paste0(names(dt.sfd.reserve.forb)), paste0("reserve_forb_", names(dt.sfd.reserve.forb)))

summary(dt.sfd.reserve.forb)

#### distance based functional diversity metric 

fd.hill.res.reserve.forb <- alpha.fd.hill(asb_sp_w = weight.mat.reserve.forb, sp_dist = fdist)

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices.reserve.forb <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight.mat.reserve.forb,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


alpha_fd_indices.reserve.forb$functional_diversity_indices

## extract functional diversity metrics 
d.afdi.reserve.forb <- alpha_fd_indices.reserve.forb$functional_diversity_indices %>% 
  rownames_to_column(var = "reserve") %>% 
  as.data.table() %>% 
  dplyr::select("reserve", "fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                "fspe")

names(d.afdi.reserve.forb) <- ifelse(names(d.afdi.reserve.forb) == "reserve", paste0(names(d.afdi.reserve.forb)), paste0("reserve_forb_", names(d.afdi.reserve.forb)))

dt.dbfd.reserve.forb <- fd.hill.res.reserve.forb$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")

names(dt.dbfd.reserve.forb) <- ifelse(names(dt.dbfd.reserve.forb) == "reserve", paste0(names(dt.dbfd.reserve.forb)), paste0("reserve_forb_", names(dt.dbfd.reserve.forb)))


## forb res 
setDT(dt.dbfd.reserve.forb)
setDT(d.afdi.reserve.forb)
setDT(dt.sfd.reserve.forb)

#-------------------------- graminoid ----------------------------#

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight.mat.reserve.graminoid <- dt.p.reserve[life_form %in% c("Graminoid"),] %>% 
  dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
  dplyr::select(reserve, cover_percent, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover_percent") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("reserve"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames %>% 
  column_to_rownames(var="reserve") %>% 
  as.matrix()

### Get the occurrence dataframe:
asb_sp_summ.reserve.graminoid <- mFD::asb.sp.summary(asb_sp_w = weight.mat.reserve.graminoid) 
asb_sp_occ.reserve.graminoid <- asb_sp_summ.reserve.graminoid$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha.fd.res.reserve.graminoid <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ.reserve.graminoid, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt.sfd.reserve.graminoid <- alpha.fd.res.reserve.graminoid$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")

names(dt.sfd.reserve.graminoid) <- ifelse(names(dt.sfd.reserve.graminoid) == "reserve", paste0(names(dt.sfd.reserve.graminoid)), paste0("reserve_graminoid_", names(dt.sfd.reserve.graminoid)))

summary(dt.sfd.reserve.graminoid)

#### distance based functional diversity metric 

fd.hill.res.reserve.graminoid <- alpha.fd.hill(asb_sp_w = weight.mat.reserve.graminoid, sp_dist = fdist)

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices.reserve.graminoid <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight.mat.reserve.graminoid,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


alpha_fd_indices.reserve.graminoid$functional_diversity_indices

## extract functional diversity metrics 
d.afdi.reserve.graminoid <- alpha_fd_indices.reserve.graminoid$functional_diversity_indices %>% 
  rownames_to_column(var = "reserve") %>% 
  as.data.table() %>% 
  dplyr::select("reserve", "fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                "fspe")

names(d.afdi.reserve.graminoid) <- ifelse(names(d.afdi.reserve.graminoid) == "reserve", paste0(names(d.afdi.reserve.graminoid)), paste0("reserve_graminoid_", names(d.afdi.reserve.graminoid)))

dt.dbfd.reserve.graminoid <- fd.hill.res.reserve.graminoid$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")

names(dt.dbfd.reserve.graminoid) <- ifelse(names(dt.dbfd.reserve.graminoid) == "reserve", paste0(names(dt.dbfd.reserve.graminoid)), paste0("reserve_graminoid_", names(dt.dbfd.reserve.graminoid)))


## graminoid res 
setDT(dt.dbfd.reserve.graminoid)
setDT(d.afdi.reserve.graminoid)
setDT(dt.sfd.reserve.graminoid)

## forb res 
setDT(dt.dbfd.reserve.forb)
setDT(d.afdi.reserve.forb)
setDT(dt.sfd.reserve.forb)

## herb res 
setDT(dt.dbfd.reserve.herb)
setDT(d.afdi.reserve.herb)
setDT(dt.sfd.reserve.herb)

dt.fd.reserve.level <- dt.fd.reserve.level.raw %>% 
  left_join(dt.sfd.reserve.herb[,.(reserve, reserve_herb_nb_fe, reserve_herb_fred, reserve_herb_fored, reserve_herb_fvuln)]) %>% 
  left_join(dt.sfd.reserve.forb[,.(reserve, reserve_forb_nb_fe, reserve_forb_fred, reserve_forb_fored, reserve_forb_fvuln)]) %>% 
  left_join(dt.sfd.reserve.graminoid[,.(reserve, reserve_graminoid_nb_fe, reserve_graminoid_fred, reserve_graminoid_fored, reserve_graminoid_fvuln)]) %>% 
  left_join(d.afdi.reserve.herb[, .(reserve, reserve_herb_feve, reserve_herb_fric)]) %>% 
  left_join(d.afdi.reserve.forb[, .(reserve, reserve_forb_feve, reserve_forb_fric)]) %>% 
  left_join(d.afdi.reserve.graminoid[, .(reserve, reserve_graminoid_feve, reserve_graminoid_fric)]) %>% 
  left_join(dt.dbfd.reserve.herb[, .(reserve, reserve_herb_FD_q1)]) %>% 
  left_join(dt.dbfd.reserve.forb[, .(reserve, reserve_forb_FD_q1)]) %>% 
  left_join(dt.dbfd.reserve.graminoid[, .(reserve, reserve_graminoid_FD_q1)]) 


########################## beta diversity within reserve ##################################

# Has to be calculated for each site seperately
# Preparation steps are as usual, but we'll loop through the reserves 

dt.beta <- data.table(
  mean_beta_divq0 = NA,
  mean_beta_divq1 = NA, 
  mean_beta_divq2 = NA, 
  reserve_sor_beta_div = NA,
  reserve = NA
) %>% filter(!is.na(reserve))

for(reserve_name in unique(dt.p$reserve)){

reserve.data.raw <- dt.p %>% filter(reserve %in% c(reserve_name)) %>% 
  dplyr::select(site_ID, species, cover_percent)

reserve.data <- reserve.data.raw[, cover_percent := mean(cover_percent, na.rm = T), by = c("species", "site_ID")] %>% 
  unique() %>% 
  mutate(cover_percent = ifelse(is.na(cover_percent), 1, cover_percent)) 


### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
reserve.weights <- reserve.data %>% 
  dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
  dplyr::select(site_ID, cover_percent, species) %>% 
  pivot_wider(names_from = "species", values_from = "cover_percent") %>% 
  as.data.table() %>% 
  mutate(across(where(is.list), ~ sapply(., toString)),
         across(where(is.character) & !all_of("site_ID"), ~ as.numeric(.)),
         across(everything(), ~ ifelse(is.na(.), 0, .))
  ) %>% 
  remove_rownames() %>% 
  column_to_rownames(var="site_ID") %>% 
  as.matrix()


## calculate functional beta div 
tr_cat_fdist <- tr_cat
tr_cat_fdist$trait_weight <- c(1, 1, 1, 1, 1) #
fdist <- mFD::funct.dist(sp_tr = sp_tr, tr_cat = tr_cat_fdist, metric = "gower")


#### distance based functional diversity metric 

beta.div <- beta.fd.hill(asb_sp_w = reserve.weights, sp_dist = fdist)

## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)
mFD::dist.to.df(list_dist = list("FDq1" = beta.div$"beta_fd_q"$"q1"))
##


### calculate beta diversity using the vegan approach (Soerensen dissimilarity index)

tmpVeg <- betadiver(reserve.weights, method = "w") %>% mean(na.rm = T)

## extract mean beta div
tmp <- data.table(
  mean_beta_divq0 = mean(beta.div$beta_fd_q$q0),
  mean_beta_divq1 = mean(beta.div$beta_fd_q$q1),
  mean_beta_divq2 = mean(beta.div$beta_fd_q$q2),
  reserve_sor_beta_div = tmpVeg,
  reserve = reserve_name
)

dt.beta <- rbind(dt.beta, tmp)

print(paste0(reserve_name, " done"))

}

dt.beta

## use vegan approach 
library(vegan)


########################## beta diversity within sites ##################################

# Has to be calculated for each site seperately
# Preparation steps are as usual, but we'll loop through the reserves 

dt.beta.site <- data.table(
  site_mean_beta_divq0 = NA,
  site_mean_beta_divq1 = NA, 
  site_mean_beta_divq2 = NA, 
  site_sor_beta_div = NA,
  site_ID = NA
) %>% filter(!is.na(site_ID))

for(site in unique(dt.p$site_ID)){
  
  site.data.raw <- dt.p %>% filter(site_ID %in% c(site)) %>% 
    dplyr::select(plot_ID, species, cover_percent)
  
  site.data <- site.data.raw[, cover_percent := mean(cover_percent, na.rm = T), by = c("species", "plot_ID")] %>% 
    unique() %>% 
    mutate(cover_percent = ifelse(is.na(cover_percent), 1, cover_percent)) 
  
  
  ### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
  site.weights <- site.data %>% 
    dplyr::filter(species %in% c(all_of(unique(trait.data$species)))) %>% 
    dplyr::select(plot_ID, cover_percent, species) %>% 
    pivot_wider(names_from = "species", values_from = "cover_percent") %>% 
    as.data.table() %>% 
    mutate(across(where(is.list), ~ sapply(., toString)),
           across(where(is.character) & !all_of("plot_ID"), ~ as.numeric(.)),
           across(everything(), ~ ifelse(is.na(.), 0, .))
    ) %>% 
    remove_rownames() %>% 
    column_to_rownames(var="plot_ID") %>% 
    as.matrix()
  
  
  ## calculate 
  tr_cat_fdist <- tr_cat
  tr_cat_fdist$trait_weight <- c(1, 1, 1, 1, 1) #
  fdist <- mFD::funct.dist(sp_tr = sp_tr, tr_cat = tr_cat_fdist, metric = "gower")
  
  
  #### distance based functional diversity metric 
  
  beta.div.site <- beta.fd.hill(asb_sp_w = site.weights, sp_dist = fdist)
  
  ## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)
  mFD::dist.to.df(list_dist = list("FDq1" = beta.div$"beta_fd_q"$"q1"))
  
  
  ### calculate beta diversity using the vegan approach (Soerensen dissimilarity index)
  
  tmpVegSite <- betadiver(site.weights, method = "w") %>% mean(na.rm = T)

  
  ## extract mean beta div
  tmp <- data.table(
    site_mean_beta_divq0 = mean(beta.div.site$beta_fd_q$q0),
    site_mean_beta_divq1 = mean(beta.div.site$beta_fd_q$q1),
    site_mean_beta_divq2 = mean(beta.div.site$beta_fd_q$q2),
    site_sor_beta_div = tmpVegSite, 
    site_ID = site
  )
  
  dt.beta.site <- rbind(dt.beta.site, tmp)
  
  print(paste0(site, " done"))
  
}

dt.beta.site


######### calculate evenness and shannon diversity ########################
library("chemodiv")
library(vegan)

### site 
evenness.raw <- chemodiv::calcDiv(weight.mat.site, type = "PielouEven")

site_IDs <- weight.mat.site %>% 
  as.data.frame() %>%
  rownames_to_column(var = "site_ID") %>% 
  dplyr::select(site_ID)

evenness <- cbind(evenness.raw, site_IDs) %>% 
  rename(site_plant_evenness_pielou = PielouEven)

shannon.site <- diversity(weight.mat.site)
dt.shan.site <- data.table(
  site_ID = names(shannon.site),
  shannon_site = unname(shannon.site)
)

### plot 

evenness.raw.plot <- chemodiv::calcDiv(weight.mat, type = "PielouEven")

plot_IDs <- weight.mat %>% 
  as.data.frame() %>%
  rownames_to_column(var = "plot_ID") %>% 
  dplyr::select(plot_ID)

evenness.plot <- cbind(evenness.raw.plot, plot_IDs) %>% 
  rename(plot_plant_evenness_pielou = PielouEven)

shannon.plot <- diversity(weight.mat)
dt.shan.plot <- data.table(
  plot_ID = names(shannon.plot),
  shannon_plot = unname(shannon.plot)
)

### reserve 
evenness.raw.reserve <- chemodiv::calcDiv(weight.mat.reserve, type = "PielouEven")

reserves <- weight.mat.reserve %>% 
  as.data.frame() %>%
  rownames_to_column(var = "reserve") %>% 
  dplyr::select(reserve)

evenness.reserve <- cbind(evenness.raw.reserve, reserves) %>% 
  rename(reserve_plant_evenness_pielou = PielouEven)

shannon.reserve <- diversity(weight.mat.reserve)
dt.shan.reserve <- data.table(
  reserve = names(shannon.reserve),
  shannon_reserve = unname(shannon.reserve)
)

############## combine everything ################
res.met <- fread("data/processedData/dataFragments/reserve_meta_waterberg2024.csv")
plot.met <- fread("data/processedData/dataFragments/plot_meta_waterberg2024.csv")

dt.div <- fread("data/processedData/dataFragments/species_numbers_per_site_waterberg2024.csv") %>% mutate(
  total_plant_species_richness_site = (graminoids_per_site + forbs_per_site + woodies_per_site),
  herbacous_species_richness_site = (graminoids_per_site + forbs_per_site), 
  total_plant_species_richness_reserve = (graminoids_per_reserve + forbs_per_reserve + woodies_per_reserve),
  herbacous_species_richness_reserve = (graminoids_per_reserve + forbs_per_reserve), 
  
)

dt.div.plot <- fread("data/processedData/dataFragments/plot_traits_and_div_waterberg2024.csv") %>% 
  mutate(
    herbacous_species_richness_plot = (graminoids_per_plot + forbs_per_plot), 
  )



#### load lidar
res.lid.raw <- fread("data/processedData/dataFragments/LidarResultsWaterberg2024Radius20m.csv")
names(res.lid.raw)
res.lid.raw2 <- res.lid.raw %>% 
  dplyr::select(
    name, adjusted_mean_3d, adjusted_mean_3d_woody, adjusted_mean_3d_herb,
    point_fraction, fraction_points_woody, point_fraction_understory,
    sd_3d,  sd_3d_woody, sd_3d_herb, 
    sd_fraction_points_partial, sd_adjusted_3d_partial,
  )

names(res.lid.raw2) <- paste0("plot_lidar_", names(res.lid.raw2))

plot.lid <- res.lid.raw2 %>%
  rename(plot_ID = plot_lidar_name) %>% 
  mutate(
    site_ID = gsub("_P01", "", plot_ID),
    site_ID = gsub("_P02", "", site_ID),
    site_ID = gsub("_P03", "", site_ID),
    site_ID = gsub("_P04", "", site_ID),
    site_ID = gsub("_P05", "", site_ID),
    site_ID = ifelse(grepl("SU_S04", site_ID), "SU_S04", site_ID),
    reserve = case_when(
      grepl("LA", site_ID) ~ "Lapalala", 
      grepl("JE", site_ID) ~ "Jembisa", 
      grepl("WI", site_ID) ~ "Willowisp", 
      grepl("SY", site_ID) ~ "Syringa Sands", 
      grepl("SU", site_ID) ~ "Summerplace", 
      grepl("DA", site_ID) ~ "Dabchick", 
      grepl("AN", site_ID) ~ "Ant's Farm", 
      grepl("KA", site_ID) ~ "Kaingo", 
      grepl("SW", site_ID) ~ "Swebeswebe", 
      grepl("MA", site_ID) ~ "Marakele")
  ) %>% as.data.table()
  

site.lid <- plot.lid %>% 
  group_by(site_ID) %>% 
  summarize(site_adj_mean_3d = mean(plot_lidar_adjusted_mean_3d, na.rm = T),
            site_sd_adj_mean_3d = sd(plot_lidar_adjusted_mean_3d, na.rm = T), 
            
            site_adj_mean_3d_woody = mean(plot_lidar_adjusted_mean_3d_woody, na.rm = T),
            site_sd_adj_mean_3d_woody = sd(plot_lidar_adjusted_mean_3d_woody, na.rm = T),
            site_adj_mean_3d_herb = mean(plot_lidar_adjusted_mean_3d_herb, na.rm = T),
            site_sd_adj_mean_3d_herb = sd(plot_lidar_adjusted_mean_3d_herb, na.rm = T),
            
            site_mean_return_fraction = mean(plot_lidar_point_fraction, na.rm = T),
            site_mean_return_fraction_woody = mean(plot_lidar_fraction_points_woody, na.rm = T),
            site_sd_return_fraction_woody = sd(plot_lidar_fraction_points_woody, na.rm = T),
            site_sd_return_fraction = sd(plot_lidar_point_fraction, na.rm = T)) %>% 
  as.data.table()

reserve.lid <- plot.lid %>% 
  group_by(reserve) %>% 
    summarize(reserve_adj_mean_3d = mean(plot_lidar_adjusted_mean_3d, na.rm = T),
              reserve_sd_adj_mean_3d = sd(plot_lidar_adjusted_mean_3d, na.rm = T), 
              
              reserve_adj_mean_3d_woody = mean(plot_lidar_adjusted_mean_3d_woody, na.rm = T),
              reserve_sd_adj_mean_3d_woody = sd(plot_lidar_adjusted_mean_3d_woody, na.rm = T),
              reserve_adj_mean_3d_herb = mean(plot_lidar_adjusted_mean_3d_herb, na.rm = T),
              reserve_sd_adj_mean_3d_herb = sd(plot_lidar_adjusted_mean_3d_herb, na.rm = T),
              
              reserve_mean_return_fraction = mean(plot_lidar_point_fraction, na.rm = T),
              reserve_mean_return_fraction_woody = mean(plot_lidar_fraction_points_woody, na.rm = T),
              reserve_sd_return_fraction_woody = sd(plot_lidar_fraction_points_woody, na.rm = T),
              reserve_sd_return_fraction = sd(plot_lidar_point_fraction, na.rm = T)) %>% 
      as.data.table()

#load camera trap data: 
cameraTrapData <- fread("data/processedData/dataFragments/cameraTrapObs.csv")


dt.comb <- dt.fd.plot.level %>% 
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
    grepl("MA", site_ID) ~ "Marakele" 
  )) %>%
  left_join(dt.fd.site.level) %>% 
  left_join(dt.fd.reserve.level) %>% 
  left_join(res.met) %>% 
  left_join(dt.beta) %>% 
  left_join(dt.beta.site) %>% 
  left_join(plot.lid) %>% 
  left_join(site.lid) %>% 
  left_join(reserve.lid) %>% 
  left_join(plot.met) %>% 
  left_join(dt.div) %>% 
  left_join(evenness) %>% 
  left_join(evenness.plot) %>% 
  left_join(evenness.reserve) %>% 
  left_join(dt.shan.plot) %>% 
  left_join(dt.shan.site) %>% 
  left_join(dt.shan.reserve) %>% 
  left_join(cameraTrapData) %>% 
  left_join(dt.max.cover) %>% 
  left_join(dt.div.plot) %>% unique() 
  # rename(tree_cover_reserve = tree_cover_mean, 
  #        tree_cover_plot = tree_cover_mean_plot
  #        )

fwrite(dt.comb, "data/processedData/cleanData/waterberg2024DataPrelim.csv")
names(dt.comb)
n_distinct(dt.comb$tree_cover_site)



