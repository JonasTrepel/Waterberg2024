### calculate plot functional diversity 


library(mFD)
library(tidyverse)
library(data.table)
library(vegan)
library(tidylog)


dt_rich <- fread("data/processed_data/data_fragments/species_numbers_per_plot_waterberg2024.csv") 

dt_sp <- fread("data/processed_data/data_fragments/plot_species_waterberg2024.csv")

dt_plot <- dt_sp
quantile(dt_plot$cover_percent, na.rm = T)
dt_sp$site_ID

### calc max cover #######
library("abdiv")
x <- c(15, 6, 4, 0, 3, 0)
max(x)/sum(x)
berger_parker_d(x) # 15 / 28

simpson(x)
vegan::diversity(x, index = "simpson")

max_3_plot <- dt_plot %>%
  dplyr::select(plot_ID, site_ID, reserve, cover_percent) %>%
  arrange(reserve, site_ID, plot_ID, desc(cover_percent)) %>%  
  group_by(reserve, site_ID, plot_ID) %>%
  slice_max(cover_percent, n = 3) %>% 
  summarise(sum_cover_percent = sum(cover_percent, na.rm = TRUE))

max_cover_plot <- dt_plot %>% 
  dplyr::select(plot_ID, site_ID, reserve, cover_percent) %>% 
  left_join(max_3_plot) %>% 
  group_by(reserve, site_ID, plot_ID) %>% 
  summarize(max_cover_plot = max(cover_percent, na.rm = T), 
            community_dominance_plot = max(sum_cover_percent)/sum(cover_percent, na.rm = T), 
            berger_parker_plot = max(cover_percent, na.rm = T)/sum(cover_percent, na.rm = T), 
            simpson_dominance_plot = vegan::diversity(cover_percent, index = "simpson"))

max_3_site <- dt_plot %>%
  dplyr::select(site_ID, reserve,species, cover_percent) %>%
  group_by(reserve, site_ID, species) %>% 
  summarize(cover_percent = sum(cover_percent, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(reserve, site_ID, desc(cover_percent)) %>%  
  group_by(reserve, site_ID) %>%
  slice_max(cover_percent, n = 3) %>% 
  summarise(sum_cover_percent = sum(cover_percent, na.rm = TRUE))


max_cover_site <- dt_plot %>% 
  dplyr::select(site_ID, species, cover_percent, site_ID, reserve) %>% 
  group_by(reserve, site_ID, species) %>% 
  summarize(cover_percent = sum(cover_percent, na.rm = T)) %>% 
  dplyr::select(-species) %>% 
  left_join(max_3_site) %>%
  group_by(reserve, site_ID) %>% 
  summarize(max_cover_site = max(cover_percent, na.rm = T)/5, 
            community_dominance_site = max(sum_cover_percent)/sum(cover_percent, na.rm = T), 
            berger_parker_site = max(cover_percent, na.rm = T)/sum(cover_percent, na.rm = T), 
            simpson_dominance_site = vegan::diversity(cover_percent, index = "simpson"))

max_3_reserve <- dt_plot %>%
  dplyr::select(reserve, species, cover_percent) %>%
  group_by(reserve, species) %>% 
  summarize(cover_percent = sum(cover_percent, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(reserve, desc(cover_percent)) %>%  
  group_by(reserve) %>%
  slice_max(cover_percent, n = 3) %>% 
  summarise(sum_cover_percent = sum(cover_percent, na.rm = TRUE))

max_cover_reserve <- dt_plot %>% 
  dplyr::select(site_ID, species, cover_percent, site_ID, reserve) %>% 
  group_by(reserve, species) %>% 
  summarize(cover_percent = sum(cover_percent, na.rm = T)) %>% 
  left_join(max_3_reserve) %>%
  group_by(reserve)  %>% 
  summarize(reserve_max_cover = max(cover_percent, na.rm = T)/25, 
            community_dominance_reserve = max(sum_cover_percent)/sum(cover_percent, na.rm = T), 
            berger_parker_reserve = max(cover_percent, na.rm = T)/sum(cover_percent, na.rm = T), 
            simpson_dominance_reserve = vegan::diversity(cover_percent, index = "simpson"))


dt_dominance <- max_cover_plot %>% 
  left_join(max_cover_site) %>% 
  left_join(max_cover_reserve)


###### calculate FD -----------------------------------


### categorical traits have to be coded as factor 
n <- 435 #number of species

#number of breakpoints using Sturges rule
k <- ceiling(log2(n) + 1)

traits <- dt_sp %>% 
  dplyr::select(plot_ID, species, growth_form, height_cm, hairs, leaf_size, bulk_density, reproductive_height) %>% 
  mutate(hairs = case_when(
    .default = hairs, 
    hairs %in% c("Absent", "absent") ~ "Absent", 
    hairs %in% c("stems", "stems and branches") ~ "Stems", 
    hairs %in% c("leaves") ~ "Leaves", 
    hairs %in% c("both") ~ "Leaves and Stems")) %>% 
  mutate(growth_form = as.factor(growth_form), 
         height_cm = as.numeric(height_cm), 
         #  height_bins = as.factor(cut_number(height_cm, n = k)),
         hairs = as.factor(hairs), 
         leaf_size = as.factor(leaf_size),
         bulk_density = as.factor(bulk_density), 
         reproductive_height = as.numeric(reproductive_height))

get_mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

glimpse(traits)
trait_data <- traits %>%
  group_by(species) %>% 
  mutate(
  growth_form = get_mode(growth_form),
  height_cm = max(height_cm, na.rm = T), 
  reproductive_height = max(reproductive_height, na.rm = T), 
  hairs = get_mode(hairs), 
  leaf_size = get_mode(leaf_size), 
  bulk_density = get_mode(bulk_density)) %>%
  ungroup() %>% 
  mutate(height_bins = as.factor(cut_number(height_cm, n = k))) %>%
  # mutate(height_bins = case_when( #alternative height category
  #   height_cm < 10 ~ "1",
  #   height_cm >= 10 &  height_cm < 30 ~ "2",
  #   height_cm >= 30 &  height_cm < 50 ~ "3",
  #   height_cm >= 50 &  height_cm < 75 ~ "4",
  #   height_cm >= 75 &  height_cm < 100 ~ "5",
  #   height_cm >= 100 &  height_cm < 200 ~ "6",
  #   height_cm >= 200 ~ "7"),
  #   height_bins = as.factor(height_bins)) %>%
  dplyr::select(-plot_ID, -reproductive_height, -height_cm) %>% ## remove reproductive height for now as we don't have enough data 
  unique() %>%
  filter(complete.cases(.)) %>% 
  mutate(growth_form = factor(growth_form, levels = c(
                                "Tree single-stemmed",
                                "Tree multi-stemmed",
                                "Shrub single-stemmed",
                                "Shrub multi-stemmed",
                                "Creeping graminoid",
                                "Sparse upward graminoid",
                                "Angry tussock graminoid",
                                "Relaxed tussock graminoid",
                                "Large graminoid",
                                "Creeping forb",
                                "Round forb",
                                "Straight forb",
                                "Cussion forb",
                                "Messy forb")),
    hairs = factor(hairs, levels = c(
      "Absent", "Stems", "Leaves", "Leaves and Stems")),
    leaf_size = factor(leaf_size, levels = c(
      "Absent", "Thin linear", "Thick linear", "Micro", "Macro", "Mega")),
    bulk_density = factor(bulk_density, levels = c(
      "low", "medium", "high", "extremely thick"))
    )  %>%
    filter(complete.cases(.)) 

levels(trait_data$growth_form)
levels(trait_data$bulk_density)
levels(trait_data$hairs)
levels(trait_data$leaf_size)
levels(trait_data$height_bins)

unique(trait_data$growth_form)
sum(is.na((trait_data$bulk_density)))
levels(trait_data$hairs)
levels(trait_data$leaf_size)
levels(trait_data$height_bins)


plot(trait_data$height_bins)
## remove reproductive height for now as we don't have enough data 
summary(trait_data)
unique(trait_data$height_bins)

#### trait data: 
trait_data 

### build trait data frame for functions 
sp_tr <-  trait_data %>% dplyr::select(species, growth_form, height_bins, hairs, leaf_size, bulk_density) %>% 
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

dt_sp <- fread("data/processed_data/data_fragments/plot_species_waterberg2024.csv")

##################### alpha diversity ########################

######------------------ plot level --------------------######

# ---------------------- all life forms -------------------- #

plants <- dt_sp %>% 
  dplyr::select(species, cover_percent, plot_ID) %>% 
  ## assign low cover to some of the species
  mutate(cover_percent = ifelse(is.na(cover_percent), 1, cover_percent))

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight_mat <- plants %>% 
  dplyr::filter(species %in% c(unique(trait_data$species))) %>% 
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
asb_sp_summ <- mFD::asb.sp.summary(asb_sp_w = weight_mat) 
asb_sp_occ <- asb_sp_summ$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha_fd_res <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt_sfd <- alpha_fd_res$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "plot_ID")

summary(dt_sfd)

## distance based functional diversity metrics. 


#### as we're dealing with categorical traits we have to use the gower distance 
tr_cat_fdist <- tr_cat
tr_cat_fdist$trait_weight <- c(1, 1, 1, 1, 1) ## 
fdist <- mFD::funct.dist(sp_tr = sp_tr, tr_cat = tr_cat_fdist, metric = "gower")


#### distance based functional diversity metric 
fd_hill_res <- alpha.fd.hill(asb_sp_w = weight_mat, sp_dist = fdist)

## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)

#### get functional spaces 

fspaces_quality <- mFD::quality.fspaces(
  sp_dist = fdist,
)
round(fspaces_quality$"quality_fspaces", 3) 
### lowest number is the best one

#### get matrix of species coordinates 

sp_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"

#### compute functional richeness (volume occupied in multidimensional space)
weight_mat_multi <- weight_mat[!(rownames(weight_mat) %in% c("SW_S03_P01", "KA_S01_P03", "KA_S03_P03", "SW_S03_P05")), ]
# plots need at least 5 species. Those above have three or four. I will assign 95 % of the lowest value

alpha_fd_indices <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight_mat_multi,
  ind_vect         = c("fric"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


alpha_fd_indices$functional_diversity_indices
hist(alpha_fd_indices$functional_diversity_indices$fric)
quantile(alpha_fd_indices$functional_diversity_indices$fric, c(0, 0.05, 0.95, 1), na.rm = T)
min_fric <- min(alpha_fd_indices$functional_diversity_indices$fric)


dt_multi_plus <- data.table(
  plot_ID = c("SW_S03_P01", "KA_S01_P03", "KA_S03_P03", "SW_S03_P05"),
  fric = rep(0.9*min_fric, 4)
)

## extract functional diversity metrics 
d_afdi <- alpha_fd_indices$functional_diversity_indices %>% 
  rownames_to_column(var = "plot_ID") %>% 
  as.data.table() %>% 
  dplyr::select("plot_ID", "fric") %>% 
  rbind(dt_multi_plus)


names(d_afdi) <- ifelse(names(d_afdi) == "plot_ID", paste0(names(d_afdi)), paste0("plot_", names(d_afdi)))

dt_dbfd <- fd_hill_res$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "plot_ID")

dt_fd_plot <- dt_dbfd %>% 
  left_join(dt_sfd) %>% 
  left_join(d_afdi) %>% 
  as.data.table() %>% 
  rename(functional_redundancy_plot = fred,
         functional_diversity_plot = FD_q1, 
         functional_richness_plot = plot_fric, 
         functional_vulnerability_plot = fvuln) %>% 
  dplyr::select(plot_ID, functional_vulnerability_plot, functional_richness_plot,
                functional_diversity_plot, functional_redundancy_plot) %>% 
  mutate(
    site_ID = gsub("_P01", "", plot_ID),
    site_ID = gsub("_P02", "", site_ID),
    site_ID = gsub("_P03", "", site_ID),
    site_ID = gsub("_P04", "", site_ID),
    site_ID = gsub("_P05", "", site_ID)
  )


##################### alpha diversity ########################
######------------------ site level --------------------######

dt_sp_site_raw <- dt_sp %>% 
  dplyr::select(site_ID, species, cover_percent)

dt_sp_site <- dt_sp_site_raw %>% 
  group_by(species, site_ID) %>% 
  summarize(cover_percent = sum(cover_percent, na.rm = T)) %>%
  unique() %>% 
  mutate(cover_percent = ifelse(is.na(cover_percent), 1, cover_percent),
         cover_percent = ifelse(cover_percent == 0, 1, cover_percent)) 

min(dt_sp_site$cover_percent)

#---------------------all plants ---------------------------#

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
weight_mat_site <- dt_sp_site %>% 
  dplyr::filter(species %in% c(all_of(unique(trait_data$species)))) %>% 
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

v2 <- colnames(weight_mat_site)
v1 <- unique(trait_data$species)

## identify cols that contain only 0
cols_only_zeros <- apply(weight_mat_site, 2, function(col) all(col == 0))
sum(cols_only_zeros, na.rm = T)
colnames_only_zeros <- colnames(weight_mat_site)[cols_only_zeros]
print(colnames_only_zeros)

setdiff(v1, v2)
setdiff(v2, v1)
#none - good. 

### Get the occurrence dataframe:
asb_sp_summ_site <- mFD::asb.sp.summary(asb_sp_w = weight_mat_site) 
asb_sp_occ_site <- asb_sp_summ_site$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha_fd_res_site <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ_site, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt_sfd_site <- alpha_fd_res_site$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "site_ID")

summary(dt_sfd_site)
### plot for example reserve 
mFD::alpha.fd.fe.plot(alpha_fd_res_site, plot_asb_nm = "WI_S05")


#### distance based functional diversity metric 

fd_hill_res_site <- alpha.fd.hill(asb_sp_w = weight_mat_site, sp_dist = fdist)

## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices_site <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight_mat_site,
  ind_vect         = c("fric"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


#alpha_fd_indices_site$functional_diversity_indices

## extract functional diversity metrics 
d_afdi_site <- alpha_fd_indices_site$functional_diversity_indices %>% 
  rownames_to_column(var = "site_ID") %>% 
  as.data.table() %>% 
  dplyr::select("site_ID", "fric")

names(d_afdi_site) <- ifelse(names(d_afdi_site) == "site_ID", paste0(names(d_afdi_site)), paste0("site_", names(d_afdi_site)))

dt_dbfd_site <- fd_hill_res_site$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "site_ID")

dt_fd_site <- dt_dbfd_site %>% 
  left_join(dt_sfd_site) %>% 
  left_join(d_afdi_site) %>% 
  as.data.table() %>% 
  rename(functional_redundancy_site = fred,
         functional_diversity_site = FD_q1, 
         functional_richness_site = site_fric, 
         functional_vulnerability_site = fvuln) %>% 
  dplyr::select(site_ID, functional_vulnerability_site, functional_richness_site,
                functional_diversity_site, functional_redundancy_site)

hist(dt_fd_site$functional_richness_site)
hist(dt_fd_site$functional_diversity_site)
hist(dt_fd_site$functional_redundancy_site)

##################### alpha diversity ########################
######------------------ reserve level -------------------#####

dt_sp_reserve_raw <- dt_sp %>% 
  dplyr::select(life_form, reserve, species, cover_percent)

dt_sp_reserve <- dt_sp_reserve_raw %>% 
  group_by(species, reserve) %>% 
  summarize(cover_percent = sum(cover_percent, na.rm = T)) %>% 
  unique() %>% 
  mutate(cover_percent = as.numeric(ifelse(is.na(cover_percent), 1, cover_percent)),
         cover_percent = as.numeric(ifelse(cover_percent == 0, 1, cover_percent))) %>% unique()
min(dt_sp_reserve$cover_percent)
#---------------------all plants ---------------------------#

### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
which(duplicated(dt_sp_reserve))

weight_mat_reserve <- dt_sp_reserve %>% 
  dplyr::filter(species %in% c(all_of(unique(trait_data$species)))) %>% 
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
cols_only_zeros <- apply(weight_mat_reserve, 2, function(col) all(col == 0))
sum(cols_only_zeros, na.rm = T)
colnames_only_zeros <- colnames(weight_mat_reserve)[cols_only_zeros]
print(colnames_only_zeros)


### Get the occurrence dataframe:
asb_sp_summ_reserve <- mFD::asb.sp.summary(asb_sp_w = weight_mat_reserve) 
asb_sp_occ_reserve <- asb_sp_summ_reserve$'asb_sp_occ'


### compute metrics for alpha functional diversity 
alpha_fd_res_reserve <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_occ_reserve, 
  sp_to_fe         = fe,
  ind_nm           = c('fred', 'fored', 'fvuln'),
  check_input      = TRUE, 
  details_returned = TRUE)


## extract simple functional diversity metrics
dt_sfd_reserve <- alpha_fd_res_reserve$asb_fdfe %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")

summary(dt_sfd_reserve)


#### distance based functional diversity metric 

fd_hill_res_reserve <- alpha.fd.hill(asb_sp_w = weight_mat_reserve, sp_dist = fdist)

## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)

#### compute alpha diversity indices in a multidimensional space 

alpha_fd_indices_reserve <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = weight_mat_reserve,
  ind_vect         = c("fric"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


alpha_fd_indices_reserve$functional_diversity_indices

## extract functional diversity metrics 
d_afdi_reserve <- alpha_fd_indices_reserve$functional_diversity_indices %>% 
  rownames_to_column(var = "reserve") %>% 
  as.data.table() %>% 
  dplyr::select("reserve", "fric")

names(d_afdi_reserve) <- ifelse(names(d_afdi_reserve) == "reserve", paste0(names(d_afdi_reserve)), paste0("reserve_", names(d_afdi_reserve)))

dt_dbfd_reserve <- fd_hill_res_reserve$asb_FD_Hill %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "reserve")

dt_fd_reserve <- dt_dbfd_reserve %>% 
  left_join(dt_sfd_reserve) %>% 
  left_join(d_afdi_reserve) %>% 
  as.data.table() %>% 
  rename(functional_redundancy_reserve = fred,
         functional_diversity_reserve = FD_q1, 
         functional_richness_reserve = reserve_fric, 
         functional_vulnerability_reserve = fvuln) %>% 
  dplyr::select(reserve, functional_vulnerability_reserve, functional_richness_reserve,
                functional_diversity_reserve, functional_redundancy_reserve)

hist(dt_fd_reserve$functional_richness_reserve)
hist(dt_fd_reserve$functional_diversity_reserve)
hist(dt_fd_reserve$functional_redundancy_reserve)


########################## beta diversity within reserve ##################################

# Has to be calculated for each site seperately
# Preparation steps are as usual, but we'll loop through the reserves 

dt_beta <- data.table(
  mean_beta_divq0 = NA,
  mean_beta_divq1 = NA, 
  mean_beta_divq2 = NA, 
  reserve_sor_beta_div = NA,
  reserve = NA
) %>% filter(!is.na(reserve))

for(reserve_name in unique(dt_sp$reserve)){
  
  reserve_data_raw <- dt_sp %>% filter(reserve %in% c(reserve_name)) %>% 
    dplyr::select(site_ID, species, cover_percent)
  
  reserve_data <- reserve_data_raw %>% 
    group_by(species, site_ID) %>% 
    summarize(cover_percent = sum(cover_percent, na.rm = T)) %>% 
    unique() %>% 
    mutate(cover_percent = as.numeric(ifelse(is.na(cover_percent), 1, cover_percent)),
           cover_percent = as.numeric(ifelse(cover_percent == 0, 1, cover_percent))) %>% unique()
  
  
  ### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
  reserve_weights <- reserve_data %>% 
    dplyr::filter(species %in% c(all_of(unique(trait_data$species)))) %>% 
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
  
  beta_div <- beta.fd.hill(asb_sp_w = reserve_weights, sp_dist = fdist)
  
  ## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)
  mFD::dist.to.df(list_dist = list("FDq1" = beta_div$"beta_fd_q"$"q1"))
  ##
  
  
  ### calculate beta diversity using the vegan approach (Soerensen dissimilarity index)
  
  tmp_veg <- betadiver(reserve_weights, method = "w") %>% mean(na.rm = T)
  
  ## extract mean beta div
  tmp <- data.table(
    mean_beta_divq0 = mean(beta_div$beta_fd_q$q0),
    mean_beta_divq1 = mean(beta_div$beta_fd_q$q1),
    mean_beta_divq2 = mean(beta_div$beta_fd_q$q2),
    reserve_sor_beta_div = tmp_veg,
    reserve = reserve_name
  )
  
  dt_beta <- rbind(dt_beta, tmp)
  
  print(paste0(reserve_name, " done"))
  
}

dt_beta_reserve <- dt_beta

## use vegan approach 
library(vegan)


########################## beta diversity within sites ##################################

# Has to be calculated for each site seperately
# Preparation steps are as usual, but we'll loop through the sites

dt_beta_site <- data.table(
  site_mean_beta_divq0 = NA,
  site_mean_beta_divq1 = NA, 
  site_mean_beta_divq2 = NA, 
  site_sor_beta_div = NA,
  site_ID = NA
) %>% filter(!is.na(site_ID))

for(site in unique(dt_sp$site_ID)){
  
  site_data_raw <- dt_sp %>% filter(site_ID %in% c(site)) %>% 
    dplyr::select(plot_ID, species, cover_percent)
  
  site_data <- site_data_raw %>% 
    group_by(species, plot_ID) %>% 
    summarize(cover_percent = sum(cover_percent, na.rm = T)) %>% 
    unique() %>% 
    mutate(cover_percent = as.numeric(ifelse(is.na(cover_percent), 1, cover_percent)),
           cover_percent = as.numeric(ifelse(cover_percent == 0, 1, cover_percent))) %>% unique()
  
  
  ### build matrix for species weights (i.e., species as columns and rows contain their biomass/cover). Assemblages should be row names 
  site_weights <- site_data %>% 
    dplyr::filter(species %in% c(all_of(unique(trait_data$species)))) %>% 
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
  
  beta_div_site <- beta.fd.hill(asb_sp_w = site_weights, sp_dist = fdist)
  
  ## the q argument defines the importance of species weight compared to trait based distances (higher q, species weight is considered more important)
  mFD::dist.to.df(list_dist = list("FDq1" = beta_div$"beta_fd_q"$"q1"))
  
  
  ### calculate beta diversity using the vegan approach (Soerensen dissimilarity index)
  
  tmp_veg_site <- betadiver(site_weights, method = "w") %>% mean(na.rm = T)
  
  
  ## extract mean beta div
  tmp <- data.table(
    site_mean_beta_divq0 = mean(beta_div_site$beta_fd_q$q0),
    site_mean_beta_divq1 = mean(beta_div_site$beta_fd_q$q1),
    site_mean_beta_divq2 = mean(beta_div_site$beta_fd_q$q2),
    site_sor_beta_div = tmp_veg_site, 
    site_ID = site
  )
  
  dt_beta_site <- rbind(dt_beta_site, tmp)
  
  print(paste0(site, " done"))
  
}

dt_beta_site


######### calculate evenness and shannon diversity ########################
library("chemodiv")
library(vegan)

### site 
evenness_raw <- chemodiv::calcDiv(weight_mat_site, type = "PielouEven")

site_IDs <- weight_mat_site %>% 
  as.data.frame() %>%
  rownames_to_column(var = "site_ID") %>% 
  dplyr::select(site_ID)

evenness_site <- cbind(evenness_raw, site_IDs) %>% 
  rename(plant_evenness_site = PielouEven)

shannon_diversity_site <- vegan::diversity(weight_mat_site)

dt_shan_site <- data.table(
  site_ID = names(shannon_diversity_site),
  shannon_diversity_site = unname(shannon_diversity_site)
)

### plot 

evenness_raw_plot <- chemodiv::calcDiv(weight_mat, type = "PielouEven")

plot_IDs <- weight_mat %>% 
  as.data.frame() %>%
  rownames_to_column(var = "plot_ID") %>% 
  dplyr::select(plot_ID)

evenness_plot <- cbind(evenness_raw_plot, plot_IDs) %>% 
  rename(plant_evenness_plot = PielouEven)

shannon_diversity_plot <- diversity(weight_mat)
dt_shan_plot <- data.table(
  plot_ID = names(shannon_diversity_plot),
  shannon_diversity_plot = unname(shannon_diversity_plot)
)

### reserve 
evenness_raw_reserve <- chemodiv::calcDiv(weight_mat_reserve, type = "PielouEven")

reserves <- weight_mat_reserve %>% 
  as.data.frame() %>%
  rownames_to_column(var = "reserve") %>% 
  dplyr::select(reserve)

evenness_reserve <- cbind(evenness_raw_reserve, reserves) %>% 
  rename(plant_evenness_reserve = PielouEven)

shannon_diversity_reserve <- diversity(weight_mat_reserve)
dt_shan_reserve <- data.table(
  reserve = names(shannon_diversity_reserve),
  shannon_diversity_reserve = unname(shannon_diversity_reserve)
)



####### conbine to one functional diversity dataframe ######

dt_dominance
dt_fd_plot
dt_fd_site
dt_fd_reserve
dt_beta_reserve_fin <- dt_beta_reserve %>% dplyr::select(beta_diversity_reserve = reserve_sor_beta_div, 
                                                   functional_beta_diversity_reserve = mean_beta_divq1, 
                                                   reserve)
dt_beta_site_fin <- dt_beta_site %>% dplyr::select(beta_diversity_site = site_sor_beta_div, 
                                              functional_beta_diversity_site = site_mean_beta_divq1, 
                                              site_ID)
evenness_plot
evenness_site
evenness_reserve
dt_shan_plot
dt_shan_site
dt_shan_reserve

dt_fd_all <- dt_dominance %>% 
  left_join(dt_fd_plot) %>% 
  left_join(dt_fd_site) %>% 
  left_join(dt_fd_reserve) %>% 
  left_join(dt_beta_reserve_fin) %>% 
  left_join(dt_beta_site_fin) %>% 
  left_join(evenness_plot) %>% 
  left_join(evenness_site) %>% 
  left_join(evenness_reserve) %>% 
  left_join(dt_shan_plot) %>% 
  left_join(dt_shan_site) %>% 
  left_join(dt_shan_reserve) 

fwrite(dt_fd_all, "data/processed_data/data_fragments/plant_functional_diversity.csv")

