get_mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


library(tidyverse)
library(data.table)

dt_fam <- fread("data/processed_data/data_fragments/species_families.csv")


dt_sp <- fread("data/processed_data/data_fragments/plot_species_waterberg2024.csv") %>% 
  dplyr::select(species, growth_form, height_cm, hairs, leaf_size, bulk_density) %>%
  mutate(hairs = case_when(
    .default = hairs, 
    hairs %in% c("Absent", "absent") ~ "Absent", 
    hairs %in% c("stems", "stems and branches") ~ "Stems", 
    hairs %in% c("leaves") ~ "Leaves", 
    hairs %in% c("both") ~ "Leaves and Stems")) %>% 
  group_by(species) %>% 
  mutate(
    growth_form = get_mode(growth_form),
    height_cm = max(height_cm, na.rm = T), 
    hairs = get_mode(hairs), 
    leaf_size = get_mode(leaf_size), 
    bulk_density = get_mode(bulk_density)) %>%
  ungroup() %>% 
  unique() %>% 
  left_join(dt_fam) %>%
  filter(!is.na(family)) %>% 
  group_by(family) %>% 
  mutate(n_family = n()) %>% 
  ungroup()

p_gf <- dt_sp %>% 
  filter(n_family > 9) %>% 
  ggplot() +
  geom_bar(aes(x = growth_form), fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~family) +
  labs(x = "Growth Form", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_gf

ggsave(plot = p_gf, "builds/plots/supplement/growth_form_distribution.png", dpi = 600)

p_hairs <- dt_sp %>% 
  filter(n_family > 9) %>% 
  ggplot() +
  geom_bar(aes(x = hairs), fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~family) +
  labs(x = "Hairiness", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_hairs

ggsave(plot = p_hairs, "builds/plots/supplement/hairiness_distribution.png", dpi = 600)

p_ls <- dt_sp %>% 
  filter(n_family > 9) %>% 
  ggplot() +
  geom_bar(aes(x = leaf_size), fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~family) +
  labs(x = "Leaf Size Class", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_ls

ggsave(plot = p_ls, "builds/plots/supplement/leaf_size_distribution.png", dpi = 600)

p_bd <- dt_sp %>% 
  filter(n_family > 9) %>% 
  mutate(bulk_density = str_to_sentence(bulk_density)) %>%  
  ggplot() +
  geom_bar(aes(x = bulk_density), fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~family) +
  labs(x = "Biomass Density", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_bd

ggsave(plot = p_bd, "builds/plots/supplement/bulk density_distribution.png", dpi = 600)

p_height <- dt_sp %>% 
  filter(n_family > 9) %>% 
  mutate(height_cm = as.numeric(height_cm)) %>% 
  ggplot(aes(x = height_cm)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~family) +
  scale_x_log10() +
  labs(x = "Plant Height (cm)", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_height

ggsave(plot = p_height, "builds/plots/supplement/plant_height_distribution.png", dpi = 600)

p_n_fam <- dt_sp %>% 
  ggplot(aes(x = n_family)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  scale_x_log10() +
  labs(x = "Number of Species per Family", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_n_fam

p_n_fam <- dt_sp %>% 
  dplyr::select(family, n_family) %>% 
  unique() %>% 
  ggplot(aes(x = n_family, y = reorder(family, n_family))) +
 # geom_vline(xintercept = 10, linetype = "dashed") +
  geom_col(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(x = "Number of Species per Family", y = "Family") +
  theme_bw() 
p_n_fam
ggsave(plot = p_n_fam, "builds/plots/supplement/species_in_family_distribution.png", dpi = 600, height = 10, width = 8)

n_distinct(dt_sp$family)
n_distinct(dt_sp$species)
n_distinct(dt_sp[dt_sp$n_family > 9, ]$species)
n_distinct(dt_sp[dt_sp$n_family > 9, ]$family)

dt_traits <- fread("data/processed_data/data_fragments/plot_species_waterberg2024.csv") %>% 
  dplyr::select(species, growth_form, height_cm, hairs, leaf_size, bulk_density) %>%
  mutate(hairs = case_when(
    .default = hairs, 
    hairs %in% c("Absent", "absent") ~ "Absent", 
    hairs %in% c("stems", "stems and branches") ~ "Stems", 
    hairs %in% c("leaves") ~ "Leaves", 
    hairs %in% c("both") ~ "Leaves and Stems"), 
    height_cm = as.numeric(height_cm)) %>% 
  group_by(species) %>% 
  mutate(
    n_growth_form_species = n_distinct(growth_form),
    cv_height_cm_species = (sd(height_cm, na.rm = T)/mean(height_cm, na.rm = T)),  
    n_hairs_species = n_distinct(hairs), 
    n_leaf_size_species = n_distinct(leaf_size), 
    bulk_density_species = n_distinct(bulk_density)) %>%
  ungroup() %>% 
  dplyr::select(species, n_growth_form_species, cv_height_cm_species, n_hairs_species, n_leaf_size_species, bulk_density_species) %>% 
  unique() %>% 
  pivot_longer(cols = c("n_growth_form_species", "cv_height_cm_species", 
                        "n_hairs_species", "n_leaf_size_species", "bulk_density_species"), 
               names_to = "variable_name", 
               values_to = "variable_value")


dt_traits %>% 
  ggplot() +
  geom_histogram(aes(x = variable_value)) +
  facet_wrap(~ variable_name, scales = "free")


  