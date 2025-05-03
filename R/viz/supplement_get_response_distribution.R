library(data.table)
library(tidyverse)

dt_raw <- fread("data/processed_data/clean_data/waterberg_2024_main_dataset.csv")



dt <- dt_raw %>%  
  pivot_longer(
    cols = c(
      plant_richness_plot, graminoid_richness_plot, forb_richness_plot, woody_richness_plot,
      functional_redundancy_plot, functional_richness_plot, functional_richness_plot, 
      
      
      plant_richness_site, graminoid_richness_site, forb_richness_site, woody_richness_site,  
      functional_redundancy_site,  functional_richness_site,
      
      plant_richness_reserve, graminoid_richness_reserve, forb_richness_reserve, woody_richness_reserve,  
      functional_redundancy_reserve, functional_richness_reserve
    ),  
    names_to = "response_name", values_to = "response_value"
  ) %>% 
  mutate(response_name = factor(response_name, levels = c(
    "plant_richness_plot", "plant_richness_site", "plant_richness_reserve",
    "graminoid_richness_plot", "graminoid_richness_site", "graminoid_richness_reserve",
    "forb_richness_plot", "forb_richness_site", "forb_richness_reserve",
    "woody_richness_plot", "woody_richness_site", "woody_richness_reserve",
    "functional_richness_plot", "functional_richness_site", "functional_richness_reserve",
    "functional_redundancy_plot", "functional_redundancy_site", "functional_redundancy_reserve"
  )))

dist_plot <- dt %>%
  ggplot(aes(x = response_value)) +
  geom_histogram(bins = 30, fill = "olivedrab", color = "black", alpha = 0.7) +
  facet_wrap(~ response_name, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(x = "Value", y = "Count", title = "Histograms of Response Variables") +
  theme(
    strip.text = element_text(size = 10, face = "italic"),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
dist_plot

ggsave(plot = dist_plot, "builds/plots/supplement/response_distributions.png", height = 12, width = 9)
