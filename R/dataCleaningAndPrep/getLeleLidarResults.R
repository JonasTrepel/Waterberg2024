rm(list = ls())

library(data.table)
library(tidyverse)
library(sf)
library(lidR)
library(rlas)

## load lidar ----------------


### adapt filepath! 
files.raw <- as.data.table(list.files("O:/Nat_Ecoinformatics/C_Write/_User/JonasTrepel_au713983/DataAndResources/Waterberg_LiDAR/exported files/Lele e57/", pattern = ".laz", full.names = TRUE))


files <- files.raw %>% 
  rename(filepath = V1) %>% 
  mutate(filename = gsub("O:/Nat_Ecoinformatics/C_Write/_User/JonasTrepel_au713983/DataAndResources/Waterberg_LiDAR/exported files/Lele e57/", "", filepath), 
         filename = gsub(".laz", "", filename)) %>% 
  filter(!filename %in% c("batch_script_e572las64_lele.bat", "batch_script_e572las64_lele.txt"))

unique(files$filename)

## prep loop 

res <- data.frame()
#Scan time is 3 min, 40 sec. = 220 sec
#360,000 points/sec
# 
#possible.points <- 220*360000

#however, it says in the manual that a high res scan has about 65 mio points. 
# page 37, https://shop.leica-geosystems.com/sites/default/files/2022-01/853811_Leica_BLK360_UM_v4.0.0_en.pdf

possible.points <- 65000000

#when dividing the mean by the fraction of points, we reach higher values at lower fraction of point returns - 
### consistent with higher means represent more open systems 


#loooooop

for(i in 1:nrow(files)){
  
  file <- files[i,]$filename
  path <- files[i,]$filepath
  
  df <- read.las(path) %>%
    # discard irrelevant variables
    select(X:Z) %>% 
    ## remove duplicates
    # unique() %>%
    # calculate horizontal and 3d distance
    mutate(distance_2d = sqrt(X^2 + Y^2),
           distance_3d = sqrt(distance_2d^2 + Z^2), 
           angle_raw = (atan(X/Y)*180/pi),
           angle = case_when(
             X > 0 & Y > 0 ~ angle_raw,
             X > 0 & Y < 0 ~ 90 + abs(angle_raw),
             X < 0 & Y < 0 ~ 180 + abs(angle_raw), 
             X < 0 & Y > 0 ~ 270 + abs(angle_raw),
           )) %>% 
    # filter out observations more than 25 m away horizontally
    filter(distance_2d <= 20) %>% 
    dplyr::select(-angle_raw)
  
  (nrow(df)/possible.points)
  
  if(nrow(df)<1){next}
  
  tmp.full <- df %>% 
    summarise(mean_2d = mean(distance_2d),
              mean_3d = mean(distance_3d), 
              mean_Z = mean(Z), 
              
              adjusted_mean_2d = mean(distance_2d)/(nrow(.)/possible.points),
              adjusted_mean_3d = mean(distance_3d)/(nrow(.)/possible.points), 
              
              median_2d = median(distance_2d),
              median_3d = median(distance_3d), 
              median_Z = median(Z), 
              
              sd_2d = sd(distance_2d),
              sd_3d = sd(distance_3d),
              sd_Z = sd(Z), 
              
              point_fraction = (nrow(.)/possible.points)
    ) %>% 
    mutate(name = file)
  
  possible.points.understory <- possible.points*0.4 #.4 is the fraction of possible points below the scanner head
  
  tmp.understory <- df %>% 
    filter(Z < 0) %>% ## everything below 0
    summarise(
      mean_2d_herb = mean(distance_2d),
      mean_3d_herb = mean(distance_3d), 
      mean_Z_herb = mean(Z), 
      adjusted_mean_2d_herb = mean(distance_2d)/(nrow(.)/possible.points.understory), 
      adjusted_mean_3d_herb = mean(distance_3d)/(nrow(.)/possible.points.understory),  
      median_2d_herb = median(distance_2d),
      median_3d_herb = median(distance_3d), 
      median_Z_herb = median(Z), 
      sd_2d_herb = sd(distance_2d),
      sd_3d_herb = sd(distance_3d),
      sd_Z_herb = sd(Z),
      point_fraction_understory = (nrow(.)/possible.points.understory)) %>% 
    mutate(name = file)
  
  possible.points.woody <- possible.points*0.6 #0.6 is fraction of possible points above the scanner head
  
  tmp.woody <- df %>% 
    filter(Z > 0) %>% ## everything above 0
    summarise(
      mean_2d_woody = mean(distance_2d),
      mean_3d_woody = mean(distance_3d), 
      mean_Z_woody = mean(Z), 
      adjusted_mean_2d_woody = mean(distance_2d)/(nrow(.)/possible.points.woody),
      adjusted_mean_3d_woody = mean(distance_3d)/(nrow(.)/possible.points.woody), 
      median_2d_woody = median(distance_2d),
      median_3d_woody = median(distance_3d), 
      median_Z_woody = median(Z), 
      sd_2d_woody = sd(distance_2d),
      sd_3d_woody = sd(distance_3d),
      sd_Z_woody = sd(Z), 
      fraction_points_woody = (nrow(.)/possible.points.woody)) %>% 
    mutate(name = file)
  
  # Define the breaks for splitting X into 8 parts
  breaks <- seq(min(df$angle, na.rm = T), max(df$angle, na.rm = T), length.out = 6)
  
  # Create a function to generate the summaries for each part and Z split
  generate.summaries <- function(data, part.num, pp) {
    data %>%
      summarise(
        adjusted_mean_2d_partial = mean(distance_2d, na.rm = TRUE)/(nrow(.)/pp),
        adjusted_mean_3d_partial = mean(distance_3d, na.rm = TRUE)/(nrow(.)/pp),
        mean_2d_partial = mean(distance_2d, na.rm = TRUE),
        mean_3d_partial = mean(distance_3d, na.rm = TRUE),
        mean_Z_partial = mean(Z, na.rm = TRUE),
        part = part.num, 
        fraction_points_partial = (nrow(.)/pp),
      )
  }
  
  # Initialize an empty data frame to store the results
  parts <- data.frame()
  parts.woody <- data.frame()
  parts.herb <- data.frame()
  # Loop over each part
  for (j in 1:(length(breaks)-1)) {
    part.data <- df %>%
      filter(angle >= breaks[j] & angle < breaks[j+1])
    
    partSummary <- part.data %>%
      generate.summaries(part.num = j, pp = (possible.points/5))
    
    parts <- bind_rows(parts, partSummary)
    
  }
  
  tmp.partial <- parts %>% 
    summarize(sd_2d_partial = sd(mean_2d_partial , na.rm = T),
              sd_3d_partial = sd(mean_3d_partial , na.rm = T),
              sd_adjusted_3d_partial = sd(adjusted_mean_3d_partial, na.rm = T),
              sd_Z_partial = sd(mean_Z_partial, na.rm = T),
              mean_fraction_points_partial = mean(fraction_points_partial, na.rm = T), 
              sd_fraction_points_partial = sd(fraction_points_partial, na.rm = T), 
    ) %>% 
    mutate(name = file)
  
  tmp <- tmp.full %>% 
    left_join(tmp.woody) %>% 
    left_join(tmp.understory) %>% 
    left_join(tmp.partial)
  
  
  res <- rbind(tmp, res)
  
  print(paste0(file, " done (", i, "/", nrow(files), ")"  ))  
  
}


reserve_lidar <- res %>% unique()

fwrite(reserve_lidar, "data/processedData/dataFragments/LeleLidarResultsWaterberg2024Radius20m.csv")
