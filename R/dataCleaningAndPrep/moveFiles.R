# find all files in the camera trap folder and rewrite them into the permanent directory which is not on the ODrive 

library(data.table)
library(tidyverse)

subfolders1 <- paste0("ECO_", 10:50)

subfolders2 <- c("ECO_01", "ECO_02",  "ECO_03",  "ECO_04",  "ECO_05",  "ECO_06",  "ECO_07",  "ECO_08",  "ECO_09") 

subfolders <- c(subfolders2, subfolders1)

#subfolders <- c("ECO_03",  "ECO_06",  "ECO_07",  "ECO_08",  "ECO_09") 


for(folder in unique(subfolders)){
  
  genOrigPath <- "O:/Nat_Ecoinformatics/C_Write/_User/JonasTrepel_au713983/DataAndResources/CameraTrapsWaterberg2024/R1Raw/"
  
  specPath <- paste0(genOrigPath, folder, "/")
  
  files <- list.files(path = specPath,
                      pattern = ".JPG", 
                      full.names = T)
  
  filenames <- list.files(path = specPath,
                      pattern = ".JPG", 
                      full.names = F)
  
  dt.files <- data.frame(files = files, 
                         filenames = filenames) 
  
  for(filename in unique(dt.files$filenames)){
    
    genTargPath  <- "C:/Users/au713983/Documents/WaterbergCameraTrapsR12024All/RawImages/MissedInRound1/"
    
    cleanName <- gsub(".JPG", "", filename)
    specTargPath <- paste0(genTargPath, cleanName, "_", folder, ".JPG")
    specOrigPath <- dt.files[dt.files$filenames == filename, ]$files
      
    file.copy(from = specOrigPath, to = specTargPath, overwrite = TRUE)
    
    
  }
  
  print(paste0(folder, " done"))
}  
