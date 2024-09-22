# find all files in the camera trap folder and rewrite them into the permanent directory which is not on the ODrive 

library(data.table)
library(tidyverse)

resDet <- fread("C:/Users/au713983/Documents/CameraTraps/ECO_37/ECO_37_Processed/results_detections.csv")
resFiles <- fread("O:/Nat_Ecoinformatics/C_Write/_User/JonasTrepel_au713983/DataAndResources/CameraTrapsWaterberg2024/R1/ECO_01/results_files.csv")


subfolders <- paste0("ECO_", 1:50)

for(folder in unique(subfolders)){
  
  genOrigPath <- "O:/Nat_Ecoinformatics/C_Write/_User/JonasTrepel_au713983/DataAndResources/CameraTrapsWaterberg2024/R1/"
  
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
    
    genTargPath  <- "C:/Users/au713983/Documents/WaterbergCameraTrapsR12024All/RawImages/"
    
    cleanName <- gsub(".JPG", "", filename)
    specTargPath <- paste0(genTargPath, cleanName, "_", folder, ".JPG")
    specOrigPath <- dt.files[dt.files$filenames == filename, ]$files
      
    file.copy(from = specOrigPath, to = specTargPath, overwrite = TRUE)
    
    
  }
  
  print(folder, " done")
}  
