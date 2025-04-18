# find all files in the camera trap folder and rewrite them into the permanent directory which is not on the ODrive 

library(data.table)
library(tidyverse)

nicePics <- fread("data/processedData/dataFragments/cameraTrapR1NicePics.csv")


dt.files <- data.table(filenames = unique(nicePics$FileName))
  
  for(filename in unique(dt.files$filenames)){
    
    genTargetPath <- "O:/Nat_Ecoinformatics/C_Write/_User/JonasTrepel_au713983/DataAndResources/CameraTrapsWaterberg2024/NicePictures/R1/"
    
    
    if(grepl("_01", filename) | grepl("_02", filename) |
      grepl("_03", filename) | grepl("_04", filename) | 
      grepl("_05", filename) | grepl("_06", filename) | 
      grepl("_07", filename) | grepl("_08", filename) | 
      grepl("_09", filename)){
        origPathGen  <- "C:/Users/au713983/Documents/WaterbergCameraTrapsR12024All/RawImages/MissedInRound1/"
      }else{
        origPathGen  <- "C:/Users/au713983/Documents/WaterbergCameraTrapsR12024All/RawImages/"
      }
  
    specTargPath <- paste0(genTargetPath, filename)
    specOrigPath <- paste0(origPathGen, filename)
    
    file.copy(from = specOrigPath, to = specTargPath, overwrite = TRUE)
    
    print(paste0(filename, " done"))
    
    
  }
  