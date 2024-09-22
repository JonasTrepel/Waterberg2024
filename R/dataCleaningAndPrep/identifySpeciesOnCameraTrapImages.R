library(data.table)
library(tidyverse)
library("terra")

dtFiles <- fread("C:/Users/au713983/Documents/CameraTraps/test/processed/results_files.csv")
dtSummary <- fread("C:/Users/au713983/Documents/CameraTraps/test/processed/results_summary.csv")
dtDetRaw <- fread("C:/Users/au713983/Documents/CameraTraps/test/processed/results_detections.csv")

sitesAndCams <- fread("data/processedData/dataFragments/SitesAndCams.csv")

dtDet <- dtDetRaw %>% 
  mutate(cameraIDRaw = sapply(str_split(relative_path, pattern = "ECO"), function(x) x[2]), 
         cameraIDRaw = gsub(".JPG", "",cameraIDRaw), 
         cameraID = paste0("ECO",cameraIDRaw),
         cameraIDRaw = NULL,
         processed = "no", 
         speciesAbbr = "NA",
         DateTime = dmy_hms(DateTime), ## fix this mess tomorrow
         roundDate = round_date(DateTime, unit = "minute"),
         dateOnly = date(DateTime), 
         blastID = paste0(cameraID, roundDate)) %>% 
  left_join(sitesAndCams) %>% sample_n(20)

dtDet$dateOnly

for(i in 1:nrow(dtDet)){
  
  DaTi <- dtDet[i,]$DateTime
  camID <-  dtDet[i,]$cameraID
  bID <-  dtDet[i,]$blastID
  Da <- dtDet[i,]$dateOnly
  
  
  dtDet <- dtDet %>% 
    mutate(timeDiff = as.numeric(difftime(DaTi, DateTime, units = "secs"))) %>% 
    mutate(blastID = ifelse(timeDiff < 30 & cameraID == camID & dateOnly == Da,
                            bID, blastID), 
           timeDiff = NULL)
  
  print(paste0(i, "/", nrow(dtDet)))
  
}
table(dtDet$blastID)
## define species abbreviations and create legend

dtLeg <- data.table(
  speciesAbbr = c("a", "wa", "wi", "e", "k", "s",
                  "ro", "b", "h", "i", "u", "rh",
                  "p", "bi", "c", "ela", "bu", "g",
                  "n", "wb")) %>% 
  mutate(species = case_when(
    speciesAbbr == "a" ~ "small antelope", 
    speciesAbbr == "wa" ~ "warthog", 
    speciesAbbr == "wi" ~ "wildebeest", 
    speciesAbbr == "e" ~ "elephant", 
    speciesAbbr == "k" ~ "kudu", 
    speciesAbbr == "s" ~ "sable antelope", 
    speciesAbbr == "ro" ~ "roan antelope", 
    speciesAbbr == "b" ~ "baboon", 
    speciesAbbr == "h" ~ "human", 
    speciesAbbr == "i" ~ "impala", 
    speciesAbbr == "u" ~ "unknown", 
    speciesAbbr == "rh" ~ "rhino", 
    speciesAbbr == "p" ~ "predator", 
    speciesAbbr == "bi" ~ "bird", 
    speciesAbbr == "c" ~ "cattle", 
    speciesAbbr == "ela" ~ "eland", 
    speciesAbbr == "bu" ~ "buffalo", 
    speciesAbbr == "g" ~ "giraffe", 
    speciesAbbr == "n" ~ "nyala", 
    speciesAbbr == "wb" ~ "waterbuck", 
  ))



for(i in 1:nrow(dtDet)){
  
  if(dtDet[i,]$processed == "yes"){next}

path <- paste0(dtDet[i,]$absolute_path, "/processed/",dtDet[i,]$relative_path)

img <- rast(path)

plot(img)

spec <- readline("Type species on image here: ")

dtDet[i,]$speciesAbbr <- spec
dtDet[i,]$processed <- "yes"

print(paste0(i, "/", nrow(dtDet)))

}

dtDet

dtDetP <- dtDet %>% 
  left_join(dtLeg)

table(dtDetP$species)

## eventually write file... 

