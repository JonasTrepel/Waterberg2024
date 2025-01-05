library("exifr")
library(data.table)
library(tidyverse)
library(lubridate)

#load site metadata 
sitesAndCams <- fread("data/processedData/dataFragments/SitesAndCams.csv")


filesRaw <- list.files("/Volumes/TOSHIBA EXT/WaterbergCameraTraps2024/R1Processed", pattern = "JPG", full.names = TRUE)
files <- filesRaw


metaDTRaw <- exifr::read_exif(files, quiet = TRUE, tags = c("Directory", "FileName", "DateTimeOriginal", "Subject", 
                                                         "TagsList", "Keywords"))

# this splits the hierarchical columns (in this case Subject and Keywords) into several rows per observation if you have tagged i.e. multiple species o
metaDTRaw2 <- metaDTRaw %>% 
  unnest(cols = c(Subject, Keywords, TagsList)) %>% 
  mutate(cameraIDRaw = sapply(str_split(FileName, pattern = "ECO"), function(x) x[2]), 
         cameraIDRaw = gsub(".JPG", "",cameraIDRaw), 
         cameraID = paste0("ECO",cameraIDRaw),
         DateTimeRaw = ymd_hms(DateTimeOriginal),
         DateTime = update(DateTimeRaw, year = 2024),
         roundDate = round_date(DateTime, unit = "minute"),
         roundDateID = dense_rank(roundDate) ,
         dateOnly = date(DateTime), 
         blastID = paste0(cameraID,"_", roundDateID), 
         uniqueID = paste0("obs", 1:nrow(.))) %>% 
  left_join(sitesAndCams)  %>% 
  dplyr::select(-c(DateTimeRaw, cameraIDRaw, roundDateID)) 

fwrite(metaDTRaw2, "data/processedData/dataFragments/cameraTrapMetaDataR1.csv")
metaDTRaw2 <- fread("data/processedData/dataFragments/cameraTrapMetaDataR1.csv")


#fwrite(nicePics, "data/processedData/dataFragments/cameraTrapR1NicePics.csv")


unique(metaDTRaw2$cameraID)
table(metaDTRaw2$Subject)


siteTimes <- metaDTRaw2 %>% 
  group_by(site_ID) %>%
  summarize(
  firstTrigger = min(DateTime), 
  lastTrigger = max(DateTime), 
  deploymentTime = abs(as.numeric(difftime(firstTrigger, lastTrigger, units = "days"))))

herbiTraits <- fread("/Users/jonas/Library/CloudStorage/Dropbox/resources/traits/HerbiTraits/HerbiTraits_1.2/HerbiTraits_1.2.csv")

metaDT <- metaDTRaw2 %>% 
  left_join(siteTimes) %>% 
  filter(!Subject %in% c("Human", "Empty", "ParticularlyBeautiful", "Predator", "VervetMonkey",
                         "Baboon", "MultipleIndividuals", "Hare", "Porcupine", "Aardvark", "Bird")) %>% 
  filter(!FileName == "00000000_ECO_06.JPG") %>% 
  mutate(Binomial = case_when(
    Subject == "Giraffe" ~ "Giraffa camelopardalis", 
    Subject == "Zebra" ~ "Equus quagga", 
    Subject == "Wildebeest" ~ "Connochaetes taurinus", 
    Subject == "Impala" ~ "Aepyceros melampus", 
    Subject == "Bushpig" ~ "Potamochoerus larvatus", 
    Subject == "Eland" ~ "Tragelaphus oryx", 
    Subject == "Duiker" ~ "Sylvicapra grimmia", 
    Subject == "Kudu" ~ "Tragelaphus strepsiceros", 
    Subject == "Hartebeest" ~ "Alcelaphus buselaphus", 
    Subject == "Warthog" ~ "Phacochoerus africanus", 
    Subject == "Rhino" ~ "Ceratotherium simum", 
    Subject == "RoanAntelope" ~ "Hippotragus equinus", 
    Subject == "Bushbuck" ~ "Tragelaphus sylvaticus", 
    Subject == "Waterbuck" ~ "Kobus ellipsiprymnus", 
    Subject == "SableAntelope" ~ "Hippotragus niger", 
    Subject == "Gemsbok" ~ "Oryx gazella", 
    Subject == "Buffalo" ~ "Syncerus caffer", 
    Subject == "Elephant" ~ "Loxodonta africana", 
    Subject == "Horse" ~ "Equus ferus caballus", 
    Subject == "Klipspringer" ~ "Oreotragus oreotragus", 
    Subject == "Steenbuck" ~ "Raphicerus campestris", 
    Subject == "Cattle" ~ "Bos primigenius taurus", 
    Subject == "Reedbuck" ~ "Redunca fulvorufula", 
    Subject == "Nyala" ~ "Tragelaphus angasii", 
  )) %>% 
  left_join(herbiTraits %>% dplyr::select(Binomial, Mass.g, Guild.w.Omnivory, Guild.only.Herbivory)) %>%
  mutate(massKg = Mass.g/1000) %>% 
  rename(ScientificName = Binomial, 
         GuildWithOmnivory = Guild.w.Omnivory, 
         GuildOnlyHerbivory = Guild.only.Herbivory)

unique(metaDT$Subject)
unique(metaDT[is.na(metaDT$ScientificName), ]$Subject)

mean(metaDT$deploymentTime) 
sd(metaDT$deploymentTime) 

unique(metaDT$Subject)
table(metaDT$blastID)

#metaDT <- metaDT[metaDT$uniqueID %in% uIDS, ]

for(i in 1:nrow(metaDT)){
  
  DaTi <- metaDT[i,]$DateTime
  camID <-  metaDT[i,]$cameraID
  bID <-  metaDT[i,]$blastID
  Da <- metaDT[i,]$dateOnly
  
  
  metaDT <- metaDT %>% 
    mutate(timeDiff = as.numeric(difftime(DaTi, DateTime, units = "secs"))) %>% 
    mutate(blastID = ifelse(abs(timeDiff) < 10 & cameraID == camID & dateOnly == Da,
                            bID, blastID))
  
  print(paste0(i, "/", nrow(metaDT)))
  
}


unique(metaDT$Subject)

### 

## get number of blasts per Site 

eventsPerSite <- metaDT %>% 
  group_by(site_ID) %>% 
  summarize(nEvents = n_distinct(blastID), 
            nEventsDay = (nEvents/deploymentTime), 
            meanBodyMassKg = mean(massKg, na.rm = T)) %>% 
  unique() %>% 
  left_join(sitesAndCams) %>% 
  dplyr::select(-plot_ID)


sum(eventsPerSite$nEvents)

eventsPerReserve <- eventsPerSite %>% 
  group_by(reserve) %>% 
  summarize(nEventsReserve = mean(nEvents, na.rm = T), 
            nEventsDayReserve =  mean(nEventsDay, na.rm = T), 
            meanBodyMassKgReserve = mean(meanBodyMassKg, na.rm = T)) %>% 
  unique() 

cameraTrapResults <- eventsPerSite %>% left_join(eventsPerReserve)

hist(eventsPerSite$nEventsDay)

ggplot() +
  geom_histogram(data = cameraTrapResults, aes(x = nEventsDay, y = ..count..)) + 
  facet_wrap(~reserve)
  
ggplot() +
  geom_histogram(data = cameraTrapResults, aes(x = meanBodyMassKg, y = ..count..)) + 
  facet_wrap(~reserve)

table(metaDT$blastID)
n_distinct(metaDT$blastID)

fwrite(cameraTrapResults, "data/processedData/dataFragments/cameraTrapObs.csv")

###### explore if we can separate by feeding guilds 

browser_events_site <- metaDT %>% 
  filter(GuildWithOmnivory == "Browser") %>% 
  group_by(site_ID) %>% 
  summarize(nEvents = n_distinct(blastID), 
            n_events_day_browser = (nEvents/deploymentTime)) %>% 
  unique() %>% 
  left_join(sitesAndCams) %>% 
  dplyr::select(-plot_ID, - nEvents)


grazer_events_site <- metaDT %>% 
  filter(GuildWithOmnivory == "Grazer") %>% 
  group_by(site_ID) %>% 
  summarize(nEvents = n_distinct(blastID), 
            n_events_day_grazer = (nEvents/deploymentTime)) %>% 
  unique() %>% 
  left_join(sitesAndCams) %>% 
  dplyr::select(-plot_ID, - nEvents)

mixed_events_site <- metaDT %>% 
  filter(GuildWithOmnivory == "Mixed Feeder") %>% 
  group_by(site_ID) %>% 
  summarize(nEvents = n_distinct(blastID), 
            n_events_day_mixed = (nEvents/deploymentTime)) %>% 
  unique() %>% 
  left_join(sitesAndCams) %>% 
  dplyr::select(-plot_ID, - nEvents)


guild_events <- eventsPerSite %>% 
  left_join(browser_events_site) %>% 
  left_join(mixed_events_site) %>% 
  left_join(grazer_events_site) %>% 
  rename(n_events_day_total = nEventsDay) %>% 
  ungroup() %>% 
  dplyr::select(n_events_day_total, n_events_day_browser, n_events_day_grazer, n_events_day_mixed)

library(GGally)

ggpairs(guild_events) # dang, correlations of guild specific and total visitation rates are annoyingly high. 
