library(data.table)
library(tidyverse)
library(sf)

dt.raw <- fread("data/rawData/reserves_metadata_waterberg2024.csv")

dt.sf <- st_read("data/spatialData/reserveLocations/waterberg2024_reserves.shp")

sf_use_s2(FALSE)
dt.sf2 <- dt.sf %>% 
  as.data.table() %>%
  mutate(areaKm2 = as.numeric(st_area(dt.sf)/1000000), 
         reserve = ifelse(reserve == "Ants Farm", "Ant's Farm", reserve), 
         geometry = NULL) 


dt <- dt.raw %>% mutate( species = case_when(
  .default = species, 
  species == "Bos taurus africanus" ~ "Bos primigenius taurus", 
  species == "Taurotragus oryx" ~ "Tragelaphus oryx",
  species == "Capra hircus" ~ "Capra caucasica",
  species == "Alcelaphus lichtensteinii" ~ "Alcelaphus buselaphus",
  species == "Felis lybica" ~ "Felis silvestris",
  species == "Tragelaphus sylvaticus" ~ "Tragelaphus sylvaticus",
  species == "Galerella pulverulenta" ~ "Herpestes pulverulentus",
  species == "Lupulella mesomelas" ~ "Canis mesomelas", 
  species  == "Giraffa giraffa" ~ "Giraffa camelopardalis")) %>% 
  rename(ScientificName = species) %>% 
  pivot_longer(cols = c("Lapalala", "Jembisa", "Willowisp", "Syringa Sands",
                        "Summerplace", "Dabchick", "Ant's Farm",
                        "Kaingo", "Swebeswebe", "Marakele"), 
               names_to = "reserve", values_to = "speciesNumber") %>% 
  left_join(dt.sf2) %>% 
  mutate(densityKm2 = speciesNumber/areaKm2) %>% 
  dplyr::select(-species_common)

vCam <- unique(metaDT$ScientificName)
vRes <- unique(dt$ScientificName)
setdiff(vCam, vRes)
setdiff(vRes, vCam)


sitesAndCams <- fread("data/processedData/dataFragments/SitesAndCams.csv")

metaDTRaw2 <- fread("data/processedData/dataFragments/cameraTrapMetaDataR1.csv")

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


table(metaDT$Subject)

### 

## get number of blasts per Site 

eventsPerSiteSpecies <- metaDT %>% 
  group_by(site_ID, ScientificName, Subject) %>% 
  summarize(nEvents = n_distinct(blastID), 
            nEventsDay = (nEvents/deploymentTime), 
            meanBodyMassKg = mean(massKg, na.rm = T)) %>% 
  unique() %>% 
  left_join(sitesAndCams) %>% 
  dplyr::select(-plot_ID) %>% left_join(dt) %>%   
  filter(!is.na(densityKm2)) %>% 
  filter(!Subject %in% c("Steenbuck", "Reedbuck", "Horse"))

eventsPerReserveSpecies <- eventsPerSiteSpecies %>% 
  group_by(reserve, ScientificName, Subject) %>% 
  summarize(nEventsReserve = mean(nEvents, na.rm = T), 
            nEventsDayReserve =  mean(nEventsDay, na.rm = T), 
            meanBodyMassKgReserve = mean(meanBodyMassKg, na.rm = T)) %>% 
  unique() %>% left_join(dt) %>% 
  filter(!is.na(densityKm2)) %>% 
  filter(!Subject %in% c("Steenbuck", "Reedbuck"))


a <- ggplot() +
  geom_point(data = eventsPerReserveSpecies, aes(x = nEventsDayReserve, y = densityKm2)) +
  facet_wrap(~ Subject+ScientificName, scales = "free", labeller = label_value, ncol = 5) +
  theme_bw() +
  labs(x = "Reserve Level Mean Events/Day", y = "Density (Individuals/Km2)")
a

b <- ggplot() +
  geom_point(data = eventsPerSiteSpecies, aes(x = nEventsDay, y = densityKm2)) +
  facet_wrap(~ Subject+ScientificName, scales = "free", labeller = label_value, ncol = 5) +
  theme_bw() +
  labs(x = "Events/Day", y = "Density (Individuals/Km2)")
b

ggsave(plot = a, "builds/plots/supplement/SpeciesDensityAndActivityReserveLevel.png", dpi = 600, width = 10, height = 10)
ggsave(plot = b, "builds/plots/supplement/SpeciesDensityAndActivitySiteLevel.png", dpi = 600, width = 10, height = 10)

library(ggridges)
library(scico)
c <- ggplot() +
  geom_density_ridges(data = eventsPerSiteSpecies, aes(x = nEventsDay, y = Subject, fill = Subject)) +
  facet_wrap(~ reserve) +
  scale_fill_scico_d(palette = "batlowK") +
  theme_bw() +
  labs(x = "Events/Day", y = "") +
  theme(legend.position = "none")
c

ggsave(plot = c, "builds/plots/supplement/SpeciesLevelActivitySiteLevel.png", dpi = 600, width = 10, height = 10)
