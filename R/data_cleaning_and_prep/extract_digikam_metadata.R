library("exifr")
library(data.table)
library(tidyverse)
library(lubridate)

#load site metadata 
sites_and_cams <- fread("data/processed_data/data_fragments/sites_and_cams.csv") %>% 
  dplyr::select(-plot_ID) %>% 
  unique()


files_raw <- list.files("/Volumes/TOSHIBA EXT/WaterbergCameraTraps2024/R1Processed", pattern = "JPG", full.names = TRUE)
files <- files_raw


dt_meta_raw <- exifr::read_exif(files, quiet = TRUE, tags = c("Directory", "FileName", "DateTimeOriginal", "Subject", 
                                                         "TagsList", "Keywords"))

# this splits the hierarchical columns (in this case Subject and Keywords) into several rows per observation if you have tagged i.e. multiple species o
dt_meta_raw2 <- dt_meta_raw %>% 
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
  left_join(sites_and_cams)  %>% 
  dplyr::select(-c(DateTimeRaw, cameraIDRaw, roundDateID)) 

fwrite(dt_meta_raw2, "data/processed_data/data_fragments/camera_trap_meta_data_r1.csv")
dt_meta_raw2 <- fread("data/processed_data/data_fragments/camera_trap_meta_data_r1.csv")


#fwrite(nicePics, "data/processed_data/data_fragments/cameraTrapR1NicePics.csv")


unique(dt_meta_raw2$cameraID)
table(dt_meta_raw2$Subject)


site_times <- dt_meta_raw2 %>% 
  group_by(site_ID) %>%
  summarize(
  firstTrigger = min(DateTime), 
  lastTrigger = max(DateTime), 
  deploymentTime = abs(as.numeric(difftime(firstTrigger, lastTrigger, units = "days"))))

herbi_traits <- fread("../../../../resources/traits/HerbiTraits/HerbiTraits_1.2/HerbiTraits_1.2.csv")

dt_meta <- dt_meta_raw2 %>% 
  left_join(site_times) %>% 
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
  left_join(herbi_traits %>% dplyr::select(Binomial, Mass.g, Guild.w.Omnivory, Guild.only.Herbivory)) %>%
  mutate(massKg = Mass.g/1000) %>% 
  rename(ScientificName = Binomial, 
         GuildWithOmnivory = Guild.w.Omnivory, 
         GuildOnlyHerbivory = Guild.only.Herbivory)

unique(dt_meta$Subject)
unique(dt_meta[is.na(dt_meta$ScientificName), ]$Subject)

mean(dt_meta$deploymentTime) 
sd(dt_meta$deploymentTime) 

unique(dt_meta$Subject)
table(dt_meta$blastID)

#dt_meta <- dt_meta[dt_meta$uniqueID %in% uIDS, ]

for(i in 1:nrow(dt_meta)){
  
  DaTi <- dt_meta[i,]$DateTime
  camID <-  dt_meta[i,]$cameraID
  bID <-  dt_meta[i,]$blastID
  Da <- dt_meta[i,]$dateOnly
  
  
  dt_meta <- dt_meta %>% 
    mutate(timeDiff = as.numeric(difftime(DaTi, DateTime, units = "secs"))) %>% 
    mutate(blastID = ifelse(abs(timeDiff) < 10 & cameraID == camID & dateOnly == Da,
                            bID, blastID))
  
  print(paste0(i, "/", nrow(dt_meta)))
  
}


unique(dt_meta$Subject)

### 

## get number of blasts per Site 

events_per_site <- dt_meta %>% 
  group_by(site_ID) %>% 
  summarize(n_trigger_events = n_distinct(blastID), 
            n_trigger_events_day = (n_trigger_events/deploymentTime), 
            camera_mean_body_mass_kg = mean(massKg, na.rm = T)) %>% 
  unique() %>% 
  left_join(sites_and_cams) 


sum(events_per_site$n_trigger_events)
hist(events_per_site$n_trigger_events_day)


events_per_reserve <- events_per_site %>% 
  group_by(reserve) %>% 
  summarize(n_trigger_events_reserve = mean(n_trigger_events, na.rm = T), 
            n_trigger_events_day_reserve =  mean(n_trigger_events_day, na.rm = T), 
            camera_mean_body_mass_kg_reserve = mean(camera_mean_body_mass_kg, na.rm = T)) %>% 
  unique() 

camera_trap_results <- events_per_site %>% left_join(events_per_reserve)

hist(events_per_site$n_trigger_events_day)

ggplot() +
  geom_histogram(data = camera_trap_results, aes(x = n_trigger_events_day, y = ..count..)) + 
  facet_wrap(~reserve)
  
ggplot() +
  geom_histogram(data = camera_trap_results, aes(x = camera_mean_body_mass_kg, y = ..count..)) + 
  facet_wrap(~reserve)

table(dt_meta$blastID)
n_distinct(dt_meta$blastID)

fwrite(camera_trap_results, "data/processed_data/data_fragments/camera_trap_obs.csv")

###### explore if we can separate by feeding guilds 

browser_events_site <- dt_meta %>% 
  filter(GuildWithOmnivory == "Browser") %>% 
  group_by(site_ID) %>% 
  summarize(n_trigger_events = n_distinct(blastID), 
            n_events_day_browser = (n_trigger_events/deploymentTime)) %>% 
  unique() %>% 
  left_join(sites_and_cams) %>% 
  dplyr::select(-plot_ID, - n_trigger_events)


grazer_events_site <- dt_meta %>% 
  filter(GuildWithOmnivory == "Grazer") %>% 
  group_by(site_ID) %>% 
  summarize(n_trigger_events = n_distinct(blastID), 
            n_events_day_grazer = (n_trigger_events/deploymentTime)) %>% 
  unique() %>% 
  left_join(sites_and_cams) %>% 
  dplyr::select(-plot_ID, - n_trigger_events)

mixed_events_site <- dt_meta %>% 
  filter(GuildWithOmnivory == "Mixed Feeder") %>% 
  group_by(site_ID) %>% 
  summarize(n_trigger_events = n_distinct(blastID), 
            n_events_day_mixed = (n_trigger_events/deploymentTime)) %>% 
  unique() %>% 
  left_join(sites_and_cams) %>% 
  dplyr::select(-plot_ID, - n_trigger_events)


guild_events <- events_per_site %>% 
  left_join(browser_events_site) %>% 
  left_join(mixed_events_site) %>% 
  left_join(grazer_events_site) %>% 
  rename(n_events_day_total = n_trigger_events_day) %>% 
  ungroup() %>% 
  dplyr::select(n_events_day_total, n_events_day_browser, n_events_day_grazer, n_events_day_mixed)

library(GGally)

ggpairs(guild_events) # dang, correlations of guild specific and total visitation rates are annoyingly high. 
