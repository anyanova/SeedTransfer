##Merge seed data with PlantingLocs
##Add Marshall and Nebraska data too - this includes survival data but only for those two datasets
##July 2025
setwd("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/Data/Merging")

#Load packages
package.list <- c("here", "tidyverse", "sf", "readxl", "stringdist","data.table", "geosphere","dplyr")
  ## Installing them if they aren't already on the computer
  new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  ## And loading them
  for(i in package.list){library(i, character.only = T)}

##Read in Survival/seed data (== FACTS + Nurseries), Marshall Merge later
FS <- fread(here("Data", "Merging", "FACTS_Nursery_5Aug.csv"))
##Exclude anything without seed collection locations
FS<-FS[!is.na(SeedLAT)]

##Read in plantingLocs
plantingLocs <- st_read(here("Data","Actv_SilvReforest", "S_USA.Actv_SilvReforest.shp"))

##Fix invalid geometries, identify and delete trouble rows (n=5)
plantingLocs<-st_make_valid(plantingLocs)
which(!st_is_valid(plantingLocs))
plantingLocs <- plantingLocs[-c(55712, 162448, 162449, 194140, 206582),]


##Column SUID = 19 digit ID created based on ,\info in FACTS data
#######Example: SUID 05 14 51 0010001000 005 - Region Number = 05 Admin Forest Number =14 District Number = 51 FACTS ID = 0010001000 Subunit = 005
##SUID_OG = the original SUID that was with the FACTS data (and not always following pattern above)
##just spatial data
factsSpatial <- plantingLocs %>%
      as.data.frame %>%
      group_by(SUID) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      select(SUID, SHAPE_AREA, geometry)
    
    #Join based on SUID
    match1 <- FS %>%
      left_join(factsSpatial, by = c("ID" = "SUID"))
    
    #Second join based on SUID_OG, handling potential duplicate column names
    match2 <- FS %>%
      left_join(factsSpatial, by = c("SUID_OG" = "SUID"), suffix = c(".SUID", ".SUID_OG"))
    
    # Combine results, prioritizing match1, then match2
    combined_matches <- bind_rows(
      match1 %>% mutate(match_type = "SUID"),
      match2 %>% mutate(match_type = "SUID_OG"))
    
    # Filter for rows where SHAPE_AREA is not NA
    final_matches<- combined_matches %>%
      filter(!is.na(SHAPE_AREA))
  
  ##What ID did records get matched on?
  table(final_matches$match_type) #mostly on the manually created SUID!
  table(final_matches$Region)
  
FM<-as.data.frame(final_matches)

##Formatting fixes
  date_cols <- c("PlantingDate","DateSeedCollected","DateSeedTest")
  FM <- FM %>% mutate(across(all_of(date_cols), ~as.Date(., format = "%Y-%m-%d")))

  FM <- st_as_sf(FM, crs = "EPSG:4326")
 # st_write(FM, "ShpFiletest.shp", append=TRUE)
  class(FM)
length(unique(FM$Seedlot))

############################
##Combine with Marshall#####
##Read in Marshall, which has already been merged with nursery data and is a tibble with geometry
  Marshall<-readRDS(here("Data","MarshallTibble.rds"))
  Marshall<-Marshall %>%
    mutate(
      SurveyDate_1 = as.Date(SurveyDate_1, format = "%Y-%m-%d"),
      SurveyDate_3 = as.Date(SurveyDate_3, format = "%Y-%m-%d"),
      DateSeedCollected = as.Date(DateSeedCollected, format = "%d-%b-%y"),
      DateSeedTest = as.Date(DateSeedTest, format = "%d-%b-%y"),
      Region = as.character(Region),
      SeedLAT = as.numeric(SeedLAT))
  
  Marshall<- st_transform(Marshall, crs = "EPSG:4326")
  FM$Region<-as.character(FM$Region)
  FM$nPlanted<-as.integer(FM$nPlanted)
  STD<- bind_rows(FM, Marshall)
  #st_write(STD, "ShpFiletest.shp", append=TRUE)

##Merge in Nebraska_SF (NebraskaMerge.R) - already has geometry + nursery data
  Nebraska<-readRDS(here("Data","NebraskaTibble.rds")) 
  setnames(Nebraska,"State","STATE_ABBR")
  Nebraska<- Nebraska %>%
    mutate( SurveyDate_1 = as.Date(SurveyDate_1, format = "%Y-%m-%d"),
      SurveyDate_3 = as.Date(SurveyDate_3, format = "%Y-%m-%d"),
      DateSeedCollected = as.Date(DateSeedCollected, format = "%d-%b-%y"),
      DateSeedTest = as.Date(DateSeedTest, format = "%d-%b-%y"),
      PlantingDate = as.Date(PlantingDate, format = "%Y-%m-%d"),
      Region = as.character(Region),
      SeedLAT = as.numeric(SeedLAT))%>%
      select(-SUID) #duplicate to ID
  
  Nebraska<- st_transform(Nebraska, crs = "EPSG:4326")
  STD$PlantingDate<-as.Date(STD$PlantingDate, format = "%Y-%m-%d")
  STD<- bind_rows(STD, Nebraska)
  
#Run survival calculations to bring in years 1 and 3. Account for 0 and NA in divisor. Cap survival at 100% (3 records had more trees surviving than planted)
  STD$Survival_1<-ifelse(is.na(STD$nSurviving_1), NA_real_,ifelse(STD$nSurviving_1 == 0, 0, pmin(100, (STD$nSurviving_1/STD$nPlanted) * 100)))
  STD$Survival_3<-ifelse(is.na(STD$nSurviving_3), NA_real_,ifelse(STD$nSurviving_1 == 0, 0, pmin(100, (STD$nSurviving_3/STD$nPlanted) * 100)))
  summary(STD$Survival_1)
  summary(STD$Survival_2)
  summary(STD$Survival_3)
  
##Consolidate Species into a single column: 1. Use Species, 2: Use Seedlot Spp (first 4 char), 3: SppCode, 4: Seedlot first two letters
  STD$Species<-ifelse(!is.na(STD$Species), STD$Species,
      ifelse(!is.na(STD$SeedlotSpp) & nchar(STD$SeedlotSpp) >= 4, substr(STD$SeedlotSpp, 1, 4),
      ifelse(!is.na(STD$SppCode), STD$SppCode,substr(STD$Seedlot, 1, 2))))

  ##Create a unique ID for each row
  STD$ID<-ifelse(!is.na(STD$ID), STD$ID, STD$SUID_OG) 
  setnames(STD,"STATE_ABBR","State")
  

#Remove excess columns
colnames(STD)
STD$NurseryCode<-ifelse(STD$NurseryCode=="Bessey","BES",STD$NurseryCode)

STD2<-STD[,c(1,3,161,5:9,137:140,143,157:158,149,150,167),]
STD2<-STD2[!duplicated(STD2),]

STD_SomeSurvival<-STD[,c(1,3,161,5:9,137:140,143,157:158,168,169,162:165,149,150,167), ]
STD_SomeSurvival<-STD_SomeSurvival[!duplicated(STD_SomeSurvival),]

STD2 %>%
  st_drop_geometry() %>%
  group_by(Region) %>%
  summarise(UniqueSeedlotCount = n_distinct(Seedlot, na.rm = TRUE),
  .groups = 'drop' ) %>%  
  arrange(Region)

##SAVE CSV AND WORKSPACE
setwd("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/Data/Merging")

write.csv(st_drop_geometry(STD),"SeedTransferData_Aug2025.csv", row.names = FALSE)
saveRDS(STD2, "SeedTransferData_Aug2025.RDS")
saveRDS(STD_SomeSurvival, "SeedTransferData_PlusSurvFromMarshallAndNeb_Aug2025.RDS")
