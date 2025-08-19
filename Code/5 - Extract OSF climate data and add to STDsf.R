##OSF Climate data 1985-2015
#https://osf.io/dpxgk

package.list <- c("here","terra","sf","tidyverse","terra", "sf", "readxl", "stringdist","data.table", "geosphere","dplyr","elevatr")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
## And loading them
for(i in package.list){library(i, character.only = T)}

#Read in Seed Transfer Dataset that already has elevation and seedPts
STDsf <- readRDS("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/Data/Merging/SeedTransferData_Elev_Aug2025.RDS")

## Read in climate data from OSF (TIF RASTER)
OSF<- rast(here("Data", "Spatial", "Climate", "TopoTerra_1985-2015.tif"))


#Create a secondary dataset with seed geoms
#convert to DF first **
STDdf<-as.data.frame(STDsf)
seedPts <- terra::vect(STDdf, geom = c("SeedLONG", "SeedLAT"), crs = "EPSG:4326")


# 1. Extract values for all layers at each planting unit, retaining original IDs
planting_extracted <- terra::extract(OSF, vect(STDsf), fun = mean, ID = TRUE)

# 2. Extract values for all layers from seed locations, retaining original IDs
seed_extracted <- terra::extract(OSF, seedPts, fun = mean, ID = TRUE)

# Get the names of the climate variables from the raster (aet, def, tmin, tmax)
climate_vars <- names(OSF)

# 3. Add planting and seed collection climate to larger dataset (STDsf)
# Loop through each climate variable to create new columns
for (var_name in climate_vars) {
  # Column for planting data
  planting_col_name <- paste0(var_name, "_Planting")
  STDsf[[planting_col_name]] <- NA_real_ # Initialize with NA
  
  # Column for seed data
  seed_col_name <- paste0(var_name, "_Seed")
  STDsf[[seed_col_name]] <- NA_real_ # Initialize with NA
  
  # Populate planting data
  # *The 'extract' function returns columns named after the raster layers,
  STDsf[[planting_col_name]][planting_extracted$ID] <- planting_extracted[[var_name]]
  
  # Populate seed data
  STDsf[[seed_col_name]][seed_extracted$ID] <- seed_extracted[[var_name]]
}

setwd("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/Data/Merging")
saveRDS(STDsf, "SeedTransferData_OSFclim_Aug2025.rds")



#####################
#####################
###Explore and plot##
#####################
STDsf$elev_diff<-STDsf$Elevation_PlantingLoc - STDsf$SeedElevation
hist(STDsf$elev_diff)
summary(STDsf$elev_diff)
#view(STDsf %>%   filter(elev_diff > 1500))


STDsfev<-STDsf[STDsf$elev_diff<0,]
table(STDsfev$Region)

##Seed elevation vs planted quick look
ggplot(STDsf, aes(SeedElevation,Elevation_PlantingLoc)) + 
  geom_point() + geom_smooth(method="lm")

STDsf$SeedElevation<-as.numeric(STDsf$SeedElevation)
STDsf$PlantedElevation<-as.numeric(STDsf$Elevation_PlantingLoc)
STDsf$nPlanted<-as.numeric(STDsf$nPlanted)
STDsf$Survival_1<-as.numeric(STDsf$Survival_1)

#Simplified dataset for plotting (no geometry)
STDsf %>%  filter(nPlanted >= 1) %>%
  select(PlantedElevation, SeedElevation, Survival_1, nPlanted, elev_diff, Species) -> plot_data

