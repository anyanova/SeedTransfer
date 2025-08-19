##Add elevation and make exploratory plots Seed Transfer Dataset
#Load packages
package.list <- c("here", "tidyverse", "sf", "readxl", "stringdist","data.table", "geosphere","dplyr","elevatr")
## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
## And loading them
for(i in package.list){library(i, character.only = T)}


##Manually add elevations using elevatr package and point data
#Make STD a SF format
STD<- readRDS("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/Data/Merging/SeedTransferData_Aug2025.rds")
STDsf <- st_as_sf(STD)

#Make sure all long are negative
STDsf <- STDsf %>% mutate(
  SeedLONG = ifelse(SeedLONG > 0, -SeedLONG, SeedLONG))

STDsf <- STDsf %>%
  # Check for and fix invalid geometries
  mutate(geometry = st_make_valid(geometry)) %>%
  # Calculate centroids for all geometries
  mutate(centroid_PlantingLoc = st_centroid(geometry))

STDsf<-st_transform(STDsf, 4326)

class(STDsf)
dim(STDsf)


# Calculate planting loc centroids, isolate point data == geometry
for (i in 1:nrow(STDsf)) {
  geom_type <- st_geometry_type(STDsf[i, ])
  if (geom_type == "POINT") {
    STDsf$centroid_PlantingLoc[i] <- STDsf$geometry[i]
  } else {
    STDsf$centroid_PlantingLoc[i] <- st_centroid(STDsf$geometry[i])}}

STDsf

##Bring in elevation for planting locations
  # Extract the list of coordinates
    coords_PlantingLocs <- STDsf$centroid_PlantingLoc
  # Convert the list of coordinates into a data frame with 'longitude' and 'latitude' columns, remove NA for elevatR
    coords_df <- data.frame(matrix(unlist(coords_PlantingLocs), ncol = 2, byrow = TRUE))
    coords_df<- coords_df[complete.cases(coords_df), ]
    colnames(coords_df) <- c("x", "y") # Explicitly name them
    
    coord_crs <- sf::st_crs(4326)
    elevation_data <- elevatr::get_elev_point(coords_df, prj = coord_crs, units="meters")
    
  # Merge back into STDsf
      STDsf$Elevation_PlantingLoc <- NA
      original_complete_cases <- complete.cases(data.frame(matrix(unlist(STDsf$centroid_PlantingLoc), ncol = 2, byrow = TRUE)))
  # Assign elevation data to the corresponding rows in STDsf
    STDsf$Elevation_PlantingLoc[original_complete_cases] <- elevation_data$elevation
    
    summary(STDsf$Elevation_PlantingLoc)
    hist(STDsf$Elevation_PlantingLoc)
    
STDsf$PlantedElevation<-STDsf$Elevation_PlantingLoc
  
##Get elevation for Seed collection
  #Preserve order of all 3768 records
    STDsf$original_order <- as.numeric(rownames(STDsf))
  
  #Make lat/long numeric and remove any incomplete coordinate sets
    STDsf<- STDsf %>%
      mutate(SeedLAT = as.numeric(as.character(SeedLAT)),
      SeedLONG = as.numeric(as.character(SeedLONG))) %>%
      mutate(SeedLONG = ifelse(SeedLONG > 0, -SeedLONG, SeedLONG))

    sum(is.na(STDsf$SeedLAT))
    sum(is.na(STDsf$SeedLONG))

  #Create a clean data frame of coordinates
    coords_df_seed<- STDsf %>%
      st_drop_geometry() %>% 
      select(SeedLONG, SeedLAT,original_order) %>%
      filter(!is.na(SeedLAT) & !is.na(SeedLONG)) 
    
    coords_seed_sf <- st_as_sf(coords_df_seed, coords = c("SeedLONG", "SeedLAT"), crs = 4326, agr = "constant") %>% st_cast("POINT") 
  
  # Get elevation data using elevatr with the clean coordinate data frame
    elevation_seed <- elevatr::get_elev_point(coords_seed_sf, prj = coord_crs, units = "meters")
  
#Merge elevation data back into STDsf based on original order
    # Ensure elevation_data is a regular data frame
    elevation_seed<- elevation_seed %>%
      select(original_order, SeedElevation=elevation) %>%
      st_drop_geometry()
    
    # Perform a left_join using dplyr
    STDsf <- left_join(STDsf, elevation_seed, by = "original_order")
    
# Remove the temporary order column
STDsf <- STDsf %>% select(-original_order)
STDsf    

hist(STDsf$Elevation_PlantingLoc)
hist(STDsf$SeedElevation)

#Fix erroneous longitude 
#(Seedlot JP17881065 == Reno)
#
STDsf %>%  arrange((SeedLONG))
STDsf %>%  arrange(desc(SeedLAT))
STDsf %>%  filter(!is.na(Seedlot) & str_detect(Seedlot, "WL765916"))
STDsf %>%  filter(!is.na(Seedlot) & str_detect(Seedlot, "SMP7638"))

STDsf <- STDsf %>% mutate(SeedLONG = as.numeric(SeedLONG), SeedLAT = as.numeric(SeedLAT))

update_coords_lookup <- tibble(
  ID = c(
    "011001A010300124000",
    "011008A320100202000",
    "011008A320100209000",
    "011001A050200092000"),
  new_SeedLAT = c(47.1900, 48.1034, 48.1034, 48.1034),
  new_SeedLONG = c(-113.5760, -114.1054, -114.1054, -114.1054))

STDsf <- STDsf %>%
  left_join(update_coords_lookup, by = "ID") %>%
  mutate(SeedLAT = coalesce(new_SeedLAT, SeedLAT),
   SeedLONG = coalesce(new_SeedLONG, SeedLONG)  ) %>% 
  select(-new_SeedLAT, -new_SeedLONG)


##SAVE CSV AND WORKSPACE
setwd("~/Google Drive/My Drive/Seed Transfer Project/Data/Merging")
STDsf2<-STDsf[,!names(STDsf) %in% "centroid_PlantingLoc"] #Remove list object for csv save
write.csv(st_drop_geometry(STDsf2),"SeedTransferData_Elev_Aug2025.csv", row.names = FALSE)
saveRDS(STDsf, "SeedTransferData_Elev_Aug2025.RDS")









