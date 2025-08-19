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


##Year 1 Survival
  #weighted linear regression for labels
    model <- lm(Survival_1 ~ elev_diff, data = plot_data, weights = nPlanted)
    slope <- coef(model)[2]
    p_value <- summary(model)$coefficients[2, 4]
    
    slope_text <- sprintf("Slope = %.3f", slope)
    p_value_text <- sprintf("p-value = %.3f", p_value)
    label_text <- paste(slope_text, "\n", p_value_text)
  
  ##how many records are complete? #1756 records for Year 1
  dim(plot_data %>% rowwise() %>% filter(!any(is.na(c(PlantedElevation, SeedElevation, Survival_1, nPlanted, elev_diff)))))
  
  #Year 1 survival plot
  ggplot(plot_data, aes(elev_diff, Survival_1, weight = nPlanted)) +
    geom_point(aes(size = nPlanted, col=Species)) +
    geom_smooth(method = 'lm', se = TRUE) +
    annotate("text",
      x = min(plot_data$elev_diff, na.rm = TRUE), 
      y = max(plot_data$Survival_1, na.rm = TRUE) * 0.9, 
      hjust = 0,  vjust = 1, label = label_text, size = 5) +
    ggtitle("Survival (Year 1) x SeedSource") +
    xlab('Elevation Difference (Site - Source, meters)') +
    ylab('Survival [%]') +
    labs(size = "N Tree") +
    theme_minimal() +
    theme(text = element_text(size = 18))
  
  #Year 1 survival plot - LM by species (if more than 100 records)
  table(plot_data$Species)
  # Count the number of rows for each species
  species_counts <- STDsf %>%
                      group_by(Species) %>%
                      tally() %>%
                      filter(n > 100)
  
  # Filter the data to include only species with more than 100 rows for the lines
  plot_data_filtered_lines <- STDsf %>% filter(Species %in% species_counts$Species)
  
  # Year 1 survival plot with lines only for species with > 100 rows
  ggplot(STDsf, aes(elev_diff, Survival_3, weight = nPlanted, col = Species)) +
    geom_point(aes(size = nPlanted)) +
    geom_smooth(data = plot_data_filtered_lines,method = 'lm',se = FALSE) +
    ggtitle("Survival (Year 3) x SeedSource") +
    xlab('Elevation Difference (Site - Source, meters)') +
    ylab('Survival [%]') +
    labs(size = "N Tree") +
    theme_minimal() +
    theme(text = element_text(size = 18))
  
  
  
  
  
##Year 3 Survival
  #weighted linear regression for labels
  model <- lm(Survival_3 ~ elev_diff, data = STDsf, weights = nPlanted)
  slope <- coef(model)[2]
  p_value <- summary(model)$coefficients[2, 4]
  
  slope_text <- sprintf("Slope = %.3f", slope)
  p_value_text <- sprintf("p-value = %.3f", p_value)
  label_text <- paste(slope_text, "\n", p_value_text)
  
  ##how many records are complete? #1001 records for Year 3
  dim(STDsf %>% rowwise() %>% filter(!any(is.na(c(PlantedElevation, SeedElevation, Survival_3)))))
  
  #Year 3 survival plot
  ggplot(STDsf[STDsf$nPlanted>=1,], aes(elev_diff, Survival_3, weight = nPlanted)) +
    geom_point(aes(size = nPlanted, col=Species)) +
    geom_smooth(method = 'lm', se = TRUE) +
    annotate("text",
      x = min(STDsf$elev_diff, na.rm = TRUE), 
      y = max(STDsf$Survival_3, na.rm = TRUE) * 0.9, 
      hjust = 0,  vjust = 1, label = label_text, size = 5) +
    ggtitle("Survival (Year 3) x SeedSource") +
    xlab('Elevation Difference (Site - Source, meters)') +
    ylab('Survival [%]') +
    labs(size = "N Tree") +
    theme_minimal() +
    theme(text = element_text(size = 18))
  

  
  
##Year 5 Survival
  #weighted linear regression for labels
  model <- lm(Survival_5 ~ elev_diff, data = plot_data, weights = nPlanted)
  slope <- coef(model)[2]
  p_value <- summary(model)$coefficients[2, 4]
  
  slope_text <- sprintf("Slope = %.3f", slope)
  p_value_text <- sprintf("p-value = %.3f", p_value)
  label_text <- paste(slope_text, "\n", p_value_text)
  
  #Simplified dataset for plotting (no geometry)
  STDsf %>%
    select(PlantedElevation, SeedElevation, Survival_5, nPlanted, elev_diff) -> plot_data
  
  ##how many records are complete? #1756 records for Year 1
  dim(STDsf %>% rowwise() %>% filter(!any(is.na(c(Survival_5)))))
  
  #Year 5 survival plot
  ggplot(STDsf, aes(elev_diff, Survival_5, weight = nPlanted)) +
    geom_point(aes(size = nPlanted)) +
    geom_smooth(method = 'lm', se = TRUE) +
    annotate("text",
             x = min(plot_data$elev_diff, na.rm = TRUE) + 600, 
             y = max(plot_data$Survival_5, na.rm = TRUE) * 0.9, 
             hjust = 0,  vjust = 1, label = label_text, size = 5) +
    ggtitle("Survival (Year 5) x SeedSource") +
    xlab('Elevation Difference (Site - Source, meters)') +
    ylab('Survival [%]') +
    xlim(-300,250) + ylim(0,100) +
    labs(size = "N Tree") +
    theme_minimal() +
    theme(text = element_text(size = 18))
  
  
  
  
##How many records were complete? All years
  # Define the survival columns you're interested in
  survival_cols <- paste0("Survival_", 1:5)
  
  # Create a simplified dataset with the survival columns
  plot_data_survival <- STDsf %>%
    select(PlantedElevation, SeedElevation, nPlanted, elev_diff, all_of(survival_cols))
  
  # Function to count records where at least one of the specified columns is NOT NA
  count_any_not_na <- function(df, cols) {
    not_na_records <- df %>%
      rowwise() %>%
      filter(any(!is.na(c(PlantedElevation, SeedElevation, nPlanted, elev_diff, !!!syms(cols)))))
    return(nrow(not_na_records))
  }
  
  # Get the number of records where at least one of the survival columns is NOT NA
  any_survival_not_na_count <- count_any_not_na(plot_data_survival, survival_cols)
  any_survival_not_na_count
  
##############################  

##Calculate and compare sizes of polygon geometries
if(any(st_geometry_type(STDsf) %in% c("POLYGON", "MULTIPOLYGON"))) {
    STDsf$area <- st_area(STDsf)
    summary(STDsf$area)} else {cat("The geometry column does not contain polygons for area calculation.\n")}

class(STDsf$area)
summary(STDsf$area)
hist(as.numeric(log(STDsf$area)))
STDsf %>%  arrange(desc(area))



####Barplot of seedlots by region
unique_seedlot_counts_by_region <- STDsf %>%
  st_drop_geometry() %>% # Drop geometry if it's an sf object
  as_tibble() %>%        # Convert to tibble for dplyr operations
  filter(!is.na(Region)) %>% # Optional: Exclude rows where Region itself is NA if you don't want an "NA" bar
  group_by(Region) %>%
  summarise(
    UniqueSeedlotCount = n_distinct(Seedlot, na.rm = TRUE), # Count unique non-NA Seedlot values
    .groups = 'drop' # Drop the grouping structure
  ) %>%
  arrange(Region) # Arrange by Region for a consistent plot order

# 2. Create the bar plot
ggplot(unique_seedlot_counts_by_region, aes(x = Region, y = UniqueSeedlotCount, fill = Region)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) + # stat="identity" uses the 'y' value directly
  geom_text(aes(label = UniqueSeedlotCount), vjust = -0.5, size = 3.5) + # Add counts on top of bars
  labs(
    title = "Number of Unique Seedlots per Region", subtitle="(with matched nursery data; July 2025",
    x = "Region",
    y = "Count of Unique Seedlots"
  ) +
  theme_classic() + # A clean theme
  theme(
    axis.text.x = element_text(hjust = 1), 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5) )







##What species do we have data on?
#White bark pine (n=4) all planted at interp sites
table(STD$Species)
SppPlot<-ggplot(STD, aes(Species)) + geom_histogram(stat="count", fill="purple4") + theme_classic()
#ggsave("SppPlot.pdf",SppPlot, height=3, width=4, dpi=300)
SppPlot


##Survival daat or not
#Plot of NA vs non NA survival data
ggplot(STD, aes(x = factor(survival_data, levels = c(TRUE, FALSE), labels = c("Yes", "No")), 
                y = after_stat(count),fill = factor(survival_data, levels = c(TRUE, FALSE), labels = c("Yes", "No")))) +
  geom_bar(stat = "count") +
  labs(title = "Survival Data Counts",
       x = "Survival Data",
       y = "Count",
       fill = "Survival Data"  ) +
  scale_fill_manual(values = c("olivedrab3", "red")) +
  geom_text(aes(label = ..count..), vjust = -0.3, stat = "count") +
  theme_classic() +
  theme(legend.position = "none")

