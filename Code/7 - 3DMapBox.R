# Install mapdeck and related packages if you haven't already
# install.packages("mapdeck")
library(mapdeck)
library(dplyr)
library(sf)
library(remotes)
library(here)
library(scales)
library(htmltools)

# Set Mapbox token
set_token("pk.eyJ1IjoiYW55YW5vdmFtZXRjYWxmZSIsImEiOiJjbWIxaHQyZDMwNzFhMmpxMnVxMnZuYWI3In0.VW2iCCLQuVkDbjZj-EEVvg")

##Filter the data
STDsf<- readRDS("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/Data/Merging/SeedTransferData_Elev_Aug2025.RDS")
STDsf$centroid_PlantingLoc_sfc <- st_sfc(STDsf$centroid_PlantingLoc, crs = st_crs(STDsf))

# Extract planting coordinates from the newly created centroid_PlantingLoc_sfc column
STDsf_coords <- STDsf %>%
  mutate(
    PlantingLon = sf::st_coordinates(centroid_PlantingLoc_sfc)[,1], # Extract Longitude from the proper sfc object
    PlantingLat = sf::st_coordinates(centroid_PlantingLoc_sfc)[,2]  # Extract Latitude from the proper sfc object
  ) %>%
  # Select only the columns needed for plotting and filtering
  select(ID, Seedlot,Species, SeedLAT, SeedLONG, PlantingLon, PlantingLat)

# Filter out species with less than 100 records
species_counts <- STDsf_coords %>%
  group_by(Species) %>%
  summarise(n_records = n()) %>%
  filter(n_records >= 50)

filtered_data <- STDsf_coords %>%
  filter(Species %in% species_counts$Species) %>%
  # Remove rows where planting coordinates might be NA after extraction (e.g., from empty centroids)
  filter(!is.na(PlantingLon) & !is.na(PlantingLat))

# Prepare arc data
arc_data <- filtered_data %>%
  filter(!is.na(SeedLAT), !is.na(SeedLONG), !is.na(PlantingLat), !is.na(PlantingLon)) %>%
  mutate(origin_lat = SeedLAT,
    origin_lon = SeedLONG,
    dest_lat = PlantingLat,
    dest_lon = PlantingLon) %>%
    st_drop_geometry()

# Create destination points with species
dest_points <- arc_data %>%
  select(Seedlot, lon = dest_lon, lat = dest_lat, Species)

# Create a vector of unique species
species_list <- sort(unique(dest_points$Species))  # Sort to ensure consistent order
# Use mapdeck's default color scheme (similar to viridis)
library(viridis)
mapdeck_colors <- viridis(n = length(species_list), option = "D")
names(mapdeck_colors) <- species_list

# Build the legend HTML first
legend_items <- sapply(species_list, function(species) {
  color <- mapdeck_colors[[species]]
  sprintf("<span style='color:%s; font-size: 16px;'>&#9679;</span> %s<br>", color, species)
})

legend_div <- htmltools::div(
  style = "position: absolute; bottom: 30px; left: 30px; background: white; padding: 10px; border-radius: 5px; font-family: sans-serif; font-size: 14px; box-shadow: 0 0 10px rgba(0,0,0,0.3); z-index: 1000;",
  htmltools::strong("Species Legend"),
  htmltools::br(),
  htmltools::HTML(paste0(legend_items, collapse = ""))
)

# Create the map with 3D arcs and points for tree planting locations
# Note: The fill_colour is now linked to the "Species" column directly, which is the correct way
# to use mapdeck's built-in colour scaling and legend.
mapdeck_map <- mapdeck(style = mapdeck_style("light"), pitch = 55, zoom = 8) %>%
  add_arc(
    data = arc_data,
    origin = c("origin_lon", "origin_lat"),
    destination = c("dest_lon", "dest_lat"),
    stroke_from = "Species",
    stroke_to = "Species",
    auto_highlight = TRUE,
    tooltip = "Seedlot",
    update_view = FALSE,
    layer_id = "arc_layer",
    width = 2,
    height = 1
  ) %>%
  add_scatterplot(
    data = dest_points,
    lon = "lon",
    lat = "lat",
    radius = 5000, #larger for destination to differentiate (in M)
    fill_colour = "Species",
    fill_opacity = 255,
    stroke_width = 15,
    stroke_colour = "#000000", # Black border
    tooltip = "Seedlot",
    layer_id = "Seedlot")

# Now, prepend the legend to the map object after it's been created
mapdeck <- htmlwidgets::prependContent(mapdeck_map, legend_div)

mapdeck

library(htmlwidgets)

setwd("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/Figures")
saveWidget(
  widget = mapdeck,
  file = "tree_transfer_map_NSD.html",
  selfcontained = TRUE)
