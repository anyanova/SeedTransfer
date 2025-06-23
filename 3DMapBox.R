# Install mapdeck and related packages if you haven't already
# install.packages("mapdeck")
library(mapdeck)
library(dplyr)
library(sf)
library(remotes)
library(here)
library(scales)

# Set Mapbox token
set_token("pk.eyJ1IjoiYW55YW5vdmFtZXRjYWxmZSIsImEiOiJjbWIxaHQyZDMwNzFhMmpxMnVxMnZuYWI3In0.VW2iCCLQuVkDbjZj-EEVvg")

##Filter the data

STD <- readRDS("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/Data/SeedTransferData_Elev_May2025.rds")
STDsf$centroid_PlantingLoc_sfc <- st_sfc(STDsf$centroid_PlantingLoc, crs = st_crs(STDsf))

# Ensure SeedLAT and SeedLONG are numeric
STDsf$SeedLAT <- as.numeric(STDsf$SeedLAT)
STDsf$SeedLONG <- as.numeric(STDsf$SeedLONG)

# Extract planting coordinates from the newly created centroid_PlantingLoc_sfc column
STDsf_coords <- STDsf %>%
  mutate(
    PlantingLon = sf::st_coordinates(centroid_PlantingLoc_sfc)[,1], # Extract Longitude from the proper sfc object
    PlantingLat = sf::st_coordinates(centroid_PlantingLoc_sfc)[,2]  # Extract Latitude from the proper sfc object
  ) %>%
  # Select only the columns needed for plotting and filtering
  select(ID, Species, SeedLAT, SeedLONG, PlantingLon, PlantingLat)

# Filter out species with less than 100 records
species_counts <- STDsf_coords %>%
  group_by(Species) %>%
  summarise(n_records = n()) %>%
  filter(n_records >= 100)

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
  select(lon = dest_lon, lat = dest_lat, Species) %>%
  mutate(label = "\U0001F332")  # 🌲 Tree emoji


##Legend
# Create a vector of unique species
species_list <- unique(dest_points$Species)
# Generate a named color palette
species_colors <- setNames(hue_pal()(length(species_list)), species_list)

# Build the legend HTML
legend_items <- sapply(names(species_colors), function(species) {
  color <- species_colors[[species]]
  sprintf("<span style='color:%s;'>&#9679;</span> %s<br>", color, species)})
legend_html <- sprintf("<div style='position: absolute; bottom: 30px; left: 30px; background: white; padding: 10px; border-radius: 5px; font-family: sans-serif; font-size: 14px; box-shadow: 0 0 10px rgba(0,0,0,0.3);'>
  <strong>Species Legend</strong><br>%s</div>",
                       paste0(legend_items, collapse = ""))

Í


# Create the map with 3D arcs, points for tree planting location
mapdeck_map<-mapdeck(style = mapdeck_style("light"), pitch = 55, zoom = 8) %>%
  add_arc(
    data = arc_data,
    origin = c("origin_lon", "origin_lat"),
    destination = c("dest_lon", "dest_lat"),
    stroke_from = "Species",
    stroke_to = "Species",
    auto_highlight = TRUE,
    tooltip = "Species",
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
      fill_opacity = 100,  # range: 0 (transparent) to 255 (opaque)
      stroke_width = 15,
      stroke_colour = "#000000", # Black border
      tooltip = "Species",
      layer_id = "destination_trees")

mapdeck_map


library(htmlwidgets)

saveWidget(
  widget = mapdeck_map,
  file = "tree_transfer_map.html",
  selfcontained = TRUE)


