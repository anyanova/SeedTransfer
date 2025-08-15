##Density plots of OSF data paired with STDsf
library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(patchwork) 
library(geodist)

##Read in data with clim vars (PRISM + CWD)
STDsf <- readRDS("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/Data/SeedTransferData_OSFclim_July2025.rds")
colnames(STDsf)
dim(STDsf)

##Create columns with differences PlantingLoc-SeedLoc
STDsf$Elev_Diff<- (STDsf$PlantedElevation - STDsf$SeedElevation) *.3048
STDsf$CWD_Diff<- STDsf$def_Planting - STDsf$def_Seed
STDsf$AET_Diff<- STDsf$aet_Planting - STDsf$aet_Seed
STDsf$Tmax_Diff<- STDsf$tmax_Planting - STDsf$tmax_Seed
STDsf$Tmin_Diff<- STDsf$tmin_Planting - STDsf$tmin_Seed

#Add column with distance between Seed and Planting locs
#*Note that Geodist uses format of (LONG,LAT)
seed_coords <- cbind(STDsf$SeedLONG, STDsf$SeedLAT)

#Planting centroid coords are list of lat,long. Parse and then recombine.
char_coords <- as.character(unlist(STDsf$centroid_PlantingLoc))
parsed_coords_matrix <- matrix(as.numeric(char_coords), ncol = 2, byrow = TRUE)
STDsf$centroid_PlantingLoc_LONG <- parsed_coords_matrix[, 1] # First column is Longitude
STDsf$centroid_PlantingLoc_LAT <- parsed_coords_matrix[, 2]  # Second column is Latitude
planting_coords <- cbind(STDsf$centroid_PlantingLoc_LONG, STDsf$centroid_PlantingLoc_LAT)


#New column with distances between planting and seed   
STDsf$GeographicalDistance_km <- diag(geodist(x = seed_coords, y = planting_coords, measure = "haversine"))/1000



# --- 1. Define the Difference Columns to Plot ---
diff_plot_columns <- c("Elev_Diff","GeographicalDistance_km","Tmax_Diff","Tmin_Diff","CWD_Diff","AET_Diff")

# --- 2. Create a Named List for Custom Titles and Subtitles ---
plot_metadata <- list(
  "Elev_Diff" = list(title = "Elevation Difference (m)"),
  "GeographicalDistance_km" = list(title = "Geographical Distance (km)"),
  "Tmax_Diff" = list(title = "Max. July Temp. (°C)"),
  "Tmin_Diff" = list(title = "Min. Jan. Temp. (°C)"),
  "CWD_Diff" = list(title = "Climatic Water Deficit (mm)"),
  "AET_Diff" = list(title = "Actual Evapotranspiration (mm)"))


# --- 3. Prepare Data for Plotting (Long Format) ---
STDsf_diff_long <- STDsf %>%
  st_drop_geometry() %>%
  select(all_of(diff_plot_columns)) %>% # Select only the desired difference columns
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value))

# --- IMPORTANT: Convert 'Variable' to a factor with the desired order
# This ensures ggplot respects the order defined in diff_plot_columns
STDsf_diff_long$Variable <- factor(STDsf_diff_long$Variable, levels = diff_plot_columns)

# --- 4. Calculate Summary Statistics for Each Variable ---
summary_stats_diff <- STDsf_diff_long %>%
  group_by(Variable) %>%
  summarise(
    Mean_Value = mean(Value, na.rm = TRUE),
    Median_Value = median(Value, na.rm = TRUE),
    SD_Value = sd(Value, na.rm = TRUE),
    Min_Value = min(Value, na.rm = TRUE),
    Max_Value = max(Value, na.rm = TRUE),
    .groups = 'drop'
  )

# --- 5. Generate Data Points for Fitted Normal Curves (Robustly) ---
normal_curves_data_all <- summary_stats_diff %>%
  group_by(Variable) %>%
  reframe({
    if (SD_Value > 0 && Min_Value < Max_Value) {
      # Calculate range buffer dynamically based on 10% of the actual range
      range_buffer <- (Max_Value - Min_Value) * 0.1
      x_vals <- seq(Min_Value - range_buffer, Max_Value + range_buffer, length.out = 500)
      
      tibble(
        Value = x_vals,
        Density = dnorm(x_vals, mean = Mean_Value, sd = SD_Value)
      )
    } else {
      # Return empty tibble if normal curve cannot be plotted
      tibble(Value = numeric(0), Density = numeric(0))
    }
  }) %>%
  ungroup()

# --- 6. 6 panel plot of all vars
plot_list <- list()
bins_count <- 30 # Number of bins for the histogram

for (var_name in diff_plot_columns) { # Loop through the reordered list of *selected* variables
  current_data <- STDsf_diff_long %>% filter(Variable == var_name)
  current_summary <- summary_stats_diff %>% filter(Variable == var_name)
  current_normal_curve_data <- normal_curves_data_all %>% filter(Variable == var_name)
  
  # Determine if a normal curve can be plotted for this variable
  # Do not plot normal curve for GeographicalDistance_km
  plot_normal_curve_flag <- (nrow(current_summary) > 0 &&
                               current_summary$SD_Value > 0 &&
                               current_summary$Min_Value < current_summary$Max_Value &&
                               var_name != "GeographicalDistance_km") # NEW: Exclude GeographicalDistance_km
  
  # Calculate max density for label positioning
  hist_info <- hist(current_data$Value, plot = FALSE, breaks = bins_count)
  max_hist_density <- max(hist_info$density, na.rm = TRUE)
  
  # Prepare label data for this plot
  # Adjust Median_Label_X and hjust for reversed axes (Elev_Diff, AET_Diff)
  if (var_name %in% c("Elev_Diff", "AET_Diff")) {
    # Place label on the "positive" side (left side of reversed axis)
    label_x_pos <- current_summary$Max_Value - (current_summary$Max_Value - current_summary$Min_Value + 100) * 0.02
    label_hjust <- 0 # Right-align text with the calculated X position
  } else {
    # Original placement for other plots
    label_x_pos <- current_summary$Min_Value + (current_summary$Max_Value - current_summary$Min_Value) * 0.02
    label_hjust <- 0 # Left-align text
  }
  
  label_data_plot <- tibble(
    Variable = var_name,
    Median_Label_X = label_x_pos,
    Median_Label_Y = max_hist_density * 0.98,
    Median_Text = paste0("Median: ", round(current_summary$Median_Value, 2)))
  
  # Build the ggplot object for the current variable
  p <- ggplot(data = current_data, aes(x = Value)) +
  # Conditional histogram fill
    {if (var_name == "GeographicalDistance_km") {
        geom_histogram(aes(y = after_stat(density)),
                       fill = "white", # No gradient fill for geographical distance
                       color = "darkgray", alpha = 0.8, bins = bins_count)      } else {
        geom_histogram(aes(y = after_stat(density), fill = after_stat(x)),
                       color = "darkgray", alpha = 0.8, bins = bins_count,)}} +
    
    # Conditional fill gradient for colors
    {if (var_name != "GeographicalDistance_km") {
        if (var_name %in% c("Elev_Diff", "AET_Diff")) {
          # Flipped colors for reversed axis: negative (right) is red
          scale_fill_gradient2(
            low = "firebrick",    # For values much lower than midpoint (negative)
            mid = "white",
            high = "deepskyblue", # For values much higher than midpoint (positive)
            midpoint = 0,
            guide = "none")        } else {
          # Original colors for other plots
          scale_fill_gradient2(
            low = "deepskyblue", # Negative (left)
            mid = "white",
            high = "firebrick", # Positive (right)
            midpoint = 0,
            guide = "none")}}} +
    
    # Conditionally add the normal curve layer
    {if (plot_normal_curve_flag) { #exclude GeographicalDistance_km
        geom_line(data = current_normal_curve_data, aes(x = Value, y = Density),
                  color = "gray", linetype = "solid", linewidth = 1)}} +
    
    # Add vertical line for the median
    geom_vline(data = current_summary, aes(xintercept = Median_Value),
               color = "black", linetype = "dotted", linewidth = 1) +
    
    # Add median label
    geom_text(data = label_data_plot, aes(x = Median_Label_X, y = Median_Label_Y, label = Median_Text),
              color = "black", size = 3, hjust = label_hjust, vjust = 1, fontface = "bold") + 
    
    # Set plot titles and labels using the plot_metadata
    labs(title = plot_metadata[[var_name]]$title,
         subtitle = plot_metadata[[var_name]]$subtitle,
         x = "Difference Value",
         y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 5)),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          legend.position = "none",
          axis.text.y = element_text(size = 8))+
    scale_y_continuous(labels = scales::label_number(accuracy = 0.001), n.breaks=3) +
    
    # --- Conditionally flip X-axis for Elevation and AET plots ---
    {if (var_name %in% c("Elev_Diff", "AET_Diff")) scale_x_reverse()}
  
  plot_list[[var_name]] <- p
}


# --- 7. Combine All Plots Using patchwork ---
# The order of plots in plot_list (based on `diff_plot_columns`) will determine the final arrangement
combined_plots <- patchwork::wrap_plots(plot_list, ncol = 2)+
      plot_annotation(title = "Geographic and Climatic Differences",
                subtitle ="Planting site - Seed collection site",
                theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                              plot.subtitle = element_text(hjust = 0.5)))
# Print the final combined plot
print(combined_plots)

setwd("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/Figures")
ggsave("6pan_geoclim.pdf",combined_plots, width=7, height=5.5, units="in")
ggsave("6pan_geoclim.svg",combined_plots, width=7, height=5.5, units="in")
ggsave("6pan_geoclim.png",combined_plots, width=7, height=5.5, units="in")
