# Post-classification change detection between 2000 and 2022 for forest patches
# Script for generating change detection maps with proper class labels

# Load required packages
library(raster)
library(terra)
library(sf)
library(stringr)

# Define the forest patches
sites <- c("Agou_hill_forest_FP2", "Elavagnon_Todji_FP2", "Ewè_Adakplamè_FP2",
           "Hlanzoun_FP2", "Iko_forest_FP2", "Ikot_swamp_FP2", 
           "Koui_FP2", "Mbangassina_FP2", "Ngam_Kondomeyos_FP2")

# Define paths
lc_2000_path <- "C:/Users/chima/OneDrive - Universitaet Bern/Documents/SUSTAINFORESTS/Papers/SUSTAINFORESTS Papers/Local perspectives and Spatiotemporal dynamics/Data/LULC/LandCover_2000.tif"
lc_2022_path <- "C:/Users/chima/OneDrive - Universitaet Bern/Documents/SUSTAINFORESTS/Papers/SUSTAINFORESTS Papers/Local perspectives and Spatiotemporal dynamics/Data/LULC/LandCover_2022.tif"
output_folder <- "C:/Users/chima/OneDrive - Universitaet Bern/Documents/SUSTAINFORESTS/Papers/SUSTAINFORESTS Papers/Local perspectives and Spatiotemporal dynamics/Data/LULC/Clipped"
boundary_dir <- "C:/Users/chima/OneDrive - Universitaet Bern/Documents/SUSTAINFORESTS/Papers/SUSTAINFORESTS Papers/Local perspectives and Spatiotemporal dynamics/Data"

# Define class codes and names
class_codes <- c(1, 2, 3, 4, 5, 6, 7)
class_names <- c("Sparse vegetation", "Shrubland", "Treecover", "Wetland", 
                 "Waterbody", "Cropland", "Builtup area")

# Create a named vector for class lookup
class_lookup <- setNames(class_names, class_codes)

# Create output directory if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Load the classified rasters using terra package
lc_2000 <- rast(lc_2000_path)
lc_2022 <- rast(lc_2022_path)

# Check if the rasters have the same CRS and resolution
if (!identical(crs(lc_2000), crs(lc_2022))) {
  stop("The two rasters have different coordinate reference systems!")
}

# Process each forest patch
for (site in sites) {
  # Print progress
  cat(paste0("Processing ", site, "...\n"))
  
  # Construct the shapefile path
  shapefile_path <- file.path(boundary_dir, paste0(site, ".shp"))
  
  # Check if shapefile exists
  if (!file.exists(shapefile_path)) {
    cat(paste0("Warning: Shapefile not found at ", shapefile_path, ". Skipping...\n"))
    next
  }
  
  # Load the forest patch boundary
  fp <- vect(shapefile_path)
  
  # Make sure the shapefile has the same CRS as the rasters
  if (!identical(crs(fp), crs(lc_2000))) {
    fp <- project(fp, crs(lc_2000))
  }
  
  # Clip the rasters to the forest patch boundary
  lc_2000_clip <- crop(lc_2000, fp)
  lc_2000_clip <- mask(lc_2000_clip, fp)
  
  lc_2022_clip <- crop(lc_2022, fp)
  lc_2022_clip <- mask(lc_2022_clip, fp)
  
  # Create change detection map
  # Unique code for each transition: old_class * 1000 + new_class
  change_map <- lc_2000_clip * 1000 + lc_2022_clip
  
  # Save the clipped rasters with proper categories
  output_2000 <- file.path(output_folder, paste0(site, "_2000.tif"))
  output_2022 <- file.path(output_folder, paste0(site, "_2022.tif"))
  output_change <- file.path(output_folder, paste0(site, "_change.tif"))
  
  # Add category labels to the 2000 and 2022 rasters
  cats_2000 <- data.frame(value = class_codes, label = class_names)
  levels(lc_2000_clip) <- cats_2000
  
  cats_2022 <- data.frame(value = class_codes, label = class_names)
  levels(lc_2022_clip) <- cats_2022
  
  # Create categories for the change raster
  # Generate all possible transitions
  change_cats <- expand.grid(from = class_codes, to = class_codes)
  change_cats$value <- change_cats$from * 1000 + change_cats$to
  change_cats$label <- paste0(class_lookup[as.character(change_cats$from)], " to ", 
                              class_lookup[as.character(change_cats$to)])
  
  # Add these categories to the change raster
  levels(change_map) <- change_cats[, c("value", "label")]
  
  # Write the outputs
  writeRaster(lc_2000_clip, output_2000, overwrite = TRUE)
  writeRaster(lc_2022_clip, output_2022, overwrite = TRUE)
  writeRaster(change_map, output_change, overwrite = TRUE)
  
  # Create a legend file for the change map
  legend_file <- file.path(output_folder, paste0(site, "_change_legend.csv"))
  write.csv(change_cats, legend_file, row.names = FALSE)
  
  cat(paste0("Completed processing for ", site, ". Output saved to ", output_change, "\n"))
  cat(paste0("Change legend saved to ", legend_file, "\n"))
  
  # Create a categorical raster specifically for visualization
  # This is optional but may help with visualizing the results later
  # We'll focus on a few key transitions related to forest change
  # 1. Stable forest (Treecover to Treecover)
  # 2. Forest loss (Treecover to anything else)
  # 3. Forest gain (anything else to Treecover)
  # 4. Other changes (all other transitions)
  
  forest_change <- app(change_map, function(x) {
    from_class <- x %/% 1000
    to_class <- x %% 1000
    
    # 1 = Stable forest, 2 = Forest loss, 3 = Forest gain, 4 = Other change, 5 = No data
    ifelse(is.na(x), 5,
           ifelse(from_class == 3 & to_class == 3, 1,  # Stable forest
                  ifelse(from_class == 3 & to_class != 3, 2,  # Forest loss
                         ifelse(from_class != 3 & to_class == 3, 3,  # Forest gain
                                4))))  # Other change
  })
  
  # Set categories for the forest change raster
  cats_fc <- data.frame(
    value = c(1, 2, 3, 4, 5),
    label = c("Stable forest", "Forest loss", "Forest gain", "Other change", "No data")
  )
  levels(forest_change) <- cats_fc
  
  # Save the forest change raster
  output_fc <- file.path(output_folder, paste0(site, "_forest_change.tif"))
  writeRaster(forest_change, output_fc, overwrite = TRUE)
  
  cat(paste0("Forest change classification saved to ", output_fc, "\n"))
}

cat("All forest patches processed successfully!\n")

# Print a note about how to interpret the change codes
cat("\nNOTE: The change rasters encode land cover transitions as follows:\n")
cat("For example, code 3006 means a change from 'Treecover' (class 3) to 'Cropland' (class 6).\n")
cat("A legend file has been saved for each forest patch for easy reference.\n")
cat("Additionally, a simplified forest change raster was created focusing on forest dynamics.\n")



# Code to display the forest change rasters (fixed version)

# Load required packages
library(terra)
library(ggplot2)
library(RColorBrewer)
library(patchwork)  # For arranging multiple plots

# Define the output folder and forest patches
output_folder <- "C:/Users/chima/OneDrive - Universitaet Bern/Documents/SUSTAINFORESTS/Papers/SUSTAINFORESTS Papers/Local perspectives and Spatiotemporal dynamics/Data/LULC/Clipped"
sites <- c("Agou_hill_forest_FP2", "Elavagnon_Todji_FP2", "Ewè_Adakplamè_FP2",
           "Hlanzoun_FP2", "Iko_forest_FP2", "Ikot_swamp_FP2", 
           "Koui_FP2", "Mbangassina_FP2", "Ngam_Kondomeyos_FP2")

# Set colors for forest change categories using hex codes
forest_change_colors <- c(
  "Stable forest" = "#267300",  # sprucegreen
  "Forest loss" = "#FF0000",    # red
  "Forest gain" = "#0070FF",    # blue
  "Other change" = "#00000000", # transparent
  "No data" = "#00000000"       # transparent
)

# Function to plot forest change for a single site
plot_forest_change <- function(site) {
  # Construct file path
  file_path <- file.path(output_folder, paste0(site, "_forest_change.tif"))
  
  # Check if file exists
  if (!file.exists(file_path)) {
    cat(paste0("Forest change file not found for ", site, ".\n"))
    return(NULL)
  }
  
  # Load forest change raster
  forest_change <- rast(file_path)
  
  # Print raster summary information
  cat(paste0("\nSummary for ", site, " forest change:\n"))
  print(forest_change)
  
  # Get layer categories (if they exist)
  if (!is.null(levels(forest_change))) {
    cat("Categories:\n")
    print(levels(forest_change)[[1]])
    
    # Calculate area for each category (in hectares)
    # Alternative approach without using categories=TRUE parameter
    fc_df <- as.data.frame(forest_change, xy=TRUE)
    colnames(fc_df)[3] <- "value"
    
    # Get cell area in hectares (assuming WGS84 projection)
    cell_res <- res(forest_change)
    cell_area_ha <- prod(cell_res) * 111.32 * 111.32 * cos(mean(ext(forest_change)[3:4]) * pi/180) * 10000
    
    # Count cells by category and multiply by cell area
    areas <- data.frame(table(fc_df$value))
    colnames(areas) <- c("Value", "Count")
    areas$Area_ha <- areas$Count * cell_area_ha
    
    # Join with category labels
    if (!is.null(levels(forest_change))) {
      cat_labels <- levels(forest_change)[[1]]
      areas$Category <- cat_labels$label[match(areas$Value, cat_labels$value)]
    } else {
      areas$Category <- paste("Category", areas$Value)
    }
    
    cat("Area by category (hectares):\n")
    print(areas[, c("Category", "Area_ha")])
  }
  
  # Convert to a data frame for ggplot2
  forest_df <- as.data.frame(forest_change, xy=TRUE)
  colnames(forest_df)[3] <- "value"
  
  # Plot
  p <- ggplot() +
    geom_raster(data = forest_df, aes(x = x, y = y, fill = factor(value))) +
    scale_fill_manual(
      values = forest_change_colors,
      name = "Forest Change",
      labels = c("1" = "Stable forest", "2" = "Forest loss", 
                 "3" = "Forest gain", "4" = "Other change", "5" = "No data")
    ) +
    ggtitle(paste0(site, " Forest Change 2000-2022")) +
    coord_equal() +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
  
  return(p)
}

# Option 1: Process and display all sites one by one
for (site in sites) {
  cat(paste0("\nProcessing ", site, "...\n"))
  p <- plot_forest_change(site)
  if (!is.null(p)) {
    print(p)
    
    # Optionally save the plot
    # ggsave(paste0(output_folder, "/", site, "_forest_change_plot.png"), 
    #       p, width = 8, height = 6, dpi = 300)
  }
}

# Option 2: Save all plots in one file
all_plots <- list()
for (i in 1:length(sites)) {
  all_plots[[i]] <- plot_forest_change(sites[i])
}

# Filter out any NULL plots
all_plots <- all_plots[!sapply(all_plots, is.null)]

# If there are plots to display, combine them
if (length(all_plots) > 0) {
  # Arrange plots in a grid with 2 columns
  combined_plot <- wrap_plots(all_plots, ncol = 2)
  
  # Display the combined plot
  print(combined_plot)
  
  # Save the combined plot
  # ggsave(paste0(output_folder, "/all_forest_change_plots.png"), 
  #       combined_plot, width = 12, height = 8, dpi = 300)
}