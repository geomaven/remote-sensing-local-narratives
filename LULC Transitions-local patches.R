# Load libraries
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(reshape2)
library(openxlsx)  # <--- New library to write Excel

# 1) Define inputs
raster_dir <- "C:/Users/chima/OneDrive - Universitaet Bern/Documents/SUSTAINFORESTS/Papers/SUSTAINFORESTS Papers/Local perspectives and Spatiotemporal dynamics/Data/LULC"
shp_dir    <- "C:/Users/chima/OneDrive - Universitaet Bern/Documents/SUSTAINFORESTS/Papers/SUSTAINFORESTS Papers/Local perspectives and Spatiotemporal dynamics/Data"
patches <- c(
  "Agou_hill_forest_FP2", "Elavagnon_Todji_FP2", "Ewè_Adakplamè_FP2",
  "Hlanzoun_FP2", "Iko_forest_FP2", "Ikot_swamp_FP2",
  "Koui_FP2", "Mbangassina_FP2", "Ngam_Kondomeyos_FP2"
)
years <- c(2000, 2005, 2010, 2015, 2020, 2022)

# LULC class names
lulc_names <- c(
  "1" = "Sparse vegetation",
  "2" = "Shrubland",
  "3" = "Treecover",
  "4" = "Wetland",
  "5" = "Waterbody",
  "6" = "Cropland",
  "7" = "Builtup area"
)

# Initialize empty list to store transitions
all_transitions <- list()

generate_patch_transitions <- function(patch_name) {
  cat("Processing patch:", patch_name, "\n")
  
  patch_transitions <- data.frame(
    patch = character(),
    from = character(), to = character(), 
    year_from = numeric(), year_to = numeric(), 
    count = numeric(),
    stringsAsFactors = FALSE
  )
  
  shp_file <- file.path(shp_dir, paste0(patch_name, ".shp"))
  if (!file.exists(shp_file)) {
    warning("Missing shapefile: ", shp_file)
    return(NULL)
  }
  
  v <- vect(shp_file)
  
  for (i in 1:(length(years) - 1)) {
    year1 <- years[i]
    year2 <- years[i + 1]
    
    raster_file1 <- file.path(raster_dir, paste0("LandCover_", year1, ".tif"))
    raster_file2 <- file.path(raster_dir, paste0("LandCover_", year2, ".tif"))
    
    if (!file.exists(raster_file1) || !file.exists(raster_file2)) {
      warning("Missing raster for years ", year1, " or ", year2)
      next
    }
    
    r1 <- rast(raster_file1)
    r2 <- rast(raster_file2)
    
    if (crs(v) != crs(r1)) {
      v <- project(v, crs(r1))
    }
    
    r1sub <- mask(crop(r1, v), v)
    r2sub <- mask(crop(r2, v), v)
    
    stk <- c(r1sub, r2sub)
    names(stk) <- c("year1", "year2")
    vals <- terra::values(stk)
    
    vals <- na.omit(vals)
    
    from_vals <- as.character(vals[, "year1"])
    to_vals   <- as.character(vals[, "year2"])
    
    trans_counts <- table(from_vals, to_vals)
    
    for (from_class in rownames(trans_counts)) {
      for (to_class in colnames(trans_counts)) {
        count_val <- trans_counts[from_class, to_class]
        if (count_val > 0) {
          new_row <- data.frame(
            patch = patch_name,
            from = lulc_names[from_class],
            to = lulc_names[to_class],
            year_from = year1,
            year_to = year2,
            count = count_val,
            stringsAsFactors = FALSE
          )
          patch_transitions <- rbind(patch_transitions, new_row)
        }
      }
    }
  }
  
  return(patch_transitions)
}

# 2) Loop through all patches
for (patch in patches) {
  trans <- generate_patch_transitions(patch)
  if (!is.null(trans)) {
    all_transitions[[patch]] <- trans
  }
}

# 3) Combine all into one big dataframe
all_trans_df <- bind_rows(all_transitions)

# 4) Convert pixel count to hectares
all_trans_df <- all_trans_df %>%
  mutate(hectares = count * 0.09) %>%
  select(patch, year_from, year_to, from, to, hectares)

# 5) Write to Excel
write.xlsx(all_trans_df, file = file.path(raster_dir, "ForestPatch_LULC_Transitions.xlsx"))

cat("Excel file created successfully!\n")
