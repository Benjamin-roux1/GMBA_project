# Function to extract elevational limits from the DEM.

# 1. Crop the DEM to each row (= species x mountain range)
# 2. Using the "best" quantiles estimated earlier, we calculate the estimated min and max elevation of the species in this MR

extract.elevational.limits.DEM <- function(species, dem, quantile_min, quantile_max) {
  
  # Filter out empty/broken geometries before extraction
  species <- species %>%
    filter(!st_is_empty(st_geometry(.))) %>%
    filter(st_is_valid(st_geometry(.))) %>%
    filter(as.numeric(st_area(st_geometry(.))) > 0)
  
  results_list <- vector("list", nrow(species))
  
  for (i in seq_len(nrow(species))) {
    message(sprintf("Processing row %d/%d: %s in %s",
                    i, nrow(species), species[i,]$sciname, species[i,]$Mountain_range))
    
    elev_values <- tryCatch({
      dem_crop <- terra::crop(dem, terra::vect(species[i, ]))
      dem_mask <- terra::mask(dem_crop, terra::vect(species[i, ]))
      terra::values(dem_mask, na.rm = TRUE)
    }, error = function(e) {
      message("Extraction failed for row ", i, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(elev_values) || length(elev_values) == 0) {
      message("No elevation values for row ", i, " — skipping")
      next
    }
    
    estimated_min <- quantile(elev_values, probs = quantile_min$quantile, na.rm = TRUE)
    estimated_max <- quantile(elev_values, probs = quantile_max$quantile, na.rm = TRUE)
    
    results_list[[i]] <- data.frame(
      sciname           = species[i, ]$sciname,
      Mountain_range    = species[i, ]$Mountain_range,
      min_elevation_DEM = estimated_min,
      max_elevation_DEM = estimated_max
    )
  }
  
  results <- dplyr::bind_rows(results_list)
  return(results)
}