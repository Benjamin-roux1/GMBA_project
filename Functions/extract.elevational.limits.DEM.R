extract.elevational.limits.DEM <- function (species, dem, quantile_min, quantile_max) {
  
  results_list <- vector("list", nrow(species))
  
  for (i in seq_len(nrow(species))) {
    message("Processing row ", i, " out of ", nrow(species))
    
    # Crop DEM to species x mountain area 
    dem_crop <- terra::crop(dem, terra::vect(species[i, ]))
    dem_mask <- terra::mask(dem_crop, terra::vect(species[i, ]))
    elev_values <- terra::values(dem_mask, na.rm = TRUE)
    
    # Estimate the quantiles of elevation
    estimated_min <- quantile(elev_values, probs = quantile_min$quantile) 
    estimated_max <- quantile(elev_values, probs = quantile_max$quantile)
    
    results_list[[i]] <- data.frame(
      sciname  = species[i, ]$sciname,
      Mountain_range = species[i, ]$Mountain_range,
      min_elevation_DEM = estimated_min,
      max_elevation_DEM = estimated_max
    )
  }
  
  # Combine all results
  results <- dplyr::bind_rows(results_list)
  
  return(results)
}
