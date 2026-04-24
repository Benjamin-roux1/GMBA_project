
estimate.quantile <- function (species, dem, overlap_threshold) {
  
  # Keep only species that have min & max elevational limits 
  species <- species %>%
    filter(!is.na(min_elevation) & !is.na(max_elevation))
  
  # Keep a subset of these species (e.g. only species that have >50% overlap)
  species <- species %>%
    filter(overlap_pct > overlap_threshold)
  
  results_list <- vector("list", nrow(species))
  
  for (i in seq_len(nrow(species))) {
    message("Processing row ", i, " out of ", nrow(species))
    
    # get "true" limits for this species
    true_min <- species[i, ]$min_elevation
    true_max <- species[i, ]$max_elevation
    range_size <- true_max - true_min
    
    # Crop DEM to species x mountain area 
    dem_crop <- terra::crop(dem, terra::vect(species[i, ]))
    dem_mask <- terra::mask(dem_crop, terra::vect(species[i, ]))
    elev_values <- terra::values(dem_mask, na.rm = TRUE)
    
    # Estimate the quantiles of elevation
    all_quantiles <- quantile(elev_values, probs = seq(0.01, 0.99, by = 0.01)) 

    # Compute deviation of each quantile with "true limit"
    results_list[[i]] <- data.frame(
      sciname  = species[i, ]$sciname,
      Mountain_range = species[i, ]$Mountain_range,
      quantile = seq(0.01, 0.99, by = 0.01),
      dev_min = abs(all_quantiles - true_min),
      dev_max  = abs(all_quantiles - true_max)
    )
  }
  
  # Combine all results
  results <- bind_rows(results_list)
  
  # mean deviation per quantile
  summary <- results %>%
    group_by(quantile) %>%
    summarise(
      mean_dev_min = mean(dev_min, na.rm = TRUE),
      mean_dev_max = mean(dev_max, na.rm =TRUE),
      .groups = "drop"
    )
  
  return(summary)
}
