# Function to estimate the "best" quantiles to compute species elevational limits with a DEM

# 1. Keep only species with "true" min and max elevational limits already found in the literature.
# 2. Within these species, keep only the ones with an overlap % > 50.
# 3. Then, for each row (= one species in one mountain range), crop the DEM
#   --> 4. Estimate the quantile of elevation from 0.01 to 0.99.
#   --> 5. Calculate the deviation of each quantile with the true limits
# 6. Average the deviation for each quantile across all species and mountain ranges

# LOGIC: Because we compare mountain specific limits (extracted from the DEM for each mountain range) with "true" elevational limits that are 
# species specific but not mountain specific, we can have strong mismatches for widespread species. Therefore, we add a safety check
# with the overlap_pct argument, only selecting in this process species with an overlap percentage > 50%, to ensure that the species is
# specific to this mountain range or to this area (i.e. can include neighbouring mountain ranges).


estimate.quantile <- function (species, dem, overlap_threshold) {
  
  # Keep only species that have min & max elevational limits 
  species <- species %>%
    filter(!is.na(min_elevation) & !is.na(max_elevation))
  
  # Keep a subset of these species (e.g. only species that have >50% overlap)
  species <- species %>%
    filter(overlap_pct > overlap_threshold)
  
  probs <- seq(0.01, 0.99, by = 0.01)
  results_list <- vector("list", nrow(species))
  
  for (i in seq_len(nrow(species))) {
    message(sprintf("Processing row %d/%d: %s in %s", 
                    i, nrow(species), species[i,]$sciname, species[i,]$Mountain_range))
    
    # get "true" limits for this species
    true_min <- species[i, ]$min_elevation
    true_max <- species[i, ]$max_elevation

    # Crop DEM to species x mountain area 
    elev_values <- tryCatch({
      exactextractr::exact_extract(dem, species[i, ], fun = NULL)[[1]]$value
    }, error = function(e) {
      message("Extraction failed for row ", i, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(elev_values) || length(elev_values) == 0) {
      message("No elevation values for row ", i, " — skipping")
      next
    }
    
    # Estimate the quantiles of elevation
    all_quantiles <- quantile(elev_values, probs = probs, na.rm = TRUE) 

    # Compute deviation of each quantile with "true limit"
    results_list[[i]] <- data.frame(
      sciname  = species[i, ]$sciname,
      Mountain_range = species[i, ]$Mountain_range,
      quantile = probs,
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
