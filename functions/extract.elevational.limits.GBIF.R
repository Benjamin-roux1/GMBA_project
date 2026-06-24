# This function simply extract elevational limits from GBIF occurrences
#   1. extract the elevation for each occurrences based on latitude and longitude coordinates
#   2. group by species and mountain range (Level_03) and calculate the quantiles 0.05 and 0.95 to extract min and max elevational limits

# We calculate with all the species > 1 occurrences, i.e. even those with only 2 occurrences

extract.elevational.limits.GBIF <- function(species, dem) {
  
  message(sprintf("Extracting elevation for %d occurrences...", nrow(species)))
  
  pts_species <- species %>%
    dplyr::select(decimalLongitude, decimalLatitude)
  
  species$elevation <- terra::extract(dem, terra::vect(pts_species, crs = "EPSG:4326"))[, 2]
  
  message("Summarising by species and mountain range...")
  
  results <- species %>%
    group_by(Level_03, sciname) %>%
    filter(n() > 1) %>%
    summarise(
      NumberOcc = n(),
      min_elevation_GBIF = quantile(elevation, 0.05, na.rm = TRUE),
      max_elevation_GBIF = quantile(elevation, 0.95, na.rm = TRUE),
      Abs_min_elevation_GBIF = min(elevation, na.rm = TRUE),
      Abs_max_elevation_GBIF = max(elevation, na.rm = TRUE),
      .groups = "drop"
    )
  
  message(sprintf("Done! %d species-mountain range combinations retained", nrow(results)))
  
  return(results)
}