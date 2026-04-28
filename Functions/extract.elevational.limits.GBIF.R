# 

extract.elevational.limits.GBIF <- function (species, dem) {
  
  # 1. Select only coordinates
  pts_species <- species %>%
    dplyr::select(decimalLongitude, decimalLatitude)
  
  species$elevation <- terra::extract(dem, terra::vect(pts_species))[,2]
  # Calculate elevation max and min + range size for each species in each mountain range
  species %>%
    group_by(Level_03, sciname) %>%
    filter(n() > 10) %>%
    summarise(
      NumberOcc = n(),
      min_elevation_GBIF = quantile(elevation, 0.05, na.rm = TRUE),
      max_elevation_GBIF = quantile(elevation, 0.95, na.rm = TRUE),
      .groups = "drop"
    ) 
}
