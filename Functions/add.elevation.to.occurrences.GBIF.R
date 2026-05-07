
add.elev.to.occ <- function (dataset_path, dem) {
  
# Import and process GBIF dataset
gbif_data <- arrow::open_dataset(dataset_path)

# Collect Parquet dataset to R
gbif_data <- gbif_data %>%
  dplyr::select(species, decimalLatitude, decimalLongitude, Level_01, Level_02,
                Level_03) %>%
  collect()

message(sprintf("Loaded %d occurrences", nrow(gbif_data)))

gbif_data <- gbif_data %>%
  rename(sciname = "species")

# Fill empty Level_03 by the Level_02 or Level_01
gbif_data <- gbif_data %>%
  mutate(
    Level_03 = coalesce(Level_03, Level_02, Level_01),
    Level_02 = coalesce(Level_02, Level_01))

# Select only occurrences coordinates
pts_GBIF <- gbif_data %>%
  dplyr::select(decimalLongitude, decimalLatitude)

message("Extracting elevation...")
# Add elevation
gbif_data$elevation <- terra::extract(dem, terra::vect(pts_GBIF, crs = "EPSG:4326"))[, 2]

return(gbif_data)
}
