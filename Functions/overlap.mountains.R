# Function to intersect species ranges with mountain ranges and calculate overlap (value in km2 and %) 

# The function overlap.mountain:
# 1. Process by mountain range
#   2. creates a bbox around the mountain range
#   3. select the species that cross this bbox (first coarse filter)
#     4. Process by species (for each species crossing the mountain range) 
#     5. calculate the area of the species in km2 
#     6. calculate the percentage of overlap of the species range with the mountain range 
#   7. removes all species with < 5km2 and < 1% overlap with a GMBA Mountain range

# We chose these threshold to avoid excluding false negative, i.e. be as much inclusive as possible. 
# With the 5km2, we make sure to select even small ranges species, common in mountain areas, and for very small ranges
# species, i.e. < 5km2, we set a threshold at 1% to make sure to include them as well.

overlap.mountain <- function(mountain_shapes, species_df) {
  
  # Add genus column if missing. Little trick because reptiles doesnt have a genus column.
  if (!"genus" %in% names(species_df)) {
    species_df$genus <- NA_character_
  }
  
  sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(TRUE))
  
  # reproject to Equal Earth for accurate planar calculations
  mountain_shapes <- sf::st_transform(mountain_shapes, 8857)
  species_df <- sf::st_transform(species_df, 8857)
  
  # fix invalid geometries first
  mountain_shapes <- sf::st_make_valid(mountain_shapes)
  species_df <- sf::st_make_valid(species_df)
  
  results <- list()
  failures <- list()
  
  total_mountains <- nrow(mountain_shapes)
  
  # 1. process by mountain range
  for (i in seq_len(total_mountains)) {
    
    mountain <- mountain_shapes[i, ]
    message(sprintf("Processing mountain range %d/%d: %s", i, total_mountains, mountain$Level_03))
    
    # 2. create a bbox around each MR
    bbox_coords <- sf::st_bbox(mountain)
    bbox <- sf::st_polygon(list(cbind(
      c(bbox_coords["xmin"], bbox_coords["xmin"], bbox_coords["xmax"], bbox_coords["xmax"], bbox_coords["xmin"]),
      c(bbox_coords["ymin"], bbox_coords["ymax"], bbox_coords["ymax"], bbox_coords["ymin"], bbox_coords["ymin"])
    )))
    bbox_sf <- sf::st_sfc(bbox, crs = sf::st_crs(mountain_shapes))
    
    n_overlapping <- 0
    
    # 3. for each species of species_df, check if it overlap
    for (j in seq_len(nrow(species_df))) {
      
      species <- species_df[j, ]
      
      # quick bbox check first
      intersects_bbox <- tryCatch({
        length(sf::st_intersects(bbox_sf, species)[[1]]) > 0
      }, error = function(e) {
        failures[[length(failures) + 1]] <- data.frame(
          sciname = species$sciname,
          Mountain_system = mountain$Level_02,
          Mountain_range = mountain$Level_03,
          stage = "bbox_check",
          error = e$message
        )
        return(FALSE)
      })
      
      if (!intersects_bbox) next
      
      # 4. calculate overlap on actual geometry
      intersection <- tryCatch({
        sf::st_intersection(species, mountain)
      }, error = function(e) {
        failures[[length(failures) + 1]] <- data.frame(
          sciname = species$sciname,
          Mountain_system = mountain$Level_02,
          Mountain_range = mountain$Level_03,
          stage = "intersection",
          error = e$message
        )
        return(NULL)
      })
      
      if (is.null(intersection) || nrow(intersection) == 0) next
      
      overlap_area <- tryCatch({
        as.numeric(sf::st_area(intersection)) / 10^6
      }, error = function(e) {
        failures[[length(failures) + 1]] <- data.frame(
          sciname = species$sciname,
          Mountain_system = mountain$Level_02,
          Mountain_range = mountain$Level_03,
          stage = "area_calc",
          error = e$message
        )
        return(NA)
      })
      
      if (is.na(overlap_area)) next
      
      species_area <- as.numeric(sf::st_area(species)) / 10^6
      overlap_pct <- tryCatch({
        round((overlap_area / species_area) * 100, 4)
      }, error = function(e) {
        failures[[length(failures) + 1]] <- data.frame(
          sciname = species$sciname,
          Mountain_system = mountain$Level_02,
          Mountain_range = mountain$Level_03,
          stage = "pct_calc",
          error = e$message
        )
        return(NA)
      })
      
      if (length(overlap_pct) == 0 || is.na(overlap_pct)) next
      
      # 5. retain only species above threshold
      if (overlap_area < 5 & overlap_pct < 1) next
      
      n_overlapping <- n_overlapping + 1
      
      # 6. store one row per species per MR
      results[[length(results) + 1]] <- data.frame(
        sciname = species$sciname,
        genus = species$genus,
        family = species$family,
        Mountain_system = mountain$Level_02,
        Mountain_range = mountain$Level_03,
        overlap_area = round(overlap_area, 2),
        overlap_pct = overlap_pct,
        species_area = species_area
      )
    }
    
    message(sprintf("  → %d species retained for %s", n_overlapping, mountain$Level_03))
  }
  
  results_df  <- if (length(results) > 0) do.call(rbind, results) else NULL
  failures_df <- if (length(failures) > 0) do.call(rbind, failures) else NULL
  
  message(sprintf(
    "Done! %d records | %d failures across %d mountain ranges",
    ifelse(is.null(results_df), 0, nrow(results_df)),
    ifelse(is.null(failures_df), 0, nrow(failures_df)),
    total_mountains
  ))
  
  return(list(
    results_df  = results_df,
    failures_df = failures_df
  ))
}
