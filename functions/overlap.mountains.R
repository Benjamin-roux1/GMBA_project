# Function to intersect species ranges with mountain ranges and calculate overlap (value in km2 and %) 

# The function overlap.mountain:
# 1. Intersect species_df and all mountains --> return a list of the mountains that each species touch
#   2. Process by species:
#   3. calculate the area of the species in km2 
#     4. For each mountain ranges it actually touches:
#       5. calculate the percentage of overlap of the species range with the mountain range 
#       6. removes all species with < 5km2 and < 1% overlap with a GMBA Mountain range

# We chose these threshold to avoid excluding false negative, i.e. be as much inclusive as possible. 
# With the 5km2, we make sure to select even small ranges species, common in mountain areas, and for very small ranges
# species, i.e. < 5km2, we set a threshold at 1% to make sure to include them as well.

overlap.mountain <- function(mountain_shapes, species_df) {
  
  # Add the column if missing. Little trick to homogeneize all dataframes from different taxas.
  if (!"genus" %in% names(species_df)) {
    species_df$genus <- NA_character_
  }
  
  if (!"family" %in% names(species_df)) {
    species_df$family <- NA_character_
  }
  
  if (!"order" %in% names(species_df)) {
    species_df$order <- NA_character_
  }
  
  if (!"seasonal" %in% names(species_df)) {
    species_df$seasonal <- NA_character_
  }
  
  sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(TRUE))
  
  # reproject to Equal Earth for accurate planar calculations
  mountain_shapes <- sf::st_transform(mountain_shapes, 8857)
  species_df <- sf::st_transform(species_df, 8857)
  
  # fix invalid geometries first
  mountain_shapes <- sf::st_make_valid(mountain_shapes)
  species_df <- sf::st_make_valid(species_df)
  
  # 1. Vectorized intersection — builds a sparse matrix of which species overlaps which mountain
  message(sprintf("Running spatial index on %d species x %d mountain ranges...", 
                  nrow(species_df), nrow(mountain_shapes)))
  hits <- sf::st_intersects(species_df, mountain_shapes)
  
  n_pairs <- sum(lengths(hits))
  message(sprintf("Found %d species-mountain pairs to process", n_pairs))
  
  results <- list()
  failures <- list()
  
  # 2. Process by species: select the mountain ranges that it touches
  for (j in seq_len(nrow(species_df))) {
    matched_mountains <- hits[[j]]
    if (length(matched_mountains) == 0) next
    
    species <- species_df[j, ]
    species_area <- as.numeric(sf::st_area(species)) / 10^6  # calculate species area
    
    message(sprintf("Processing species %d/%d: %s (%d mountain matches)", 
                    j, nrow(species_df), species$sciname, length(matched_mountains)))
    
    for (i in matched_mountains) {
      mountain <- mountain_shapes[i, ]
      
      intersection <- tryCatch({
        sf::st_intersection(species, mountain)
      }, error = function(e) {
        failures[[length(failures) + 1]] <<- data.frame(
          sciname = species$sciname,
          seasonal = species$seasonal,
          Mountain_system = mountain$Level_02,
          Mountain_range = mountain$Level_03,
          stage = "intersection",
          error = e$message
        )
        return(NULL)
      })
      
      if (is.null(intersection) || nrow(intersection) == 0) next
      
      overlap_area <- as.numeric(sf::st_area(intersection)) / 10^6
      if (length(overlap_area) == 0 || is.na(overlap_area)) next
      
      # Also check species_area is valid
      if (length(species_area) == 0 || is.na(species_area) || species_area == 0) next
      
      overlap_pct <- round((overlap_area / species_area) * 100, 4)
      if (length(overlap_pct) == 0 || is.na(overlap_pct)) next
      if (overlap_area < 5 & overlap_pct < 1) next
      
      
      results[[length(results) + 1]] <- data.frame(
        sciname = species$sciname,
        seasonal = species$seasonal,
        genus = species$genus,
        family = species$family,
        order = species$order,
        Mountain_system = mountain$Level_02,
        Mountain_range = mountain$Level_03,
        overlap_area = round(overlap_area, 2),
        overlap_pct = overlap_pct,
        species_area = species_area
      )
    }
  }
  
  results_df  <- if (length(results) > 0) do.call(rbind, results) else NULL
  failures_df <- if (length(failures) > 0) do.call(rbind, failures) else NULL
  
  message(sprintf(
    "Done! %d records | %d failures",
    ifelse(is.null(results_df), 0, nrow(results_df)),
    ifelse(is.null(failures_df), 0, nrow(failures_df))
  ))
  
  return(list(results_df = results_df, failures_df = failures_df))
}
