# -----------------------------------------------
# Rarefaction (True Range - GBIF) ~ n occurrences

# we will see what's the relationship between the "true range" from Lotta's checklist and the GBIF estimates in function
# of the number of occurrences
# For this, we're using again a rarefaction method

# Run the rarefaction
rarefaction.expert <- function(checklist, replications, parquet_path) {
  
  message("---- Start running the bootstrap! ----")
  
  # Pull a mountain list
  mountain_names <- checklist %>% 
    distinct(Mountain_range) %>%
    pull(Mountain_range)
  
  # Define the sequence of subsampling
  occ_seq <- c(2, 5, 10, 15, 20, 30, 40, 50, 60, 70, 85, 100, 120, 140, 160, 180, 200)
  
  # Define sequence of low/high quantiles
  low_qu <- c(0, 1, 2, 3, 4, 5, 10, 15, 20)
  high_qu <- c(100, 99, 98, 97, 96, 95, 90, 85, 80)
  
  # 1. Run the function per mountain range:
  results <- furrr::future_map_dfr(mountain_names, function(mountain) {
    
    # Extract all occurrences for the mountain range from GBIF parquet files 
    data_mountain <- arrow::open_dataset(parquet_path) %>%
      filter(Level_03 == mountain) %>%
      collect() %>%
      rename(Mountain_range = Level_03) %>%
      left_join(checklist %>% 
                  filter(Mountain_range == mountain) %>%
                  select(sciname, Mountain_range, min_elevation, max_elevation, true_range),
                by = c("sciname", "Mountain_range"))
    
    if (nrow(data_mountain) == 0) return(NULL)
    
    # 2. For each number of occurrences in the sequence, we run the following:
    map_dfr(occ_seq, function(n_occ) {
      
      # Filter only the species that have > n_occ, our defined threshold of number of occurrences
      species_occ <- data_mountain %>%
        group_by(sciname) %>%
        summarise(total_occ = n(), .groups = "drop") %>%
        filter(total_occ > n_occ)
      
      species_names <- species_occ %>% pull(sciname)
      
      if (length(species_names) == 0) return(NULL)
      
      # 3. We replicate 1,000 times the following:
      replicates <- map_dfr(1:replications, function(b) {
        
        # 4. For each species in this mountain range
        map_dfr(species_names, function(spp) {
          
          data_species <- data_mountain %>%
            filter(sciname == spp)
          
          # We subsample n occurrences corresponding to the number of occurrences in occ_seq
          sample_data <- slice_sample(data_species, n = n_occ, replace = FALSE)
          
          # 5. For each quantile pair:
          map_dfr(seq_along(low_qu), function(q) {
            sample_data %>%
              summarise(
                sciname = spp,
                replicate = b,
                low_q = low_qu[q],
                high_q = high_qu[q],
                elev_range = quantile(elevation, high_qu[q]/100, na.rm = TRUE) - quantile(elevation, low_qu[q]/100, na.rm = TRUE),
                maxelev = quantile(elevation, high_qu[q]/100, na.rm = TRUE),
                minelev = quantile(elevation, low_qu[q]/100, na.rm = TRUE),
                delta_range = first(true_range) - elev_range,
                delta_max = first(max_elevation) - maxelev,
                delta_min = first(min_elevation) - minelev,
                offset_range = elev_range / first(true_range)
              )
          })
        })
      })
      
      # We summarise the output for the 1,000 replicates for this defined number of occurrences chosen
      replicates %>%
        left_join(species_occ %>% select(sciname, total_occ), by = "sciname") %>%
        group_by(sciname, low_q, high_q) %>%
        summarise(
          Mountain_range = mountain,
          subsample_occ = n_occ,
          total_occ_spp = first(total_occ),
          mean_elev_range = mean(elev_range, na.rm = TRUE),
          sd_elev_range = sd(elev_range, na.rm = TRUE),
          mean_maxelev = mean(maxelev, na.rm = TRUE),
          sd_maxelev = sd(maxelev, na.rm = TRUE),
          mean_minelev = mean(minelev, na.rm = TRUE),
          sd_minelev = sd(minelev, na.rm = TRUE),
          mean_delta_range = mean(delta_range, na.rm = TRUE),
          sd_delta_range = sd(delta_range, na.rm = TRUE),
          mean_delta_max = mean(delta_max, na.rm = TRUE),
          sd_delta_max = sd(delta_max, na.rm = TRUE),
          mean_delta_min = mean(delta_min, na.rm = TRUE),
          sd_delta_min = sd(delta_min, na.rm = TRUE),
          mean_offset_range = mean(offset_range, na.rm = TRUE),
          sd_offset_range = sd(offset_range, na.rm = TRUE),
          .groups = "drop"
        )
    })
  }, .options = furrr_options(seed = 42))
  
  return(results)
}
