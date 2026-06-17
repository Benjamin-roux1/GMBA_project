# -----------------------------------------------
# We want to assess if the distribution of occurrences has an effect on the range size distribution
# To do this, we reshuffle the distribution of occurrences in different mountain ranges to a model one
# Then we rarefy and look at the rarefaction for the original distribution and the modified one

# --> Here, we rarefy the number of occurrences per species

rarefaction.Nsp.reshuffled <- function(data, model_band, replications, n_occ) {
  
  message("---- Start running the bootstrap! ----")
  
  occ_seq <- c(2, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 125, 150, 175, 200)
  
  # Filter mountains with enough occurrences
  mountain_names <- data %>% 
    group_by(Mountain_range) %>%
    summarise(total_occ = n(), .groups = "drop") %>%
    filter(total_occ > n_occ) %>%
    pull(Mountain_range)
  
  results <- furrr::future_map_dfr(mountain_names, function(mountain) {
    
    data_mountain <- data %>%
      filter(Mountain_range == mountain)
    
    if (nrow(data_mountain) == 0) return(NULL)
    
    # Scale on a 0-1000m elevational domain
    data_mountain <- data_mountain %>%
      mutate(low_lim = min(elevation, na.rm = TRUE),
             high_lim = max(elevation, na.rm = TRUE),
             elev_rel = ((elevation - low_lim) / (high_lim - low_lim)) * 1000) %>%
      ungroup()
    
    # Assign elevation bins
    data_mountain <- data_mountain %>%
      mutate(elev_bin = cut(elev_rel,
                            breaks = seq(0, max(elev_rel, na.rm = TRUE) + 50, by = 50),
                            right = FALSE))
    
    # Compute n available per band
    band_counts <- data_mountain %>%
      group_by(elev_bin) %>%
      summarise(n_occ_bin = n(), .groups = "drop")
    
    # Join with model distribution and compute sampling targets
    sampling_sum <- band_counts %>%
      left_join(model_band, by = "elev_bin") %>%
      mutate(
        n_tot_min = min(n_occ_bin / prop_model, na.rm = TRUE),
        n_bin_tosample = round(prop_model * n_tot_min)
      )
    
    # Add sampling targets to occurrences
    data_mountain <- data_mountain %>%
      left_join(sampling_sum %>% select(elev_bin, n_bin_tosample), by = "elev_bin")
    
    # 2. For each subsampling level:
    map_dfr(occ_seq, function(n) {
      
      # 3. Replicate `replications` times:
      replicates <- map_dfr(1:replications, function(b) {
        
        # Resample elevational distribution for this replicate
        data_resampled <- data_mountain %>%
          group_by(elev_bin) %>%
          group_modify(~ slice_sample(.x, 
                                      n = min(first(.x$n_bin_tosample), nrow(.x)), 
                                      replace = FALSE)) %>%
          ungroup()
        
        species_names <- data_resampled %>%
          group_by(sciname) %>%
          summarise(total_occ = n(), .groups = "drop") %>%
          filter(total_occ >= n) %>%
          pull(sciname)
        
        if (length(species_names) == 0) return(NULL)
        
        # 4. For each species, subsample n occurrences
        map_dfr(species_names, function(spp) {
          
          data_species <- data_resampled %>%
            filter(sciname == spp)
          
          if (nrow(data_species) < n) return(NULL)
          
          slice_sample(data_species, n = n, replace = FALSE) %>%
            summarise(
              sciname = spp,
              elev_range = quantile(elevation, 0.95, na.rm = TRUE) - quantile(elevation, 0.05, na.rm = TRUE),
              maxelev = quantile(elevation, 0.95, na.rm = TRUE),
              minelev = quantile(elevation, 0.05, na.rm = TRUE)
            )
        })
      })
      
      if (nrow(replicates) == 0) return(NULL)
      
      n_sp <- n_distinct(replicates$sciname)
      
      replicates %>%
        group_by(sciname) %>%
        summarise(
          Mountain_range = mountain,
          subsample_occ = n,
          n_species = n_sp,
          mean_elev_range = mean(elev_range, na.rm = TRUE),
          sd_elev_range = sd(elev_range, na.rm = TRUE),
          mean_maxelev = mean(maxelev, na.rm = TRUE),
          sd_maxelev = sd(maxelev, na.rm = TRUE),
          mean_minelev = mean(minelev, na.rm = TRUE),
          sd_minelev = sd(minelev, na.rm = TRUE),
          .groups = "drop"
        )
    })
  }, .options = furrr_options(seed = 42))
  
  return(results)
}


# --> Here, we rarefy the total number of occurrences in the mountain range

rarefaction.Ntot.reshuffled <- function(data, model_band, replications, n_occ) {
  
  message("---- Start running the bootstrap! ----")
  
  # Filter mountains with enough occurrences
  mountain_names <- data %>% 
    group_by(Mountain_range) %>%
    summarise(total_occ = n(), .groups = "drop") %>%
    filter(total_occ > n_occ) %>%
    pull(Mountain_range)
  
  results <- furrr::future_map_dfr(mountain_names, function(mountain) {
    
    data_mountain <- data %>%
      filter(Mountain_range == mountain)
    
    if (nrow(data_mountain) == 0) return(NULL)
    
    occ_total <- nrow(data_mountain)
    
    # Scale on a 0-1000m elevational domain
    data_mountain <- data_mountain %>%
      mutate(low_lim = min(elevation, na.rm = TRUE),
             high_lim = max(elevation, na.rm = TRUE),
             elev_rel = ((elevation - low_lim) / (high_lim - low_lim)) * 1000) %>%
      ungroup()
    
    # Assign elevation bins
    data_mountain <- data_mountain %>%
      mutate(elev_bin = cut(elev_rel,
                            breaks = seq(0, max(elev_rel, na.rm = TRUE) + 50, by = 50),
                            right = FALSE))
    
    # Compute n available per band
    band_counts <- data_mountain %>%
      group_by(elev_bin) %>%
      summarise(n_occ_bin = n(), .groups = "drop")
    
    # Join with model distribution and compute sampling targets
    sampling_sum <- band_counts %>%
      left_join(model_band, by = "elev_bin") %>%
      mutate(
        n_tot_min = min(n_occ_bin / prop_model, na.rm = TRUE),
        n_bin_tosample = round(prop_model * n_tot_min)
      )
    
    # Add sampling targets to occurrences
    data_mountain <- data_mountain %>%
      left_join(sampling_sum %>% select(elev_bin, n_bin_tosample), by = "elev_bin")
    
    # Define adaptive occ_seq based on resampled pool size
    pool_size <- sum(sampling_sum$n_bin_tosample, na.rm = TRUE)
    
    occ_seq <- unique(c(
      if (pool_size >= 100)  seq(100,  min(1000, pool_size), by = 100),
      if (pool_size >= 1250) seq(1250, min(3000, pool_size), by = 250),
      if (pool_size >= 3500) seq(3500, min(5000, pool_size), by = 500),
      if (pool_size >= 6000) seq(6000, pool_size,            by = 1000)
    ))
    
    if (length(occ_seq) == 0) return(NULL)
    
    # 2. For each subsampling level:
    map_dfr(occ_seq, function(n) {
      
      if (pool_size < n) return(NULL)
      
      # 3. Replicate `replications` times:
      replicates <- map_dfr(1:replications, function(b) {
        
        # Resample elevational distribution for this replicate
        data_resampled <- data_mountain %>%
          group_by(elev_bin) %>%
          group_modify(~ slice_sample(.x,
                                      n = min(first(.x$n_bin_tosample), nrow(.x)),
                                      replace = FALSE)) %>%
          ungroup()
        
        # Subsample n occurrences from the resampled pool
        sample_data <- slice_sample(data_resampled, n = n, replace = FALSE)
        
        # Calculate per-species range
        sample_data %>%
          group_by(sciname) %>%
          summarise(
            elev_range = quantile(elevation, 0.95, na.rm = TRUE) - quantile(elevation, 0.05, na.rm = TRUE),
            maxelev = quantile(elevation, 0.95, na.rm = TRUE),
            minelev = quantile(elevation, 0.05, na.rm = TRUE),
            n_occ_species = n(),
            .groups = "drop"
          )
      })
      
      if (nrow(replicates) == 0) return(NULL)
      
      replicates %>%
        group_by(sciname) %>%
        summarise(
          Mountain_range = mountain,
          subsample_occ = n,
          occ_total = occ_total,
          pool_size = pool_size,
          mean_elev_range = mean(elev_range, na.rm = TRUE),
          sd_elev_range = sd(elev_range, na.rm = TRUE),
          mean_maxelev = mean(maxelev, na.rm = TRUE),
          sd_maxelev = sd(maxelev, na.rm = TRUE),
          mean_minelev = mean(minelev, na.rm = TRUE),
          sd_minelev = sd(minelev, na.rm = TRUE),
          mean_occ_species = mean(n_occ_species, na.rm = TRUE),
          sd_occ_species = sd(n_occ_species, na.rm = TRUE),
          .groups = "drop"
        )
    })
  }, .options = furrr_options(seed = 42))
  
  return(results)
}
