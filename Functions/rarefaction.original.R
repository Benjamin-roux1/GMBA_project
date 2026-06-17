
rarefaction.Nsp <- function(data, replications, n_occ) {
  
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
    
    # 2. For each subsampling level:
    map_dfr(occ_seq, function(n) {
      
      # Filter only the species with enough occurrences
      species_names <- data_mountain %>%
        group_by(sciname) %>%
        summarise(total_occ = n(), .groups = "drop") %>%
        filter(total_occ >= n) %>%
        pull(sciname)
      
      if (length(species_names) == 0) return(NULL)
      
      # 3. Replicate 1000 times:
      replicates <- map_dfr(1:replications, function(b) {
      
        # 4. For each species, subsample n occurrences
        map_dfr(species_names, function(spp) {
          
          data_species <- data_mountain %>%
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

rarefaction.Ntot <- function(data, replications, n_occ) {
  
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
  
    occ_seq <- unique(c(
      if (occ_total >= 100)  seq(100,  min(1000, occ_total), by = 100),
      if (occ_total >= 1250) seq(1250, min(3000, occ_total), by = 250),
      if (occ_total >= 3500) seq(3500, min(5000, occ_total), by = 500),
      if (occ_total >= 6000) seq(6000, occ_total,            by = 1000)
    ))
    
    if (length(occ_seq) == 0) return(NULL)
    
    # 2. For each subsampling level:
    map_dfr(occ_seq, function(n) {
      
      if (occ_total < n) return(NULL)
      
      # 3. Replicate `replications` times:
      replicates <- map_dfr(1:replications, function(b) {
        
        # Subsample n occurrences from the resampled pool
        sample_data <- slice_sample(data_mountain, n = n, replace = FALSE)
        
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
