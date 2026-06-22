
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
    
    data_mountain <- data %>% filter(Mountain_range == mountain)
    if (nrow(data_mountain) == 0) return(NULL)
    
    species_split <- split(data_mountain, data_mountain$sciname)
    # Total occurrences per species, computed once per mountain
    species_totals <- vapply(species_split, nrow, integer(1))
    
    # 2. For each subsampling level:
    map_dfr(occ_seq, function(n) {
      
      # Filter only the species with enough occurrences
      eligible_spp <- names(species_totals)[species_totals >= n]
      if (length(eligible_spp) == 0) return(NULL)
      
      # Per-species: matrix-sample, vectorized 5-95 quantiles across all replicates
      per_species <- map_dfr(eligible_spp, function(spp) {
        
        elev_vec <- species_split[[spp]]$elevation
        n_total  <- length(elev_vec)
        
        idx_mat <- vapply(
          seq_len(replications),
          function(i) sample.int(n_total, n, replace = FALSE),
          integer(n)
        )
        sample_mat <- matrix(elev_vec[idx_mat], nrow = n)
        
        lo <- matrixStats::colQuantiles(sample_mat, probs = 0.05, na.rm = TRUE)
        hi <- matrixStats::colQuantiles(sample_mat, probs = 0.95, na.rm = TRUE)
        
        tibble(
          sciname = spp,
          elev_range = hi - lo,
          maxelev = hi,
          minelev = lo
        )
      })
      
      # Number of distinct species selected at least once across the 1000 replicates
      # (i.e. species with at least one successful replicate at this n)
      n_sp <- length(eligible_spp)
      
      per_species %>%
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
  
  mountain_names <- data %>%
    group_by(Mountain_range) %>%
    summarise(total_occ = n(), .groups = "drop") %>%
    filter(total_occ > n_occ) %>%
    pull(Mountain_range)
  
  results <- furrr::future_map_dfr(mountain_names, function(mountain) {
    
    data_mountain <- data %>% filter(Mountain_range == mountain)
    if (nrow(data_mountain) == 0) return(NULL)
    
    occ_total <- nrow(data_mountain)
    elev_vec  <- data_mountain$elevation
    spp_vec   <- data_mountain$sciname
    
    occ_seq <- unique(c(
      if (occ_total >= 100)  seq(100,  min(1000, occ_total), by = 100),
      if (occ_total >= 1250) seq(1250, min(3000, occ_total), by = 250),
      if (occ_total >= 3500) seq(3500, min(5000, occ_total), by = 500),
      if (occ_total >= 6000) seq(6000, occ_total,            by = 1000)
    ))
    if (length(occ_seq) == 0) return(NULL)
    
    map_dfr(occ_seq, function(n) {
      
      if (occ_total < n) return(NULL)
      
      # One replicate = draw n row-indices from the mountain pool, then
      # compute per-species quantiles within that draw via data.table.
      replicates <- map_dfr(seq_len(replications), function(b) {
        
        idx <- sample.int(occ_total, n, replace = FALSE)
        
        dt <- data.table::data.table(sciname = spp_vec[idx], elevation = elev_vec[idx])
        
        dt[, .(
          elev_range    = quantile(elevation, 0.95, na.rm = TRUE) - quantile(elevation, 0.05, na.rm = TRUE),
          maxelev       = quantile(elevation, 0.95, na.rm = TRUE),
          minelev       = quantile(elevation, 0.05, na.rm = TRUE),
          n_occ_species = .N
        ), by = sciname]
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