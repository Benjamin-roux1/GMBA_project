# -----------------------------------------------
# Rarefaction (True Range - GBIF) ~ n occurrences

# we will see what's the relationship between the "true range" from Lotta's checklist and the GBIF estimates in function
# of the number of occurrences
# For this, we're using again a rarefaction method

# Run the rarefaction
rarefaction.expert <- function(checklist, replications, parquet_path) {
  
  message("---- Start running the bootstrap! ----")
  
  mountain_names <- checklist %>% distinct(Mountain_range) %>% pull(Mountain_range)
  occ_seq <- c(2, 5, 10, 15, 20, 30, 40, 50, 60, 70, 85, 100, 120, 140, 160, 180, 200)
  low_qu  <- c(0, 1, 2, 3, 4, 5, 10, 15, 20)
  high_qu <- c(100, 99, 98, 97, 96, 95, 90, 85, 80)
  
  results <- furrr::future_map_dfr(mountain_names, function(mountain) {
    
    data_mountain <- arrow::open_dataset(parquet_path) %>%
      filter(Level_03 == mountain) %>%
      collect() %>%
      rename(Mountain_range = Level_03) %>%
      left_join(
        checklist %>%
          filter(Mountain_range == mountain) %>%
          select(sciname, Mountain_range, min_elevation, max_elevation, true_range),
        by = c("sciname", "Mountain_range")
      )
    
    if (nrow(data_mountain) == 0) return(NULL)
    
    # Split once per mountain instead of refiltering per species/n_occ
    species_split <- split(data_mountain, data_mountain$sciname)
    
    map_dfr(occ_seq, function(n_occ) {
      
      eligible_spp <- names(species_split)[
        vapply(species_split, nrow, integer(1)) > n_occ
      ]
      if (length(eligible_spp) == 0) return(NULL)
      
      map_dfr(eligible_spp, function(spp) {
        
        spp_data   <- species_split[[spp]]
        elev_vec   <- spp_data$elevation
        n_total    <- length(elev_vec)
        true_range <- spp_data$true_range[1]
        max_elev_e <- spp_data$max_elevation[1]
        min_elev_e <- spp_data$min_elevation[1]
        
        # Matrix of resampled indices: n_occ rows x replications cols
        idx_mat <- vapply(
          seq_len(replications),
          function(i) sample.int(n_total, n_occ, replace = FALSE),
          integer(n_occ)
        )
        sample_mat <- matrix(elev_vec[idx_mat], nrow = n_occ)
        
        # Vectorized quantiles across all replicates at once, per quantile pair
        map_dfr(seq_along(low_qu), function(q) {
          lo <- matrixStats::colQuantiles(sample_mat, probs = low_qu[q] / 100, na.rm = TRUE)
          hi <- matrixStats::colQuantiles(sample_mat, probs = high_qu[q] / 100, na.rm = TRUE)
          elev_range <- hi - lo
          offset_rng <- elev_range / true_range
          delta_range <- true_range - elev_range
          delta_max <- max_elev_e - hi
          delta_min <- min_elev_e - lo
          
          tibble(
            sciname = spp,
            Mountain_range = mountain,
            subsample_occ = n_occ,
            low_q = low_qu[q],
            high_q = high_qu[q],
            total_occ_spp = n_total,
            mean_elev_range = mean(elev_range), sd_elev_range = sd(elev_range),
            mean_maxelev = mean(hi), sd_maxelev = sd(hi),
            mean_minelev = mean(lo), sd_minelev = sd(lo),
            mean_delta_range = mean(delta_range), sd_delta_range = sd(delta_range),
            mean_delta_max = mean(delta_max), sd_delta_max = sd(delta_max),
            mean_delta_min = mean(delta_min), sd_delta_min = sd(delta_min),
            mean_offset_range = mean(offset_rng), sd_offset_range = sd(offset_rng)
          )
        })
      })
    })
  }, .options = furrr_options(seed = 42))
  
  return(results)
}