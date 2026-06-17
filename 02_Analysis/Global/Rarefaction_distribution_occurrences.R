##-----------------------
#  1.1. Set up -----
##-----------------------
message("---- Set up! ----")

library(here); library(data.table); library(dplyr)
library(tidyverse); library(readxl); library(terra)
library(sf); library(arrow); library(rgbif); library(writexl)
library(exactextractr)

# define data path OR even config.R file with libraries & path
source_path <- "/mnt/users/berou1714/PhD_project/"
#source_path <- "C:/Users/berou1714/OneDrive - Norwegian University of Life Sciences/Desktop/PhD_project/"

reptiles <- read_xlsx(paste0(source_path, "GMBA_project/files_processed/Reptiles/reptiles_dataframe.xlsx"))

reptiles_GBIF <- reptiles %>%
  filter(!is.na(NumberOcc))

# ---------------------------
# we estimate the link between the number of occurrences and its distribution along elevation
# we run a rarefaction method with bootstrapping and look at the occurrence distribution

# We will process by mountain range, from the parquet files with DEM elevation 

# Here, we run a bootstrapping based on the number of occurrences per species
# we rarefy each species according to a defined number of occurrences

# --------------------
# Boostrapping 

# Bootstrap function
message("---- Start running the bootstrap! ----")

bootstrap <- function(data, replications, n_occ, source_path, bin_width) {
  
  # Filter only the mountain ranges that have > n_occ, our defined threshold of number of occurrences
  mountain_names <- data %>% 
    group_by(Mountain_range) %>%
    summarise(total_occ = sum(NumberOcc), .groups = "drop") %>%
    filter(total_occ > n_occ) %>%
    pull(Mountain_range)
  
  # Fixed subsampling sequence (occurrences per species)
  occ_seq <- c(2, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 125, 150, 175, 200)
  
  # 1. Run the function per mountain range:
  results <- furrr::future_map_dfr(mountain_names, function(mountain) {
    
    # Extract all occurrences for the mountain range from GBIF parquet files 
    data_mountain <- arrow::open_dataset(paste0(source_path, "GBIF_data/processed_files/reptiles_gbif_parquet")) %>%
      filter(Level_03 == mountain) %>%
      collect()
    
    occ_total <- nrow(data_mountain)
    
    if (occ_total == 0) return(NULL)
    
    # Define elevation bins from the data
    elev_min <- floor(min(data_mountain$elevation, na.rm = TRUE) / bin_width) * bin_width
    elev_max <- ceiling(max(data_mountain$elevation, na.rm = TRUE) / bin_width) * bin_width
    breaks <- seq(elev_min, elev_max, by = bin_width)
    bin_mids <- breaks[-length(breaks)] + bin_width / 2
    
    # 2. For each number of occurrences in the sequence, we run the following:
    map_dfr(occ_seq, function(n) {
      
      # Keep only species with at least n occurrences
      eligible_species <- data_mountain %>%
        group_by(sciname) %>%
        filter(n() >= n) %>%
        ungroup()
      
      n_species <- n_distinct(eligible_species$sciname)
      
      if (n_species == 0) return(NULL)
      
      # 3. 1,000 replicates: count occurrences per elevation bin (pooled across species)
      bin_counts <- map(1:replications, function(b) {
        
        # Subsample exactly n occurrences per eligible species, then pool
        sample_data <- eligible_species %>%
          group_by(sciname) %>%
          slice_sample(n = n, replace = FALSE) %>%
          ungroup()
        
        counts <- cut(sample_data$elevation, breaks = breaks, include.lowest = TRUE, right = FALSE) %>%
          table() %>%
          as.numeric()
        
        counts / nrow(sample_data)  # proportion (density) rather than raw counts
      })
      
      # Stack into matrix (replicates x bins) and summarise
      count_matrix <- do.call(rbind, bin_counts)
      
      tibble(
        Mountain_range = mountain,
        subsample_occ  = n,          # n occ per species
        n_species      = n_species,  # number of eligible species at this n
        occ_total      = occ_total,
        elev_bin_mid   = bin_mids,
        mean_density   = colMeans(count_matrix, na.rm = TRUE),
        sd_density     = apply(count_matrix, 2, sd, na.rm = TRUE)
      )
    })
    
  }, .options = furrr_options(seed = 42))
  
  return(results)
}



# ---------------------------
# we estimate the link between the number of occurrences and its distribution along elevation
# we run a rarefaction method with bootstrapping and look at the occurrence distribution

# We will process by mountain range, from the parquet files with DEM elevation 

# Here, we run a bootstrapping based on the total number of occurrences in the mountain range
# we subsample and look at the distribution of occurrences

# --------------------
# Boostrapping 

# Bootstrap function
message("---- Start running the bootstrap! ----")

bootstrap <- function(data, replications, n_occ, source_path, bin_width) {
  
  # Filter only the mountain ranges that have > n_occ, our defined threshold of number of occurrences
  mountain_names <- data %>% 
    group_by(Mountain_range) %>%
    summarise(total_occ = sum(NumberOcc), .groups = "drop") %>%
    filter(total_occ > n_occ) %>%
    pull(Mountain_range)
  
  # 1. Run the function per mountain range:
  results <- furrr::future_map_dfr(mountain_names, function(mountain) {
    
    # Extract all occurrences for the mountain range from GBIF parquet files 
    data_mountain <- arrow::open_dataset(paste0(source_path, "GBIF_data/processed_files/reptiles_gbif_parquet")) %>%
      filter(Level_03 == mountain) %>%
      collect()
    
    occ_total <- nrow(data_mountain)
    
    if (occ_total == 0) return(NULL)
    
    # Define elevation bins from the data
    elev_min <- floor(min(data_mountain$elevation, na.rm = TRUE) / bin_width) * bin_width
    elev_max <- ceiling(max(data_mountain$elevation, na.rm = TRUE) / bin_width) * bin_width
    breaks <- seq(elev_min, elev_max, by = bin_width)
    bin_mids <- breaks[-length(breaks)] + bin_width / 2
    
    # Define the sequence of subsampling
    occ_seq <- c(
      if (nrow(data_mountain) >= 100)
        seq(100, min(1000, nrow(data_mountain)), by = 100),
      
      if (nrow(data_mountain) >= 1250)
        seq(1250, min(3000, nrow(data_mountain)), by = 250),
      
      if (nrow(data_mountain) >= 3500)
        seq(3500, min(5000, nrow(data_mountain)), by = 500),
      
      if (nrow(data_mountain) >= 6000)
        seq(6000, nrow(data_mountain), by = 1000)
    )
    
    occ_seq <- unique(occ_seq)
    
    # 2. For each number of occurrences in the sequence, we run the following:
    map_dfr(occ_seq, function(n) {
      
      if (occ_total < n) return(NULL)
      
      # 3. 1,000 replicates: count occurrences per elevation bin (pooled across species)
      bin_counts <- map(1:replications, function(b) {
        sample_data <- slice_sample(data_mountain, n = n, replace = FALSE)
        
        counts <- cut(sample_data$elevation, breaks = breaks, include.lowest = TRUE, right = FALSE) %>%
          table() %>%
          as.numeric()
        
        counts / n  # proportion (density) rather than raw counts
      })
      
      # Stack into matrix (replicates x bins) and summarise
      count_matrix <- do.call(rbind, bin_counts)
      
      tibble(
        Mountain_range = mountain,
        subsample_occ = n,
        occ_total = occ_total,
        elev_bin_mid = bin_mids,
        mean_density = colMeans(count_matrix, na.rm = TRUE),
        sd_density= apply(count_matrix, 2, sd, na.rm = TRUE)
      )
    })
    
  }, .options = furrr_options(seed = 42))
  
  return(results)
}

# --------------
# Run bootstrap
library(furrr)
plan(multisession, workers = 10)

# we set a 1000 replications for each subsample, and we set the threshold of number of occurrences at 1, to keep all mountains
bootstrap_results <- bootstrap(reptiles_GBIF, replications = 1000, n_occ = 1, source_path = source_path, bin_width = 100)
plan(sequential)
