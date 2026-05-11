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
# we will estimate the link between the number of occurrences and the range size 
# we first do it at the mountain range level, and we select only the mountain ranges that have at least 10,000 occ 
# we run a rarefaction method with bootstrapping and calculate the range size

# We will process by mountain range, from the parquet files with DEM elevation 

# --------------------
# Boostrapping 

# Bootstrap function
message("---- Start running the bootstrap! ----")

bootstrap <- function(data, replications, n_occ, source_path) {
  
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
    
    occ_total <-  nrow(data_mountain)
    
    if (nrow(data_mountain) == 0) return(NULL)
    
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
      
      if (nrow(data_mountain) < n) return(NULL)
      
      # 3. We replicate 1,000 times the following:
      replicates <- map_dfr(1:replications, function(b) {
        
        # We subsample n occurrences corresponding to the number of occurrences in occ_seq
        sample_data <- slice_sample(data_mountain, n = n, replace = TRUE)
        
        # We calculate per-species range and elevational limits
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
      
      # We summarise the output for the 1,000 replicates for this defined number of occurrences chosen
      replicates %>%
        group_by(sciname) %>%
        summarise(
          Mountain_range = mountain,
          subsample_occ  = n,
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

# --------------
# Run bootstrap
library(furrr)
plan(multisession, workers = 10)

# we set a 1000 replications for each subsample, and we set the threshold of number of occurrences at 1, to keep all mountains
bootstrap_results <- bootstrap(reptiles_GBIF, replications = 1000, n_occ = 1, source_path = source_path)
plan(sequential)

write.csv(bootstrap_results,
          file = paste0(source_path, "GMBA_project/Outputs/bootstrap_results.csv"),
          row.names = FALSE)