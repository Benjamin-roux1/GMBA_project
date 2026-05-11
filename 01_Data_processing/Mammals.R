##------------------------------------------------------
#----- 1. Source Mammal distribution Data from MDD
##------------------------------------------------------

# This snippet unpacks and opens zip folders containing gpkg for all mammals 
# zips can be downloaded via the Mammal Diversity Database: https://www.mammaldiversity.org/assets/data/MDD.zip

##---------------------
# 1.1 Set up  -----
##---------------------
library(here); library(data.table); library(dplyr)
library(tidyverse); library(readxl); library(terra)
library(sf); library(arrow); library(rgbif); library(writexl)
library(exactextractr)

# Load configuration
#source(
#here::here("R/00_Config_file.R")
#)

# define data path OR even config.R file with libraries & path
source_path <- "/mnt/users/berou1714/PhD_project/"
#source_path <- "C:/Users/berou1714/OneDrive - Norwegian University of Life Sciences/Desktop/PhD_project/"

# source all functions
list.files(path = paste0(source_path, "GMBA_project/Functions"), pattern = "*.R", full.names = TRUE) %>%
  purrr::walk(source)

##--------------------------------------------------------
# 1.2 Unzip folder containing range shapefiles  -----
##--------------------------------------------------------
message("1.2. Unzip folder containing range shapefiles")

# First, let's have a look at the organization of the folders and dataframe

# Define the folder containing the zip file(s)
zip_folder <- paste0(source_path, "GMBA_project/Raw_datasets/Mammals/MDD_Mammalia/")
# Find the zip file in the folder
#zip_file <- list.files(zip_folder, pattern = "\\.zip$", full.names = TRUE)[1]
# Peek inside the zip
#zip_content <- unzip(zip_file, list = TRUE)
# Find the .gpkg file inside
#gpkg_file <- zip_content$Name[grepl("\\.gpkg$", zip_content$Name)][1]
# Extract and read it
#unzip(zip_file, files = gpkg_file, exdir = tempdir())
#zip_test <- sf::st_read(file.path(tempdir(), gpkg_file), quiet = TRUE)

# These files are heavy. Every time we will need the geometry (crop, DEM), we will loop over the orders.

##----------------------------------------------------------
#  ----- 2. Overlap Mammal ranges with GMBA shapefile
##----------------------------------------------------------

# This script overlaps mammal distribution ranges with GMBA mountain ranges (level 03)
# in the end, we have a dataframe with one row per species per mountain range and the % of overlap 

# ❗The species range shps are partly very large files. Therefore, we process each order separately

##------------------------------------------
# 2.1 Source & clean gmba mountain   -----
##------------------------------------------
message("2.1. Source gmba mountains")

#source the gmba regions
mountain_shapes <- sf::st_read(paste0(source_path, "GMBA_project/GMBA_mountains/GMBA_Inventory_v2.0_standard_300/GMBA_Inventory_v2.0_standard_300.shp")) %>%
  st_make_valid()

# Group by Level_03 (scale of mountain system chosen)
{
  sf_use_s2(FALSE)
  mountain_shapes03 <- mountain_shapes %>%
    st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
    st_make_valid() %>%
    group_by(Level_01, Level_02, Level_03) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    st_make_valid()
  sf_use_s2(TRUE)
  }
# so we have 137 distinct combinations level_02 - level_03, i.e 137 mountain systems

# check correct mapping of the mountain systems
ggplot() +
  geom_sf(data = mountain_shapes03, fill = "grey30", color = NA) +
  theme_minimal()

# A bit of cleaning the mountain dataframe
# some rows are not defined at Level03 or Level02
# in these cases, we fill the NA with the closest filled upper level
{
  sf_use_s2(FALSE)
  mountain_shapes03 <- mountain_shapes03 %>%
    st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
    st_make_valid() %>%
    mutate(
      Level_03 = coalesce(Level_03, Level_02, Level_01),
      Level_02 = coalesce(Level_02, Level_01)
    ) %>%
    st_make_valid()
  sf_use_s2(TRUE)
  }

rm(mountain_shapes)
gc()

##--------------------------------------------------------------------------
# 2.2. Intersect species ranges with GMBA and calculate % of overlap -----
##--------------------------------------------------------------------------

message("2.2. Intersect species ranges with GMBA and calculate overlap (value in km2 and %) ")

# We are going to loop over each order separately, instead of loading them all on R
# So for each order, which correspond to a zipped file, we first unzip it and then process
# In each zip file, we also process by chunk because it is otherwise extremely slow

# Source to the folder with all the zipped files and make it a list of files
zip_folder <- paste0(source_path, "GMBA_project/Raw_datasets/Mammals/MDD_Mammalia/")
zip_files <- list.files(zip_folder, pattern = "\\.zip$", full.names = TRUE)

all_results <- list()   # store results from all orders
all_failures <- list()  # store failures from all orders

mytempdir <- "/mnt/SCRATCH/berou1714/tempdir/"

chunk_size <- 25   # We process by chunks of 50 species

for (zip_file in zip_files) {     # HERE START THE LOOP
  
  # extract the name of the order from the file name
  order_name <- sub("MDD_", "", tools::file_path_sans_ext(basename(zip_file)))
  message("Processing order: ", order_name)
  
  # Unzip the file and check if there is a gpkg file, i.e. what we are looking for
  zip_contents <- unzip(zip_file, list = TRUE)
  gpkg_file <- zip_contents$Name[grepl("\\.gpkg$", zip_contents$Name)][1]
  
  # If there is one, we unzip it in the tempdir() and then load it into R Studio
  if (!is.na(gpkg_file)) {
    unzip(zip_file, files = gpkg_file, exdir = mytempdir)
    gpkg_path <- file.path(mytempdir, gpkg_file)
    layer_name <- sf::st_layers(gpkg_path)$name[1]
    
    message("File unzipped!")
    
    # Get total species count without loading geometries
    total_species <- sf::st_read(gpkg_path,
                                 query = paste0("SELECT COUNT(*) as n FROM ", layer_name),
                                 quiet = TRUE)$n
    message(sprintf("Total species: %d for %s", total_species, order_name))
    
    # We process by chunks of 50 species
    chunk_results <- list()
    chunk_failures <- list()
    offsets <- seq(0, total_species, by = chunk_size)   # Generate sequences of numbers 50 to 50 (0, 50, 100, ...)
    
    for (offset in offsets) {
      if (offset >= total_species) next  # skip if no rows left
      
      message(sprintf("Reading chunk offset %d/%d", offset, total_species))
      
      chunk <- sf::st_read(gpkg_path,
                           query = paste0("SELECT sciname, \"order\", family, geom 
                                        FROM ", layer_name, "
                                        LIMIT ", chunk_size, " OFFSET ", offset),   # Meaning: extract 50 rows starting from 'offset'
                           quiet = TRUE) %>%
        st_make_valid() %>%
        st_transform(8857) %>%          # reproject to Equal Earth (meters)
        st_simplify(dTolerance = 1000) %>%  # simplify to 10km
        st_transform(4326) %>%          # reproject back to WGS84
        st_make_valid()                 # fix any issues after simplification
      
      results <- overlap.mountain(mountain_shapes03, chunk)
      
      if (!is.null(results$results_df)) {
        chunk_results[[as.character(offset)]] <- results$results_df
      }
      if (!is.null(results$failures_df)) {
        chunk_failures[[as.character(offset)]] <- results$failures_df
      }
      
      rm(chunk, results)
      gc()
      
    }
    
    if (length(chunk_results) > 0) {
      all_results[[order_name]] <- dplyr::bind_rows(chunk_results)
    }
    if (length(chunk_failures) > 0) {
      all_failures[[order_name]] <- dplyr::bind_rows(chunk_failures)
    }
    
    message(sprintf("%s done!", order_name))
  }
}    # HERE END THE LOOP

# Combine all orders into one dataframe at the end
mammals_dataframe <- dplyr::bind_rows(all_results)
mammals_failures  <- dplyr::bind_rows(all_failures)

rm(all_results, all_failures)
gc()

##-------------------------------------------------------------
#  ----- 3. Clean the data from Handbook of Mammals 
##-------------------------------------------------------------
message("3. Clean the data from Handbook of Mammals ")

# Physical copies of Handbook of the Mammals of the World available at
# https://github.com/jhpoelen/hmw

# This script cleans textual information from the handbook of mammals to min and max elevational ranges for mammals. 

# ❗ ATTENTION !! the functions below do not clean HMW completely. there are still elevationa data that can not be grasped by the functions

##------------------------------
# 3.1. Download the data -----
##------------------------------
message("3.1. Download the data")

# This is the single files combined
url <- "https://raw.githubusercontent.com/jhpoelen/hmw/main/hmw.csv"
hmw_data <- read.csv(url)

##----------------------------------
# 3.2. Filter and clean HMW -----
##----------------------------------
message("3.2. Filter and clean HMW")

hmw_data <- hmw_data %>%
  rename(sciname = "name")

# Keep only species in HMW that we have in our checklist
hmw_matched <- hmw_data %>%
  semi_join(mammals_dataframe, by = "sciname")

# clean the dataframe hmw
hmw_clean <- hmw_matched %>%
  select(sciname, habitat) %>%
  distinct(sciname,.keep_all = TRUE)

##-----------------------------------------------
# 3.3. first clean out the common typos -----
##-----------------------------------------------
message("3.3. first clean out the common typos")

# common patterns numbers
pattern <- "(\\w+\\s+){0,5}(\\d+\\s*\\-?\\s*\\d*\\s*m)(\\s+\\w+){0,5}"

# Clean common typos in the habitat column ¢
hmw_clean <- hmw_clean |>
  # Remove '¢.' or 'c.'
  mutate(habitat = str_replace_all(habitat, regex("¢\\.|c\\.", ignore_case = TRUE), "")) |>
  mutate(habitat = str_replace_all(habitat, regex("\\b(c|¢)\\b", ignore_case = TRUE), "")) |>
  # Remove 'c .' or 'C .', with case insensitivity and regardless of spaces between characters
  mutate(habitat = str_replace_all(habitat, regex("c\\s*\\.\\s*", ignore_case = TRUE), "")) |>
  # Remove '.' followed by spaces and digits
  mutate(habitat = str_replace_all(habitat, regex("\\.\\s*(\\d+)"), "\\1")) |>
  # Transform "number—number" with possible multiple dashes and spaces to "number m - number m"
  mutate(habitat = str_replace_all(habitat, regex("(\\d+)\\s*—[-]*\\s*(\\d+)"), "\\1 m - \\2 m")) |>
  # Transform "of1100 m" to "of number m"
  mutate(habitat = str_replace_all(habitat, regex("of(\\d+)\\s*m"), "of number m"))
# cleaned info from 'habitat'

##-----------------------------------------------------------------
# 3.4. Cleaning the elevations out of the habitat column -----
##-----------------------------------------------------------------
message("3.4. Cleaning the elevations out of the habitat column")

extract_elevation <- function(elevation_info) {
  
  # --- STEP 1: range pattern like 1000-1500m or 1000 - 1500 m ---
  range_pattern <- "(\\d+)\\s*-\\s*(\\d+)\\s*m(?!m)"
  range_match <- str_match(elevation_info, range_pattern)
  
  if (!is.na(range_match[1])) {
    num1 <- as.numeric(range_match[2])
    num2 <- as.numeric(range_match[3])
    
    # security: difference must be > 100m
    if (abs(num1 - num2) < 100) {
      return(tibble(min_elevation = NA_real_, max_elevation = NA_real_))
    }
    if (num1 < 50) {
      return(tibble(min_elevation = 0))
    }
    
    return(tibble(
      min_elevation = min(num1, num2),
      max_elevation = max(num1, num2),
    ))
  }
  
  # --- STEP 2: two separate elevations like "1000 m ... 1500 m" ---
  separate_pattern <- "(\\d+)\\s*m(?!m)"
  separate_matches <- str_match_all(elevation_info, separate_pattern)[[1]]
  
  if (nrow(separate_matches) >= 2) {
    nums <- as.numeric(separate_matches[, 2])
    
    # security: difference must be > 100m
    if (abs(max(nums) - min(nums)) < 100) {
      return(tibble(min_elevation = NA_real_, max_elevation = NA_real_))
    }
    if (min(nums) < 50) {
      return(tibble(min_elevation = 0))
    }
    
    return(tibble(
      min_elevation = min(nums),
      max_elevation = max(nums),
    ))
  }
  
  # --- STEP 3: nothing found ---
  return(tibble(
    min_elevation = NA_real_,
    max_elevation = NA_real_,
  ))
}

# Apply to the habitat column
hmw_elevation <- hmw_clean |>
  mutate(elevation_data = map(habitat, extract_elevation)) |>
  tidyr::unnest(cols = c(elevation_data)) %>%
  select(- habitat)


# Match the cleaned Handbook of Mammals Dataset with MDD checklist
mammals_dataframe <- mammals_dataframe %>%
  left_join(hmw_elevation, by = "sciname")

rm(hmw_data, hmw_matched, hmw_clean, hmw_elevation)
gc()

##----------------------------------------------------------
# ----- 4. Get elevations with DEM 
##----------------------------------------------------------
message("4. Get elevations with DEM ")

# This snippet extract the min and max elevational limits of each species in each mountain range
# I use the Digital Elevation Model Copernicus GLO-90, with a resolution of 90m
# https://portal.opentopography.org/raster?opentopoID=OTSDEM.032021.4326.1
# European Space Agency (2024). Copernicus Global Digital Elevation Model. Distributed by OpenTopography. https://doi.org/10.5069/G9028PQB.

# The procedure is the following:
#   1. I estimate the average best quantiles to estimate ranges limits, i.e. the quantiles with the average 
#     lowest deviation to the 'true limits' that we extracted from the literature (see part 3)
#   2. Based on these quantiles, I extract the elevational limits for each species x mountain range

# LOGIC: Because we compare mountain specific limits (extracted from the DEM for each mountain range) with "true" elevational limits that are 
# species specific but not mountain specific, we can have strong mismatches for widespread species. Therefore, we add a safety check
# with the overlap_pct argument, only selecting in this process species with an overlap percentage > 50%, to ensure that the species is
# specific to this mountain range or to this area (i.e. can include neighbouring mountain ranges).


# Here I will need to load range distribution, so it's gonna be in a loop again
# From the base dataframe, we process by order:
# --> we extract the zip file with multipolygons corresponding to this order
# --> we add the geom column to our base dataframe filtered for this order
# --> then we can run the function 

zip_folder <- paste0(source_path, "GMBA_project/Raw_datasets/Mammals/MDD_Mammalia/")
zip_files <- list.files(zip_folder, pattern = "\\.zip$", full.names = TRUE)
dem <- terra::rast(paste0(source_path, "GMBA_project/demMountains_GLO90.tif"))
overlap_threshold <- 20
chunk_size <- 10

# Precompute mountain_join
mountain_join <- mountain_shapes03 %>%
  select(-Level_01) %>%
  st_make_valid() %>%
  mutate(geom_mountain = geometry) %>%
  st_drop_geometry()

all_intersects <- list()   # store results from all orders

for (zip_file in zip_files) {     
  
  # extract the name of the order from the file name
  order_name <- sub("MDD_", "", tools::file_path_sans_ext(basename(zip_file)))
  message("\n--- Processing order: ", order_name, "---\n")
  
  # Unzip the file and check if there is a gpkg file, i.e. what we are looking for
  zip_contents <- unzip(zip_file, list = TRUE)
  gpkg_file <- zip_contents$Name[grepl("\\.gpkg$", zip_contents$Name)][1]
  
  # If there is one, we unzip it in the tempdir() and then load it into R Studio
  if (!is.na(gpkg_file)) {
    unzip(zip_file, files = gpkg_file, exdir = mytempdir)
    gpkg_path <- file.path(mytempdir, gpkg_file)
    layer_name <- sf::st_layers(gpkg_path)$name[1]
    
    # Get species for this order from mammals_dataframe
    order_species <- mammals_dataframe %>%
      filter(order == order_name) %>%
      pull(sciname) %>%
      unique()
    
    total_species <- length(order_species)
    message(sprintf("Total species in dataframe for %s: %d", order_name, total_species))
    
    offsets <- seq(0, total_species, by = chunk_size)
    chunk_intersects <- list()
    
    for (offset in offsets) {
      if (offset >= total_species) next  # skip if no rows left
      
      message(sprintf("Reading chunk offset %d/%d", offset, total_species))
      
      # Get species names for this chunk
      chunk_species <- order_species[(offset + 1):min(offset + chunk_size, total_species)]
      species_list <- paste0("'", chunk_species, "'", collapse = ",")
      
      # Read only the shapes for these species
      chunk_shapes <- sf::st_read(gpkg_path,
                                  query = paste0("SELECT sciname, \"order\", family, geom 
                                                 FROM ", layer_name, "
                                                 WHERE sciname IN (", species_list, ")"),
                                  quiet = TRUE) %>%
        st_make_valid() %>%
        st_transform(8857) %>%          # reproject to Equal Earth (meters)
        st_simplify(dTolerance = 1000) %>%  # simplify to 1km
        st_transform(4326) %>%          # reproject back to WGS84
        st_make_valid()                 # fix any issues after simplification
      
      # Join with mammals_dataframe for this chunk
      order_df <- mammals_dataframe %>%
        filter(sciname %in% chunk_species) %>%
        left_join(chunk_shapes)
      
      order_df_sf <- st_as_sf(order_df) %>%
        sf::st_make_valid()
      
      # Intersect for each row the species distribution with the corresponding mountain shp
      sf_use_s2(FALSE)
      
      chunk_intersect <- order_df_sf %>%
        st_make_valid() %>%
        left_join(mountain_join, by = c("Mountain_range" = "Level_03", "Mountain_system" = "Level_02")) %>%
        mutate(
          geom = purrr::map2(
            geom, geom_mountain,
            ~ {
              if (is.null(.y) || length(.y) == 0) {
                return(st_geometrycollection())
              }
              tryCatch(
                st_intersection(.x, .y),
                error = function(e) {
                  message("Intersection failed: ", e$message)
                  st_geometrycollection()
                }
              )
            }
          )
        ) %>%
        mutate(geom = st_as_sfc(geom, crs = st_crs(order_df_sf))) %>%  # convert list to sfc
        select(-geom_mountain) %>%
        st_as_sf(sf_column_name = "geom")
      
      sf_use_s2(TRUE)
      
      chunk_intersects[[as.character(offset)]] <- chunk_intersect
      
      rm(chunk_shapes, chunk_species, chunk_intersect, order_df, order_df_sf)
      gc()
      
    }
    
    # Combine all chunks for this order
    all_intersects[[order_name]] <- dplyr::bind_rows(chunk_intersects)
    
    unlink(gpkg_path)
    rm(chunk_intersects)
    gc()
    
    message("\n--- Done with ", order_name, "! ---\n")
  }
}

# Combine ALL orders for quantile estimation
message("Combining all orders for quantile estimation...")
all_mammals_intersect <- dplyr::bind_rows(all_intersects)

rm(all_intersects)
gc()

message("Estimating quantiles for all orders...")
# Estimate quantiles
quantiles <- estimate.quantile(all_mammals_intersect, dem, overlap_threshold)

if (is.null(quantiles)) stop("No valid quantiles found — check elevation data")

quantile_min <- quantiles %>%
  filter(quantile <= 0.49) %>%
  filter(mean_dev_min == min(mean_dev_min))
quantile_max <- quantiles %>%
  filter(quantile >= 0.51) %>%
  filter(mean_dev_max == min(mean_dev_max))

message("Extracting elevation for all species...")
# Extract DEM elevational limits
mammals_elevations_DEM <- extract.elevational.limits.DEM(all_mammals_intersect, dem, quantile_min, quantile_max)

# Combine all orders and join back to main dataframe
mammals_dataframe <- mammals_dataframe %>%
  left_join(dplyr::bind_rows(mammals_elevations_DEM), by = c("sciname", "Mountain_range"))

##----------------------------------------------------------
# ----- 5. Get mammals elevational ranges with GBIF 
##----------------------------------------------------------

# This snippet extract the min and max elevational limits of each species in each mountain range
# I use the Digital Elevation Model Copernicus GLO-90, with a resolution of 90m
# https://portal.opentopography.org/raster?opentopoID=OTSDEM.032021.4326.1
# European Space Agency (2024). Copernicus Global Digital Elevation Model. Distributed by OpenTopography. https://doi.org/10.5069/G9028PQB.

##--------------------------------
# 5.1. Import GBIF dataset -----
##--------------------------------
message("5.1. Import & clean GBIF dataset")

mammals_GBIF <- arrow::open_dataset(paste0(source_path, "GBIF_data/data/Mammals_parquetclean"))

# -----------
# TAKE A SUBSET (TO BE REMOVED) 
# The dataset is huge, so I first collect the species list and sample 50 of them
#species_sample <- mammals_GBIF %>%
  #distinct(species) %>%
  #collect() %>%          
  #slice_sample(n = 50) %>%
  #pull(species)

# Then I filter the GBIF dataset with this 50 species
mammals_GBIF <- mammals_GBIF %>%
  #filter(species %in% species_sample) %>%
  dplyr::select(species, decimalLatitude, decimalLongitude, Level_01, Level_02,
                Level_03) %>%
  collect()
# -----------

mammals_GBIF <- mammals_GBIF %>%
  rename(sciname = "species")

# Fill empty Level_03 by the Level_02 or Level_01
mammals_GBIF <- mammals_GBIF %>%
  mutate(
    Level_03 = coalesce(Level_03, Level_02, Level_01),
    Level_02 = coalesce(Level_02, Level_01))

##---------------------------------------
# 5.2. Standardize species names -----
##---------------------------------------
# Here, we use the function rgbif::name_backbone_checklist to standardize both GBIF and literature with the same procedure
# The function standardize.species.names() follow the following procedure:
#   1. for both gbif and literature species list, it return a dataframe with original names and corrected names
#   2. in both dataset, it removes the species names flagged as unsufficiently accurate
#   3. in both dataset, it replaces the original species names by the "true ones" from the rgbif function
#   4. then it join both dataset, keeping in the gbif dataset only species found in the literature

# The return is the gbif cleaned version, with standardized species names and only species found in our literature dataset

species_names <- standardize.species.names(mammals_GBIF, mammals_dataframe)
GBIF_clean <- species_names$gbif_final

# Update sciname in base dataframe
mammals_dataframe <- mammals_dataframe %>%
  left_join(species_names$name_mapping, 
            by = c("sciname" = "verbatim_name")) %>%
  mutate(sciname = ifelse(!is.na(canonicalName), canonicalName, sciname)) %>%
  select(-canonicalName)

##------------------------
# 5.3. Extract -----
##------------------------
# This function simply extract elevational limits from GBIF occurrences
#   1. extract the elevation for each occurrences based on latitude and longitude coordinates
#   2. group by species and mountain range (Level_03) and calculate the quantiles 0.05 and 0.95 to extract min and max elevational limits
# We calculate with all the species > 1 occurrences, i.e. even those with only 2 occurrences

mammals_GBIF_elev <- extract.elevational.limits.GBIF(GBIF_clean, dem)

# A bit of cleaning
mammals_GBIF_elev <- mammals_GBIF_elev %>%
  rename(Mountain_range = "Level_03")

# Add GBIF elev to the base dataframe
mammals_dataframe <- mammals_dataframe %>%
  left_join(mammals_GBIF_elev, by = c("sciname", "Mountain_range"))

##--------------------------
# 5.4. Save data -----
##--------------------------

# Save the file
writexl::write_xlsx(mammals_dataframe, paste0(source_path, "GMBA_project/files_processed/mammals_dataframe.xlsx"))

##----------------------------------------------------------
# ----- 6. Clean and sort for expert validation
##----------------------------------------------------------
message("6. Clean and sort for expert validation")

mammals_dataframe_experts <- mammals_dataframe %>%
  select(-c(overlap_area, overlap_pct, species_area, NumberOcc,
            Abs_min_elevation_GBIF, Abs_max_elevation_GBIF)) %>%  # remove overlap info useless for experts
  mutate(
    presence_corrected = "",
    min_corrected = "",
    max_corrected = "",
    validated_elevation_data = "",
    confidence_assessment = "",
    reviewer_comments = ""
  )

# Write one file per mountain range to be sent to expert
outdir <- paste0(source_path, "GMBA_project/Outputs/Mammals/")
if (!file.exists(outdir)) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
}

mountain_ranges <- unique(mammals_dataframe_experts$Mountain_range)

for (mr in mountain_ranges) {
  df_sub <- mammals_dataframe_experts %>%
    filter(Mountain_range == mr)
  
  # Clean the name for use as filename (remove special characters)
  clean_name <- gsub("[^a-zA-Z0-9_-]", "_", mr)
  
  write_xlsx(df_sub, paste0(outdir, clean_name, ".xlsx"))
}