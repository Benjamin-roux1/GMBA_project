
# ---------------------------------------------------#
#  1. Source Mammal distribution Data from MDD
# ----------------------------------------------------#

# This script unpacks and opens zip folders containing gpkg for all mammals 
# zips can be downloaded via the Mammal Diversity Database: https://www.mammaldiversity.org/assets/data/MDD.zip

# There are some large files --> processing each order separately

#----------------------------------------------------------#
# 1.1 Set up  -----
#----------------------------------------------------------#
library(here); library(data.table); library(dplyr)
library(tidyverse); library(readxl); library(terra)
library(sf); library(arrow); library(rgbif)

# Load configuration
#source(
#here::here("R/00_Config_file.R")
#)

# define data path OR even config.R file with libraries & path
source_path <- "C:/Users/berou1714/OneDrive - Norwegian University of Life Sciences/Desktop/PhD_project/"

# source all functions
list.files(path = paste0(source_path, "GMBA_project/Functions"), pattern = "*.R", full.names = TRUE) %>%
  purrr::walk(source)

#----------------------------------------------------------#
# 1.2 Unzip folder containing range shapefiles  -----
#----------------------------------------------------------#

# First, let's have a look at the organization of the folders and dataframe

# Define the folder containing the zip file(s)
zip_folder <- paste0(source_path, "GMBA_project/Raw_datasets/Mammals/MDD_Mammalia/")
# Find the zip file in the folder
zip_file <- list.files(zip_folder, pattern = "\\.zip$", full.names = TRUE)[1]
# Peek inside the zip
zip_content <- unzip(zip_file, list = TRUE)
# Find the .gpkg file inside
gpkg_file <- zip_content$Name[grepl("\\.gpkg$", zip_content$Name)][1]
# Extract and read it
unzip(zip_file, files = gpkg_file, exdir = tempdir())
mammals <- sf::st_read(file.path(tempdir(), gpkg_file), quiet = TRUE)
head(mammals)

# These files are heavy. Everytime we will need the geometry (crop, DEM), we will loop over the orders.

# ----------------------------------------------------------#
# 2. Overlap Mammal ranges with GMBA shapefile
# ----------------------------------------------------------#

# This script overlaps mammal distribution ranges with GMBA mountain ranges (level 03) and alpine biome 
# in the end the geometries of the species a checklist is saved with each order as seperate sheet. 

# ❗ The species range shps are partly very large files. Therefore, I process each order seperately

#----------------------------------------------------------#
# 2.3 Source gmba mountain and alpine biome shps   -----
#----------------------------------------------------------#

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

#----------------------------------------------------------------------------------------#
# 2.4. Intersect species ranges with GMBA and calculate % of overlap -----
#-----------------------------------------------------------------------------------------#
# We are going to loop over each order separately, instead of loading them all on R
# So for each order, which is zipped file, we first unzip it and then process

# Source to the folder with all the zipped files and make it a list of files
zip_folder <- paste0(source_path, "GMBA_project/Raw_datasets/Mammals/MDD_Mammalia/TEST/")
zip_files <- list.files(zip_folder, pattern = "\\.zip$", full.names = TRUE)

all_results <- list()   # store results from all orders
all_failures <- list()  # store failures from all orders

for (zip_file in zip_files) {     # HERE START THE LOOP
  
  # extract the name of the order from the file name
  order_name <- sub("MDD_", "", tools::file_path_sans_ext(basename(zip_file)))
  message("Processing order: ", order_name)
  
  # Unzip the file and check if there is a gpkg file, i.e. what we are looking for
  zip_contents <- unzip(zip_file, list = TRUE)
  gpkg_file <- zip_contents$Name[grepl("\\.gpkg$", zip_contents$Name)][1]
  
  # If there is one, we unzip it in the tempdir() and then load it into R Studio
  if (!is.na(gpkg_file)) {
    unzip(zip_file, files = gpkg_file, exdir = tempdir())
    gpkg_path <- file.path(tempdir(), gpkg_file)
    layer_name <- sf::st_layers(gpkg_path)$name[1]
    
    mammals_shapes <- sf::st_read(gpkg_path, 
                                  query = paste0("SELECT sciname, \"order\", family, geom FROM ", layer_name),
                                  quiet = TRUE)


# The function intersect_species_mountain ranges:
# 1. creates bboxes for mountain ranges and for the species that is beeing processed. 
# 2. If sp and mountain bbox intersect 
#   2.1. it calculates the area of a species in km2
#   2.2. the percentage of overlap of the species range with the mountain range 
#   2.3. the percentage of overlap with the alpine biome in that mountain range
# 3. removes all species with < 1% overlap with a GMBA Mountain range

    results <- overlap.mountain(mountain_shapes03, mammals_shapes)

# Result is a list with two dataframes:
# results_df contains all species that have succesfully been processed
# failures_df contains species where an error occured

    # Store results, adding order name for traceability
    if (!is.null(results$results)) {
      all_results[[order_name]] <- results$results
    }
    if (!is.null(results$failures)) {
      all_failures[[order_name]] <- results$failures
    }
  }
}    # HERE END THE LOOP


# Combine all orders into one dataframe at the end
mammals_dataframe <- dplyr::bind_rows(all_results)
mammals_failures  <- dplyr::bind_rows(all_failures)


#--------------------------------------------------------------#
#  4. Clean the data from Handbook of Mammals 
#--------------------------------------------------------------#

# Physical copies of Handbook of the Mammals of the World available at
# https://github.com/jhpoelen/hmw

# This script cleans textual information from the handbook of mammals to min and max elevational ranges for mammals. 

# ❗ ATTENTION !! the functions below do not clean HMW completely. there are still elevationa data that can not be grasped by the functions
# after running this script the cleaning of the output file has been finalized manually


#------------------------#
# 4.2. Download the data
#-------------------------#

# This is the single files combined
url <- "https://raw.githubusercontent.com/jhpoelen/hmw/main/hmw.csv"
hmw_data <- read.csv(url)

#----------------------------------#
# 4.3. Filter and clean HMW
#----------------------------------#
hmw_data <- hmw_data %>%
  rename(sciname = "name")

# Keep only species in HMW that we have in our checklist
hmw_matched <- hmw_data %>%
  semi_join(mammals_dataframe, by = "sciname")

# clean the dataframe hmw
hmw_clean <- hmw_matched %>%
  select(sciname, habitat) %>%
  distinct(sciname,.keep_all = TRUE)

#-------------------------------------------------------#
# 4.4. first clean out the common typos
#-------------------------------------------------------#

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

#--------------------------------------------------------------------#
# 4.5. Cleaning the elevations out of the habitat column 
#---------------------------------------------------------------------#

extract_elevation <- function(elevation_info) {
  
  # --- STEP 1: range pattern like 1000-1500m or 1000 - 1500 m ---
  range_pattern <- "(\\d+)\\s*-\\s*(\\d+)\\s*m(?!m)"  # ✅ (?!m) excludes "mm"
  range_match <- str_match(elevation_info, range_pattern)
  
  if (!is.na(range_match[1])) {
    num1 <- as.numeric(range_match[2])
    num2 <- as.numeric(range_match[3])
    
    # ✅ Security: difference must be > 100m
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
  separate_pattern <- "(\\d+)\\s*m(?!m)"  # ✅ (?!m) excludes "mm"
  separate_matches <- str_match_all(elevation_info, separate_pattern)[[1]]
  
  if (nrow(separate_matches) >= 2) {
    nums <- as.numeric(separate_matches[, 2])
    
    # ✅ Security: difference must be > 100m
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

#---------------------------------------------------------------------------------#
#  5. Match the cleaned Handbook of Mammals Dataset with MDD checklist
#--------------------------------------------------------------------------------#

# This script loads the cleaned handbook of mammals and the checklist (with overlaps mountain ranges) 
# and joins the available data by species names

mammals_dataframe <- mammals_dataframe %>%
  left_join(hmw_elevation, by = "sciname")

#----------------------------------------------------------#
# 7. Get elevations with DEM 
#----------------------------------------------------------#

# This snippet extract the min and max elevational limits of each species in each mountain range
# I use the Digital Elevation Model Copernicus GLO-90, with a resolution of 90m
# https://portal.opentopography.org/raster?opentopoID=OTSDEM.032021.4326.1
# European Space Agency (2024). Copernicus Global Digital Elevation Model. Distributed by OpenTopography. https://doi.org/10.5069/G9028PQB.

# The procedure is the following:
#   1. I estimate the average best quantiles to estimate ranges limits, i.e. the quantiles with the average 
#     lowest deviation to the 'true limits' that we extracted from the litterature (see part 3)
#   2. Based on these quantiles, I extract the elevational limits for each species x mountain range



# Here I will need to load range distribution, so it's gonna be in a loop again
# From the base dataframe, we process by order:
# --> we extract the zip file with multipolygons corresponding to this order
# --> we add the geom column to our base dataframe filtered for this order
# --> then we can run the function 

zip_folder <- paste0(source_path, "GMBA_project/Raw_datasets/Mammals/MDD_Mammalia/TEST/")
zip_files <- list.files(zip_folder, pattern = "\\.zip$", full.names = TRUE)
dem <- terra::rast(paste0(source_path, "GMBA_project/demMountains_GLO90.tif"))
overlap_threshold <- 20

all_results <- list()   # store results from all orders
all_failures <- list()  # store failures from all orders

for (zip_file in zip_files) {     
  
  # extract the name of the order from the file name
  order_name <- sub("MDD_", "", tools::file_path_sans_ext(basename(zip_file)))
  message("\n--- Processing order: ", order_name, "---\n")
  
  # Unzip the file and check if there is a gpkg file, i.e. what we are looking for
  zip_contents <- unzip(zip_file, list = TRUE)
  gpkg_file <- zip_contents$Name[grepl("\\.gpkg$", zip_contents$Name)][1]
  
  # If there is one, we unzip it in the tempdir() and then load it into R Studio
  if (!is.na(gpkg_file)) {
    unzip(zip_file, files = gpkg_file, exdir = tempdir())
    gpkg_path <- file.path(tempdir(), gpkg_file)
    layer_name <- sf::st_layers(gpkg_path)$name[1]
    
    mammals_shapes <- sf::st_read(gpkg_path, 
                                  query = paste0("SELECT sciname, \"order\", family, geom FROM ", layer_name),
                                  quiet = TRUE)
    message("\n--- File unzipped! ---\n")
    
    order_df <- mammals_dataframe %>%
      filter(order == order_name) %>%
      left_join(mammals_shapes, by = "sciname")
    
    order_df_sf <- st_as_sf(order_df) %>%
      sf::st_make_valid()
    
    message("\n--- Intersect each species of ", order_name, " to the mountain range! ---\n")
    # Intersect for each row the species distribution with the corresponding mountain shp
    sf::sf_use_s2(FALSE)
      mammals_intersect <- order_df_sf %>%
        rowwise() %>%
        mutate(
          geom = st_intersection(
            geom,
            mountain_shapes03 %>% 
              filter(Level_03 == Mountain_range) %>% 
              st_geometry()
          )
        ) %>%
        ungroup()
    sf::sf_use_s2(TRUE)
    
    message("\n--- Estimation of the best quantiles! ---\n")
    quantiles <- estimate.quantile(mammals_intersect, dem, overlap_threshold)
    quantile_min <- quantiles %>%
      filter(quantile <= 0.49) %>%
      filter(mean_dev_min == min(mean_dev_min))
    quantile_max <- quantiles %>%
      filter(quantile >= 0.51) %>%
      filter(mean_dev_max == min(mean_dev_max))
    
    message("\n--- Extraction of the DEM elevational limits! ---\n")
    mammals_elevations_DEM <- extract.elevational.limits.DEM(mammals_intersect, dem, quantile_min, quantile_max)
    
    all_results[[order_name]] <- mammals_elevations_DEM
    
    message("\n--- Job is done for ", order_name, "! ---\n")
  }
}

# combine all orders and join back to the main dataframe
mammals_dataframe <- mammals_dataframe %>%
  left_join(dplyr::bind_rows(all_results), by = c("sciname", "Mountain_range"))


#------------------------------------------------------------------------#
# ----- 5. Get mammals elevational ranges with GBIF 
#-------------------------------------------------------------------------#

# This snippet extract the min and max elevational limits of each species in each mountain range
# I use the Digital Elevation Model Copernicus GLO-90, with a resolution of 90m
# https://portal.opentopography.org/raster?opentopoID=OTSDEM.032021.4326.1
# European Space Agency (2024). Copernicus Global Digital Elevation Model. Distributed by OpenTopography. https://doi.org/10.5069/G9028PQB.

# The procedure is the following:
#   1. I estimate the average best quantiles to estimate ranges limits, i.e. the quantiles with the average 
#     lowest deviation to the 'true limits' that we extracted from the litterature (see part 3)
#   2. Based on these quantiles, I extract the elevational limits for each species x mountain range

#--------------------------------#
# 5.1. Import GBIF dataset -----
#--------------------------------#

mammals_GBIF <- arrow::open_dataset(paste0(source_path, "GBIF_data/data/Squamata_parquetclean"))

# -----------
# TAKE A SUBSET (TO BE REMOVED) 
# The dataset is huge, so I first collect the species list and sample 50 of them
species_sample <- mammals_GBIF %>%
  distinct(species) %>%
  collect() %>%          
  slice_sample(n = 50) %>%
  pull(species)

# Then I filter the GBIF dataset with this 50 species
mammals_GBIF <- mammals_GBIF %>%
  filter(species %in% species_sample) %>%
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

#---------------------------------------#
# 5.2. Standardize species names -----
#---------------------------------------#
species_names <- standardize.species.names(mammals_GBIF, mammals_mountain)
GBIF_clean <- species_names$gbif
mammals_clean <- species_names$litterature

#------------------------#
# 5.3. Extract -----
#------------------------#
mammals_GBIF_elev <- extract.elevational.limits.GBIF(GBIF_clean, dem)

# A bit of cleaning
mammals_GBIF_elev <- mammals_GBIF_elev %>%
  rename(Mountain_range = "Level_03")

# Add GBIF elev to the base dataframe
mammals_dataframe <- mammals_dataframe %>%
  left_join(mammals_GBIF_elev, by = c("sciname", "Mountain_range"))

#---------------------------#
# 5.4. Save data -----
#--------------------------#

# Save the file
writexl::write_xlsx(mammals_dataframe, paste0(source_path, "GMBA_project/files_processed/mammals_dataframe.xlsx"))

#----------------------------------------------------------#
# ----- 6. Clean and sort for expert validation
#----------------------------------------------------------#

mammals_dataframe_experts <- mammals_dataframe %>%
  select(-c(overlap_area, overlap_pct, species_area)) %>%  # remove overlap info useless for experts
  mutate(
    presence_corrected = "",
    min_corrected = "",
    max_corrected = "",
    validated_elevation_data = "",
    confidence_assessment = "",
    reviewer_comments = ""
  )