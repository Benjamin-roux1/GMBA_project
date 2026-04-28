##-----------------------------------------------------------------------------
#  ---- 1. Source and check out data from BirdLife International
##-----------------------------------------------------------------------------

# Data has been received from BirdLife International after formal application
# BirdLife International and Handbook of the Birds of the World (2022) Bird species distribution maps of the world. Version 2022.2. 
# Sourced from http://datazone.birdlife.org/species/requestdis.

##-------------------------
# 1.1. Set up  -----
##-------------------------
library(here); library(data.table); library(dplyr)
library(tidyverse); library(readxl); library(terra)
library(sf); library(arrow); library(rgbif); library(gpkg)
library(writexl)

# Load configuration
#source(
#here::here("R/00_Config_file.R")
#)

# define data path OR even config.R file with libraries & path
source_path <- "C:/Users/berou1714/OneDrive - Norwegian University of Life Sciences/Desktop/PhD_project/"

# source all functions
list.files(path = paste0(source_path, "GMBA_project/Functions"), pattern = "*.R", full.names = TRUE) %>%
  purrr::walk(source)

##-------------------------------
# 1.2. Load the data  -----
##-------------------------------
geopac <- geopackage(paste0(source_path, "GMBA_project/Raw_datasets/Birds/species/BOTW_2025.gpkg"), connect = TRUE)
gpkg_list_tables(geopac)
gpkg_tbl(geopac, "all_species")

##--------------------------------
# 1.3. Explore the data  -----
##--------------------------------

# total rows -> 11961
gpkg_table(geopac, "all_species") %>%
  filter(presence %in% c(1, 2, 3), seasonal == 1) %>%
  summarise(n_rows = n()) %>%
  collect()

# unique species -> 10382
gpkg_table(geopac, "all_species") %>%
  filter(presence %in% c(1, 2, 3), seasonal == 1) %>%
  summarise(n_species = n_distinct(sci_name)) %>%
  collect()

# So we have duplicated species, we will have to union the polygons per species first

birds_shapes <- st_read(paste0(source_path, "GMBA_project/Raw_datasets/Birds/species/BOTW_2025.gpkg"),
                 query = "SELECT sci_name, geom
                          FROM all_species 
                          WHERE presence IN (1, 2, 3)
                          AND seasonal = 1")

# Lets work with a subset (TO BE REMOVED)
#birds_shapes <- birds_shapes[sample(nrow(birds_shapes), 50), ]
############

birds_shapes <- birds_shapes %>%
  rename(sciname = "sci_name")

# Visual check
ggplot(birds_shapes) +
  geom_sf(data = birds_shapes[3,], fill = "lightblue") +
  theme_perso()

##------------------------------------------
#  1.4. Cropping duplicated species -----
##------------------------------------------

duplicated_species <- birds_shapes %>%
  st_drop_geometry() %>%
  group_by(sciname) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  pull(sciname)

# Function to union ranges if species has more than one range
union.ranges <- function(duplicated_species, all_species) {
  
  # union the duplicated species
  birds_unioned <- birds_shapes %>% 
    filter(sciname %in% duplicated_species) %>%
    group_by(sciname) %>%
    summarise(geom = st_union(geom), .groups = "drop")
  
  # keep non-duplicated species as is
  birds_single <- birds_shapes %>%
    filter(!sciname %in% duplicated_species)
  
  # combine both
  birds_final <- bind_rows(birds_single, birds_unioned)
  
  return(birds_final)
}

# Process each species and combine results
results <- union.ranges(duplicated_species, birds_shapes)

birds_shapes_clean <- results

##---------------------------------------------------------
#  ----- 2. Overlap Birds ranges with GMBA shapefile
##---------------------------------------------------------

# This script overlaps birds distribution ranges (BirdLife International) with GMBA mountain ranges (level 03)

##-------------------------------------
# 2.1. Source gmba mountain -----
##-------------------------------------

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

##------------------------------------------------------------------------------
# 2.2. Intersect species ranges with GMBA and calculate % of overlap -----
##------------------------------------------------------------------------------

# The function overlap.mountain:
# 1. creates bboxes for mountain ranges 
# 2. If sp and mountain bbox intersect 
#   2.1. it takes the area of a species in km2 (is already in reptile dataset)
#   2.2. the percentage of overlap of the species range with the mountain range 
# 3. removes all species with < 5km2 and < 1% overlap with a GMBA Mountain range

# We chose these threshold to avoid excluding false negative, i.e. be as much inclusive as possible. 
# With the 5km2, we make sure to select even small ranges species, common in mountain areas, and for very small ranges
# species, i.e. < 5km2, we set a threshold at 1% to make sure to include them as well.

# Execute the main function
results <- overlap.mountain(mountain_shapes03, birds_shapes_clean)

# Result is a list with two dataframes:
# results_df contains all species that have succesfully been processed
# failures_df contains species where an error occured

results_success <- results$results_df
results_failures <- results$failures_df

# Let's create a base dataframe in which we will add the different columns throughout the process
birds_dataframe <- results_success

##-----------------------------------------------------
#  ----- 3. Bind Elevations to Species 
##-----------------------------------------------------

# This script binds elevation data to species names sourced from Global database of birds (quintero and Jetz, 2018)
# We have a file with the elevational limits per species and for different mountain ranges
# and a file with the link between the mountain ID number and the corresponding mountain range from GMBA

##-----------------------------
# 3.1. Load data -----
##-----------------------------

birds_elev_limits <- readxl::read_excel(paste0(source_path, "GMBA_project/Raw_datasets/Birds/elevation/birds_elevational_limits.xlsx"))
mountain_ID <- readxl::read_excel(paste0(source_path, "GMBA_project/Raw_datasets/Birds/elevation/mountain_range_ID.xlsx"))

##-----------------------------
# 3.2. Clean data -----
##-----------------------------
birds_elev_limits <- birds_elev_limits %>% 
  select(-7) %>%
  rename(Mountain_ID = "Mountain ID")
mountain_ID <- mountain_ID %>%
  select(Mountain_ID, `Mountain Range`) %>%
  filter(!is.na(Mountain_ID) & !is.na(`Mountain Range`)) %>%
  rename(Mountain_system = "Mountain Range")

##----------------------------------------------------------
# 3.3. Bind the mountain range ID to elevation limits -----
##----------------------------------------------------------
# Add the corresponding Mountain_system to the mountain ID number in the elev_limit file
birds_elev_limits <- birds_elev_limits %>%
  left_join(mountain_ID, by = "Mountain_ID") %>%
  select(- Mountain_ID) %>%
  rename(
    min_elevation = "Minimum elevation",
    max_elevation = "Maximum elevation",
    sciname = "Species"
  )

# There can be duplicates for a same species x Mountain system (because we also have a column country)
# We don't really know what these countries means, so we will take for these duplicates the min and max per mountain system
birds_elev_limits <- birds_elev_limits %>%
  group_by(sciname, Mountain_system) %>%
  summarise(
    min_elevation = min(min_elevation, na.rm = TRUE),
    max_elevation = max(max_elevation, na.rm =TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    min_elevation = ifelse(is.infinite(min_elevation), NA, min_elevation),  # clean the Inf values with NA
    max_elevation = ifelse(is.infinite(max_elevation), NA, max_elevation)
  )

birds_dataframe <- birds_dataframe %>%
  left_join(birds_elev_limits %>% select(sciname, min_elevation, max_elevation, Mountain_system),
            by = c("sciname", "Mountain_system"))

##----------------------------------------------------------
#  ----- 4. Get elevations with DEM 
##----------------------------------------------------------

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

##--------------------------------------
# 4.1. Load species data  -----
##--------------------------------------
# From the dataframe with species selected for each mountain range, we add their range distribution as a new column
birds_mountain <- birds_dataframe %>%
  left_join(birds_shapes_clean, by = "sciname")

##-------------------------------------------------------------
# 4.2. Crop species distribution in each mountain range  -----
##-------------------------------------------------------------
birds_mountain_sf <- st_as_sf(birds_mountain) %>%
  st_make_valid()

# Intersect for each row the species distribution with the corresponding mountain shp
{
  sf_use_s2(FALSE)
  birds_intersect <- birds_mountain_sf %>%
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
  sf_use_s2(TRUE)
  }


# Visual check of the cropping
sp <- birds_intersect[1, ]
bbox <- st_bbox(birds_mountain_sf %>% filter(sciname == sp$sciname))
ggplot() +
  geom_sf(data = mountain_shapes03, fill = NA, color = "grey50") +
  geom_sf(data = birds_mountain_sf %>% filter(sciname == sp$sciname),  # Whole species range
          fill = "lightblue", alpha = 0.4) + 
  geom_sf(data = birds_intersect %>% filter(sciname == sp$sciname),  # Intersected species range
          fill = "red", alpha = 0.6) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_perso()

# Now we have a dataframe with all the species and their distribution in each mountain ranges specifically

##------------------------------
# 4.3. Add the DEM  -----
##------------------------------
dem <- terra::rast(paste0(source_path, "GMBA_project/demMountains_GLO90.tif"))

##----------------------------------------
# 4.4. Estimate the best quantile  -----
##----------------------------------------

# Remember to choose an overlap threshold here
overlap_treshold <- 20
quantiles <- estimate.quantile(birds_intersect, dem, overlap_treshold)

quantile_min <- quantiles %>%
  filter(quantile <= 0.49) %>%
  filter(mean_dev_min == min(mean_dev_min))
quantile_min
quantile_max <- quantiles %>%
  filter(quantile >= 0.51) %>%
  filter(mean_dev_max == min(mean_dev_max))
quantile_max

quantiles %>%
  pivot_longer(cols = c(mean_dev_min, mean_dev_max),
               names_to = "type",
               names_prefix = "mean_dev_",
               values_to = "mean_dev") %>%
  ggplot(aes(x = quantile, y = mean_dev, fill = type)) +
  geom_col(alpha = 0.5, position = "identity") +
  geom_vline(xintercept = quantile_min$quantile, color = "blue", linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = quantile_max$quantile, color = "red", linewidth = 0.8, linetype = "dashed") +
  theme_minimal()

##---------------------------------------------------
# 4.5. extract elevational ranges with DEM -----
##---------------------------------------------------

birds_elevations_DEM <- extract.elevational.limits.DEM(birds_intersect, dem, quantile_min, quantile_max)

birds_dataframe <- birds_dataframe %>%
  left_join(birds_elevations_DEM, by = c("sciname", "Mountain_range"))


##------------------------------------------------------------------------
# ------ 5. Get birds elevational ranges with GBIF
##------------------------------------------------------------------------

# This snippet extract the min and max elevational limits of each species in each mountain range
# I use the Digital Elevation Model Copernicus GLO-90, with a resolution of 90m
# https://portal.opentopography.org/raster?opentopoID=OTSDEM.032021.4326.1
# European Space Agency (2024). Copernicus Global Digital Elevation Model. Distributed by OpenTopography. https://doi.org/10.5069/G9028PQB.


##---------------------------------------
# 5.1. Import & clean GBIF dataset ----
##---------------------------------------

# Import GBIF dataset
birds_GBIF <- arrow::open_dataset(paste0(source_path, "GBIF_data/data/Aves_parquetclean"))

# -----------
# TAKE A SUBSET (TO BE REMOVED) 
# The dataset is huge, so I first collect the species list and sample 50 of them
#species_sample <- birds_GBIF %>%
  #distinct(species) %>%
  #collect() %>%          
  #slice_sample(n = 50) %>%
  #pull(species)

# Then I filter the GBIF dataset with this 50 species
birds_GBIF <- birds_GBIF %>%
  #filter(species %in% species_sample) %>%
  dplyr::select(species, decimalLatitude, decimalLongitude, Level_01, Level_02,
                Level_03) %>%
  collect()
# -----------

birds_GBIF <- birds_GBIF %>%
  rename(sciname = "species")

# Fill empty Level_03 by the Level_02 or Level_01
birds_GBIF <- birds_GBIF %>%
  mutate(
    Level_03 = coalesce(Level_03, Level_02, Level_01),
    Level_02 = coalesce(Level_02, Level_01))

##---------------------------------------
# 5.2. Standardize species names ----
##---------------------------------------

# Here, we use the function rgbif::name_backbone_checklist to standardize both GBIF and literature with the same procedure
# The function standardize.species.names() follow the following procedure:
#   1. for both gbif and literature species list, it return a dataframe with original names and corrected names
#   2. in both dataset, it removes the species names flagged as unsufficiently accurate
#   3. in both dataset, it replaces the original species names by the "true ones" from the rgbif function
#   4. then it join both dataset, keeping in the gbif dataset only species found in the literature

# The return is the gbif cleaned version, with standardized species names and only species found in our literature dataset


species_names <- standardize.species.names(birds_GBIF, birds_mountain)
GBIF_clean <- species_names$gbif_final

##---------------------------------------
# 5.3. Extract elevational limits ----
##---------------------------------------
# This function simply extract elevational limits from GBIF occurrences
#   1. extract the elevation for each occurrences based on latitude and longitude coordinates
#   2. group by species and mountain range (Level_03) and calculate the quantiles 0.05 and 0.95 to extract min and max elevational limits

birds_GBIF_elev <- extract.elevational.limits.GBIF(GBIF_clean, dem)

# A bit of cleaning
birds_GBIF_elev <- birds_GBIF_elev %>%
  rename(Mountain_range = "Level_03")

# Add GBIF elev to the base dataframe
birds_dataframe <- birds_dataframe %>%
  left_join(birds_GBIF_elev, by = c("sciname", "Mountain_range"))

##------------------------
#  5.4. Save data -----
##------------------------

# Save the file
writexl::write_xlsx(birds_dataframe, paste0(source_path, "GMBA_project/files_processed/birds_dataframe.xlsx"))

##----------------------------------------------------------
# ------ 6. Clean and sort for expert validation
##----------------------------------------------------------

birds_dataframe_experts <- birds_dataframe %>%
  select(-c(overlap_area, overlap_pct, species_area, NumberOcc)) %>%  # remove overlap info useless for experts
  mutate(
    presence_corrected = "",
    min_corrected = "",
    max_corrected = "",
    validated_elevation_data = "",
    confidence_assessment = "",
    reviewer_comments = ""
  )

# Write one file per mountain range to be sent to expert
outdir <- paste0(source_path, "GMBA_project/Outputs/Birds/")
if (!file.exists(outdir)) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
}

mountain_ranges <- unique(birds_dataframe_experts$Mountain_range)

for (mr in mountain_ranges) {
  df_sub <- birds_dataframe_experts %>%
    filter(Mountain_range == mr)
  
  # Clean the name for use as filename (remove special characters)
  clean_name <- gsub("[^a-zA-Z0-9_-]", "_", mr)
  
  write_xlsx(df_sub, paste0(outdir, clean_name, ".xlsx"))
}