##----------------------------------------------------------
#  1. Source Reptile distribution Data from GARD
##----------------------------------------------------------

# zips can be downloaded via the GARD Global Assessment of Reptile Distribution v.1.7: http://www.gardinitiative.org/data.html
# Global distributions of 10,914 reptile species merged and collated from various sources.

# Roll et. al. 2017 The global distribution of tetrapods reveals a need for targeted reptile conservation. Nature Ecology & Evolution 1:1677-1682
# Caetano, et al. 2022. Automated assessment reveals that the extinction risk of reptiles is widely underestimated across space and phylogeny. PLoS Biology, 20(5): e3001544.

##-----------------------------
# ----- 1.1. Set up
##-----------------------------
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

# source all functions
list.files(path = paste0(source_path, "GMBA_project/Functions"), pattern = "*.R", full.names = TRUE) %>%
  purrr::walk(source)

##----------------------------------------
# 1.2. Load the range shapefiles  -----
##----------------------------------------
message("1.2. Load the range shapefiles")

reptile_shapes <- sf::st_read(paste0(source_path, "GMBA_project/Raw_datasets/Reptiles/Distribution/doi_10_5061_dryad_9cnp5hqmb__v20220427/Gard_1_7_ranges.shp"), 
                              options = "ENCODING=ISO-8859-1") %>%
  st_make_valid()

reptile_shapes <- reptile_shapes %>%
  rename(sciname = binomial)

# Explore the dataset
reptile_shapes_df <- reptile_shapes %>%
  st_drop_geometry()
# check the different groups
reptile_shapes_df %>%
  group_by(group) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

#### subset for code testing (TO BE REMOVED)
#reptile_shapes <- reptile_shapes[sample(nrow(reptile_shapes), 50), ]
####

##---------------------------------------------------------
#  ------ 2. Overlap Reptile ranges with GMBA shapefile
##---------------------------------------------------------

# This script overlaps reptile distribution ranges with GMBA mountain ranges (level 03) 

##-------------------------------------
# 2.1. Source gmba mountains -----
##-------------------------------------
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

# A bit of cleaning the dataframe
# some rows are not defined at Level03 or Level02
# in these cases, we fill the NA with the closest filled superior level
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

##--------------------------------------------------------------------------------------
# 2.2. Intersect species ranges with GMBA and calculate overlap (value in km2 and %) 
##--------------------------------------------------------------------------------------
message("2.2. Intersect species ranges with GMBA and calculate overlap (value in km2 and %) ")

# The function overlap.mountain:
# 1. Intersect species_df and all mountains --> return a list of the mountains that each species touch
#   2. Process by species:
#   3. calculate the area of the species in km2 
#     4. For each mountain ranges it actually touches:
#       5. calculate the percentage of overlap of the species range with the mountain range 
#       6. removes all species with < 5km2 and < 1% overlap with a GMBA Mountain range

# We chose these threshold to avoid excluding false negative, i.e. be as much inclusive as possible. 
# With the 5km2, we make sure to select even small ranges species, common in mountain areas, and for very small ranges
# species, i.e. < 5km2, we set a threshold at 1% to make sure to include them as well.

# Execute the main function
results <- overlap.mountain(mountain_shapes03, reptile_shapes)

# Result is a list with two dataframes:
# results_df contains all species that have succesfully been processed
# failures_df contains species where an error occured

results_success <- results$results_df
results_failures <- results$failures_df

# Let's create a base dataframe in which we will add the different columns throughout the process
reptile_dataframe <- results_success

##-----------------------------------------------------------------
#  ----- 3. Bind Elevations to Species 
##-----------------------------------------------------------------
message("3. Bind Elevations to Species ")

# This script binds elevation data to species names (GARD)
# elevation data has been obtained by Squambase, Meiri 2024
# https://onlinelibrary.wiley.com/doi/10.1111/geb.13812 

# --> database that contains information on multiple key traits for all 11,744 recognised species of Squamates worldwide
# Because this is Squamates only, we will not have informations on Testudines or Crocodilia

##------------------------
# 3.1. Load data -----
##------------------------

# Load the elevation data
elevation_data <- read_excel(paste0(source_path, "GMBA_project/Raw_datasets/Reptiles/Elevation/Supplementary_Table_S1_-_squamBase1.xlsx")) %>%
  select("Species name (Binomial)", "Minimum elevation (m)", "Maximum elevation (m)") %>%
  rename(sciname = "Species name (Binomial)") %>% 
  rename(min_elevation = "Minimum elevation (m)") %>%
  rename(max_elevation = "Maximum elevation (m)")

# Change column type of elevation limits to numeric
elevation_data[, 2:3] <- lapply(elevation_data[, 2:3], as.numeric)

##---------------------------
# 3.2. Left join data -----
##---------------------------
message("3.2. Left join data")

# Add extracted range limits to our base dataframe
reptile_dataframe <- reptile_dataframe %>%
  left_join(elevation_data, by = "sciname")

# We have some negative elevations value that could mean something in some depressions areas.
# So we keep them.

# count for how many species we miss min or max elevation data
reptile_dataframe %>%
  summarise(
    missing_min = sum(is.na(min_elevation)),
    missing_max = sum(is.na(max_elevation)),
    total       = n()
  )

##----------------------------------------------
#  ------- 4. Get elevations with DEM 
##----------------------------------------------
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

##---------------------------------
# 4.1. Load species data  ------
##---------------------------------
message("4.1. Load species data ")

# From the dataframe with species selected for each mountain range, we add their range distribution as a new column
reptile_mountain <- reptile_dataframe %>%
  left_join(reptile_shapes %>% select(sciname, geometry), by = "sciname")

##-------------------------------------------------------------
# 4.2. Crop species distribution in each mountain range  -----
##-------------------------------------------------------------
message("4.2. Crop species distribution in each mountain range")

reptile_mountain_sf <- st_as_sf(reptile_mountain) %>%
  st_make_valid()

# Intersect for each row the species distribution with the corresponding mountain shp
{
  sf_use_s2(FALSE)
reptile_intersect <- reptile_mountain_sf %>%
  st_make_valid() %>%
  rowwise() %>%
  mutate(
    geometry = tryCatch({
      st_intersection(
        st_make_valid(geometry),
        mountain_shapes03 %>%
          filter(Level_03 == Mountain_range) %>%
          st_make_valid() %>%
          st_geometry()
      )
    }, error = function(e) {
      message("Intersection failed for ", Mountain_range, ": ", e$message)
      st_geometrycollection()  # return empty geometry instead of crashing
    })
  ) %>%
  ungroup()
sf_use_s2(TRUE)
}


# Visual check of the cropping
sp <- reptile_intersect[1, ]
bbox <- st_bbox(reptile_mountain_sf %>% filter(sciname == sp$sciname))
ggplot() +
  geom_sf(data = mountain_shapes03, fill = NA, color = "grey50") +
  geom_sf(data = reptile_mountain_sf %>% filter(sciname == sp$sciname),  # Whole species range
          fill = "lightblue", alpha = 0.6) + 
  geom_sf(data = reptile_intersect %>% filter(sciname == sp$sciname),  # Intersected species range
          fill = "red", alpha = 0.4) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme.perso()

# Now we have a dataframe with all the species and their distribution in each mountain ranges specifically

##--------------------------
# 4.3. Add the DEM  -----
##--------------------------
dem <- terra::rast(paste0(source_path, "GMBA_project/demMountains_GLO90.tif"))

##----------------------------------------
# 4.4. Estimate the best quantile  -----
##----------------------------------------
message("4.4. Estimate the best quantile")

overlap_treshold <- 20
quantiles <- estimate.quantile(reptile_intersect, dem, overlap_treshold)

ggplot(quantiles, aes(x = quantile)) +
  geom_col(aes(y = mean_dev_min), fill = "red", alpha = 0.5) + 
  geom_col(aes(y = mean_dev_max), fill = "blue", alpha = 0.5) +
  theme_minimal()

##-----------------------------------------------------
# 4.5. Get reptile elevational ranges with DEM -----
##-----------------------------------------------------
message("4.5. Get reptile elevational ranges with DEM")

quantile_min <- quantiles %>%
  filter(quantile <= 0.49) %>%
  filter(mean_dev_min == min(mean_dev_min))
quantile_min
quantile_max <- quantiles %>%
  filter(quantile >= 0.51) %>%
  filter(mean_dev_max == min(mean_dev_max))
quantile_max

reptile_elevations_DEM <- extract.elevational.limits.DEM(reptile_intersect, dem, quantile_min, quantile_max)

reptile_dataframe <- reptile_dataframe %>%
  left_join(reptile_elevations_DEM, by = c("sciname", "Mountain_range"))

##------------------------------------------------------------------------
# ------ 5. Get reptile elevational ranges with GBIF
##------------------------------------------------------------------------

# This snippet extract the min and max elevational limits of each species in each mountain range
# I use the Digital Elevation Model Copernicus GLO-90, with a resolution of 90m
# https://portal.opentopography.org/raster?opentopoID=OTSDEM.032021.4326.1
# European Space Agency (2024). Copernicus Global Digital Elevation Model. Distributed by OpenTopography. https://doi.org/10.5069/G9028PQB.

##---------------------------------------
# 5.1. Import & clean GBIF dataset ----
##---------------------------------------
message("5.1. Import & clean GBIF dataset")

reptile_GBIF <- arrow::open_dataset(paste0(source_path, "GBIF_data/data/Squamata_parquetclean"))

# Collect Parquet dataset to R
reptile_GBIF <- reptile_GBIF %>%
  dplyr::select(species, decimalLatitude, decimalLongitude, Level_01, Level_02,
                Level_03) %>%
  collect()

reptile_GBIF <- reptile_GBIF %>%
  rename(sciname = "species")

# TAKE A SUBSET (TO BE REMOVED)
#reptile_GBIF <- reptile_GBIF %>%
  #filter(sciname %in% (distinct(., sciname) %>% slice_sample(n = 50) %>% pull(sciname)))

# Fill empty Level_03 by the Level_02 or Level_01
reptile_GBIF <- reptile_GBIF %>%
  mutate(
    Level_03 = coalesce(Level_03, Level_02, Level_01),
    Level_02 = coalesce(Level_02, Level_01))

##---------------------------------------
# 5.2. Standardize species names ----
##---------------------------------------
message("5.2. Standardize species names")

# Here, we use the function rgbif::name_backbone_checklist to standardize both GBIF and literature with the same procedure
# The function standardize.species.names() follow the following procedure:
#   1. for both gbif and literature species list, it return a dataframe with original names and corrected names
#   2. in both dataset, it removes the species names flagged as unsufficiently accurate
#   3. in both dataset, it replaces the original species names by the "true ones" from the rgbif function
#   4. then it join both dataset, keeping in the gbif dataset only species found in the literature

# The return is the gbif cleaned version, with standardized species names and only species found in our literature dataset

species_names <- standardize.species.names(reptile_GBIF, reptile_mountain)
GBIF_clean <- species_names$gbif_final

##---------------------------------------
# 5.3. Extract elevational limits ----
##---------------------------------------
# This function simply extract elevational limits from GBIF occurrences
#   1. extract the elevation for each occurrences based on latitude and longitude coordinates
#   2. group by species and mountain range (Level_03) and calculate the quantiles 0.05 and 0.95 to extract min and max elevational limits
message("5.3. Extract elevational limits")

reptiles_GBIF_elev <- extract.elevational.limits.GBIF(GBIF_clean, dem)

# A bit of cleaning
reptiles_GBIF_elev <- reptiles_GBIF_elev %>%
  rename(Mountain_range = "Level_03")

# Add GBIF elev to the base dataframe
reptile_dataframe <- reptile_dataframe %>%
  left_join(reptiles_GBIF_elev, by = c("sciname", "Mountain_range"))
 
##------------------------
# 5.4. Save data -----
##------------------------

# Save the file
writexl::write_xlsx(reptile_dataframe, paste0(source_path, "GMBA_project/files_processed/reptile_dataframe.xlsx"))

##----------------------------------------------------------
# ----- 6. Clean and sort for expert validation
##----------------------------------------------------------
message("6. Clean and sort for expert validation")

reptile_dataframe_experts <- reptile_dataframe %>%
  select(-c(overlap_area, overlap_pct, species_area)) %>%  # remove overlap info useless for experts
  mutate(
    presence_corrected = "",
    min_corrected = "",
    max_corrected = "",
    validated_elevation_data = "",
    confidence_assessment = "",
    reviewer_comments = ""
  )

# Write one file per mountain range to be sent to expert
outdir <- paste0(source_path, "GMBA_project/Outputs/Reptiles/")
if (!file.exists(outdir)) {
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
}

mountain_ranges <- unique(reptile_dataframe_experts$Mountain_range)

for (mr in mountain_ranges) {
  df_sub <- reptile_dataframe_experts %>%
    filter(Mountain_range == mr)
  
  # Clean the name for use as filename (remove special characters)
  clean_name <- gsub("[^a-zA-Z0-9_-]", "_", mr)
  
  write_xlsx(df_sub, paste0(outdir, clean_name, ".xlsx"))
}