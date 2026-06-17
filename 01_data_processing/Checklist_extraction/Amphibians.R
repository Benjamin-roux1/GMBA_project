##-------------------------------------------------------------
#  -----  1. Source Amphibians distribution Data from IUCN
##-------------------------------------------------------------

# shapefiles can be downloaded from The IUCN Red List of Threatened Species, version 6.3. https://www.iucnredlist.org/resources/spatial-data-download

##-----------------------
#  1.1. Set up -----
##-----------------------
library(here); library(data.table); library(dplyr)
library(tidyverse); library(readxl); library(terra)
library(sf); library(arrow); library(rgbif); library(writexl)
library(exactextractr); library(furrr)

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

##------------------------------------------
# 1.2. Load the range shapefiles  -----
##------------------------------------------
# shp are divided in 2 parts
# we filter the columns before downloading to simplify the dataframe
# we only keep presence = 1, meaning that the species occur here, origin = 1, 2, ie native or reintroduced
# and seasonality = 1, ie resident (exclude breeding and non breeding season)

#geometry is loaded directly from st_read so no need to add in the query
amphibians_shapes01 <- sf::st_read(paste0(source_path, "GMBA_project/Raw_datasets/Amphibians/AMPHIBIANS/AMPHIBIANS_PART1.shp"),
                                   query = "SELECT sci_name, seasonal, family, genus, SHAPE_Area 
                                   FROM AMPHIBIANS_PART1
                                   WHERE presence = 1 AND origin IN (1, 2)")

amphibians_shapes02 <- sf::st_read(paste0(source_path, "GMBA_project/Raw_datasets/Amphibians/AMPHIBIANS/AMPHIBIANS_PART2.shp"),
                                   query = "SELECT sci_name, seasonal, family, genus, SHAPE_Area 
                                   FROM AMPHIBIANS_PART2
                                   WHERE presence = 1 AND origin IN (1, 2)")

# combine the two dataframes
amphibians_shapes <- bind_rows(amphibians_shapes01, amphibians_shapes02) %>%
  st_make_valid()

# remove to save space
rm(amphibians_shapes01, amphibians_shapes02)
gc()

# a bit of cleaning
amphibians_shapes <- amphibians_shapes %>%
  rename(sciname = "sci_name")

##------------------------------------
#  1.3. Explore the dataset -----
##------------------------------------
#amphibians_test <- amphibians_shapes %>%
 # st_drop_geometry()
# We test for duplicates
#amphibians_test %>%
 # group_by(sciname) %>%
  #summarise(n = n()) %>%
  #filter(n>1)
# We have duplicated species, so let's first union the species
# ------------------

#### subset for code testing (TO BE REMOVED)
#amphibians_shapes <- amphibians_shapes[sample(nrow(amphibians_shapes), 100), ]
####

##-------------------------------------------
#  1.4. Checking duplicated species -----
##-------------------------------------------

duplicated_species <- amphibians_shapes %>%
  st_drop_geometry() %>%
  group_by(sciname) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  pull(sciname) %>%
  unique()

# Process each species and combine results
results <- union.ranges(duplicated_species, amphibians_shapes)

amphibians_shapes_clean <- results

rm(amphibians_shapes, results)

##----------------------------------------------------------------
#  ------ 2. Overlap Amphibians ranges with GMBA shapefile
##----------------------------------------------------------------

# This script overlaps Amphibians distribution ranges with GMBA mountain ranges (level 03) 

##-------------------------------------
# 2.1. Source gmba mountains -----
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
    summarise(geometry = st_union(geometry),
              .groups = "drop") %>%
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

sf::st_write(mountain_shapes03,
             paste0(source_path, "GMBA_project/GMBA_mountains/mountain_shapes03/mountain_shapes03.shp"))

##---------------------------------------------------------------------------------------
# 2.2. Intersect species ranges with GMBA and calculate overlap (value in km2 and %) 
##---------------------------------------------------------------------------------------

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
results <- overlap.mountain(mountain_shapes03, amphibians_shapes_clean)

# Result is a list with two dataframes:
# results_df contains all species that have succesfully been processed
# failures_df contains species where an error occured

results_success <- results$results_df
results_failures <- results$failures_df

# Let's create a base dataframe in which we will add the different columns throughout the process
amphibians_dataframe <- results_success

rm(results, results_success)
##----------------------------------------------------------
#  ----- 3. Bind Elevations to Species 
##----------------------------------------------------------

# This script binds elevation data to species names.
# Elevation data have been extracted from the supplementary material of the following paper:
# Guirguis et al. (2023). Risk of extinction increases towards higher elevations across the world's amphibians. 
# Global Ecology and Biogeography, 32, 1954–1963. https://doi.org/10.1111/geb.13746

##---------------------------
# 3.1. Load data -----
##---------------------------
# Load the elevation data
elevation_data <- read_excel(paste0(source_path, "GMBA_project/Raw_datasets/Amphibians/elevation/geb13746-sup-0001-tables1.xlsx")) %>%
  select("scientific_name", "elev_mid", "elev_range") %>%
  rename(sciname = "scientific_name")

##-------------------------------
# 3.2. Clean the data -----
##-------------------------------
# We have 2 columns, elev_mid is the middle of the elevation gradient
# and elev range is the size of the elevation gradient
# so we can deduce min and max

# Change column type of elevation limits to numeric
elevation_data[, 2:3] <- lapply(elevation_data[, 2:3], as.numeric)

# remove the _ between genus and species names
elevation_data <- elevation_data %>% mutate(sciname = gsub("_", " ", sciname))

elevation_data <- elevation_data %>%
  mutate(min_elevation = elev_mid - (elev_range/2),
         max_elevation = elev_mid + (elev_range/2))

elevation_data <- elevation_data %>% select(-c(elev_mid, elev_range))

##-------------------------------
# 3.3. Left join data -----
##-------------------------------
# Add extracted range limits to our base dataframe
amphibians_dataframe <- amphibians_dataframe %>%
  left_join(elevation_data, by = "sciname") %>% 
  arrange(sciname)

##----------------------------------------------------------
#  ------- 4. Get elevations with DEM 
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

##-----------------------------------
# 4.1. Load species data  ------
##-----------------------------------

# From the dataframe with species selected for each mountain range, we add their range distribution as a new column
amphibians_mountain <- amphibians_dataframe %>%
  left_join(amphibians_shapes_clean)

##-------------------------------------------------------------
# 4.2. Crop species distribution in each mountain range  -----
##-------------------------------------------------------------
amphibians_mountain_sf <- st_as_sf(amphibians_mountain) %>%
  st_make_valid()

# Intersect for each row the species distribution with the corresponding mountain shp
sf_use_s2(FALSE)

amphibians_intersect <- amphibians_mountain_sf %>%
  st_make_valid() %>%
  left_join(
    mountain_shapes03 %>%
      select(-Level_01) %>%
      st_make_valid() %>%
      mutate(geom_mountain = geometry) %>%
      st_drop_geometry(),  # drop sf class, keep geom_mountain as column
    by = c("Mountain_range" = "Level_03", "Mountain_system" = "Level_02")
  ) %>%
  mutate(
    geometry = purrr::map2(
      geometry, geom_mountain,
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
  mutate(geometry = st_as_sfc(geometry, crs = st_crs(amphibians_mountain_sf))) %>%  # convert list to sfc
  select(-geom_mountain) %>%
  st_as_sf(sf_column_name = "geometry")

sf_use_s2(TRUE)

# Visual check of the cropping
sp <- amphibians_intersect[1, ]
bbox <- st_bbox(amphibians_mountain_sf %>% filter(sciname == sp$sciname))
ggplot() +
  geom_sf(data = mountain_shapes03, color = "black") +
  geom_sf(data = amphibians_mountain_sf %>% filter(sciname == sp$sciname),  # Whole species range
          fill = "lightblue", alpha = 0.6) + 
  geom_sf(data = amphibians_intersect %>% filter(sciname == sp$sciname),  # Intersected species range
          fill = "red", alpha = 0.4) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme.perso()

# Now we have a dataframe with all the species and their distribution in each mountain ranges specifically

rm(amphibians_mountain_sf)
gc()

##----------------------------
# 4.3. Add the DEM  -----
##----------------------------
dem <- terra::rast(paste0(source_path, "GMBA_project/demMountains_GLO90.tif"))

##----------------------------------------
# 4.4. Estimate the best quantile  -----
##----------------------------------------
overlap_treshold <- 20
quantiles <- estimate.quantile(amphibians_intersect, dem, overlap_treshold)

ggplot(quantiles, aes(x = quantile)) +
  geom_col(aes(y = mean_dev_min, fill = "red", alpha = 0.5)) + 
  geom_col(aes(y = mean_dev_max, fill = "blue", alpha = 0.5)) +
  theme_minimal()

##-------------------------------------------------------
# 4.5. Get amphibians elevational ranges with DEM -----
##-------------------------------------------------------

quantile_min <- quantiles %>%
  filter(quantile <= 0.49) %>%
  filter(mean_dev_min == min(mean_dev_min))
quantile_min
quantile_max <- quantiles %>%
  filter(quantile >= 0.51) %>%
  filter(mean_dev_max == min(mean_dev_max))
quantile_max

amphibians_elevations_DEM <- extract.elevational.limits.DEM(amphibians_intersect, dem, quantile_min, quantile_max)

amphibians_dataframe <- amphibians_dataframe %>%
  left_join(amphibians_elevations_DEM, by = c("sciname", "Mountain_range"))

##------------------------------------------------------------------------
# ------ 5. Get amphibians elevational ranges with GBIF
##------------------------------------------------------------------------
# This snippet extract the min and max elevational limits of each species in each mountain range
# I use the Digital Elevation Model Copernicus GLO-90, with a resolution of 90m
# https://portal.opentopography.org/raster?opentopoID=OTSDEM.032021.4326.1
# European Space Agency (2024). Copernicus Global Digital Elevation Model. Distributed by OpenTopography. https://doi.org/10.5069/G9028PQB.


##---------------------------------------
# 5.1. Import & clean GBIF dataset ----
##---------------------------------------
amphibians_GBIF <- arrow::open_dataset(paste0(source_path, "GBIF_data/data/Amphibia_parquetclean"))

# Collect Parquet dataset to R
amphibians_GBIF <- amphibians_GBIF %>%
  dplyr::select(species, decimalLatitude, decimalLongitude, Level_01, Level_02,
                Level_03) %>%
  collect()

amphibians_GBIF <- amphibians_GBIF %>%
  rename(sciname = "species")

# TAKE A SUBSET (TO BE REMOVED)
#amphibians_GBIF <- amphibians_GBIF %>%
  #filter(sciname %in% (distinct(., sciname) %>% slice_sample(n = 50) %>% pull(sciname)))

# Fill empty Level_03 by the Level_02 or Level_01
amphibians_GBIF <- amphibians_GBIF %>%
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

species_names <- standardize.species.names(amphibians_GBIF, amphibians_dataframe)
GBIF_clean <- species_names$gbif_final

# Update sciname in base dataframe
amphibians_dataframe <- amphibians_dataframe %>%
  left_join(species_names$name_mapping, 
            by = c("sciname" = "verbatim_name")) %>%
  mutate(sciname = ifelse(!is.na(canonicalName), canonicalName, sciname)) %>%
  select(-canonicalName)

##---------------------------------------
# 5.3. Extract elevational limits ----
##---------------------------------------
# This function simply extract elevational limits from GBIF occurrences
#   1. extract the elevation for each occurrences based on latitude and longitude coordinates
#   2. group by species and mountain range (Level_03) and calculate the quantiles 0.05 and 0.95 to extract min and max elevational limits

amphibians_GBIF_elev <- extract.elevational.limits.GBIF(GBIF_clean, dem)

# A bit of cleaning
amphibians_GBIF_elev <- amphibians_GBIF_elev %>%
  rename(Mountain_range = "Level_03")

# Add GBIF elev to the base dataframe
amphibians_dataframe <- amphibians_dataframe %>%
  left_join(amphibians_GBIF_elev, by = c("sciname", "Mountain_range"))

##-------------------------
# 5.4. Save data -----
##-------------------------

# Save the file
writexl::write_xlsx(amphibians_dataframe, paste0(source_path, "GMBA_project/files_processed/amphibians_dataframe.xlsx"))

##----------------------------------------------------------
# ----- 6. Clean and sort for expert validation
##----------------------------------------------------------

amphibians_dataframe_experts <- amphibians_dataframe %>%
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
outdir <- paste0(source_path, "GMBA_project/Outputs/Amphibians/")
if (!file.exists(outdir)) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
}

mountain_ranges <- unique(amphibians_dataframe_experts$Mountain_range)

for (mr in mountain_ranges) {
  df_sub <- amphibians_dataframe_experts %>%
    filter(Mountain_range == mr)
  
  # Clean the name for use as filename (remove special characters)
  clean_name <- gsub("[^a-zA-Z0-9_-]", "_", mr)
  
  write_xlsx(df_sub, paste0(outdir, clean_name, ".xlsx"))
}