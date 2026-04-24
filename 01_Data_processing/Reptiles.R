#----------------------------------------------------------#
#  1. Source Reptile distribution Data from GARD
#----------------------------------------------------------#

# zips can be downloaded via the GARD Global Assessment of Reptile Distribution v.1.7: http://www.gardinitiative.org/data.html

# global distributions of 10914 reptile species merged and collated from verious sources

#Roll et. al. 2017 The global distribution of tetrapods reveals a need for targeted reptile conservation. Nature Ecology & Evolution 1:1677-1682

#Caetano, et al. 2022. Automated assessment reveals that the extinction risk of reptiles is widely underestimated across space and phylogeny. PLoS Biology, 20(5): e3001544.

#----------------------------------------------------------#
# 1.1. Set up  -----
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
# 1.2. Load the range shapefiles  -----
#----------------------------------------------------------#
reptile_shapes <- sf::st_read(paste0(source_path, "GMBA_project/Raw_datasets/Reptiles/Distribution/doi_10_5061_dryad_9cnp5hqmb__v20220427/Gard_1_7_ranges.shp"), 
                              options = "ENCODING=ISO-8859-1") %>%
  st_make_valid()

#### subset for code testing (TO BE REMOVED)
reptile_shapes <- reptile_shapes[sample(nrow(reptile_shapes), 50), ]
####

reptile_shapes <- reptile_shapes %>%
  rename(sciname = binomial)

#----------------------------------------------------------#
#  2. Overlap Reptile ranges with GMBA shapefile
#----------------------------------------------------------#

# This script overlaps reptile distribution ranges with GMBA mountain ranges (level 03) and alpine biome 
# The species range shps are partly very large files. Therefore, I process each group separately
# There are 6 groups (see 00_Source_data_GARD)

#----------------------------------------------------------#
# 2.2. Source gmba mountain and alpine biome shps   -----
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

#----------------------------------------------------------------------------------------#
# 2.3. Intersect species ranges with GMBA and calculate overlap (value in km2 and %) 
#-----------------------------------------------------------------------------------------#

# The function intersect_species_mountain ranges:
# 1. creates bboxes for mountain ranges and for the species that is beeing processed. 
# 2. If sp and mountain bbox intersect 
#   2.1. it takes the area of a species in km2 (is already in reptile dataset)
#   2.2. the percentage of overlap of the species range with the mountain range 
# 3. removes all species with < 5km2 and < 1% overlap with a GMBA Mountain range


# To test function
#reptile_shapes_filtered <- reptile_shapes[1:10,]
#results_test <- overlap.mountain(mountain_shapes03, reptile_shapes_filtered)
# visual check
# subset your data
#sahara <- mountain_shapes03 %>% filter(Level_03 == "Sahara Ranges")
#rueppellii <- reptile_shapes %>% filter(binomial == "Ablepharus rueppellii")
#ggplot() +
  #geom_sf(data = sahara, fill = "orange", color = NA) +                   
  #geom_sf(data = rueppellii, fill = "steelblue", alpha = 0.5, color = NA) +     
  #theme_minimal()

# Execute the main function
results <- overlap.mountain(mountain_shapes03, reptile_shapes)

# Result is a list with two dataframes:
# results_df contains all species that have succesfully been processed
# failures_df contains species where an error occured

reptile_success <- results$results
results_failures <- results$failures

# Let's create a base dataframe in which we will add the different columns throughout the process
reptile_dataframe <- reptile_success

#----------------------------------------------------------#
#  3. Bind Elevations to Species 
#----------------------------------------------------------#

# This script binds elevation data to species names (GARD)
# elevation data has been obtained by Squambase, Meiri 2024
# https://onlinelibrary.wiley.com/doi/10.1111/geb.13812 

#----------------------------------------------------------#
# 3.2. Load data -----
#----------------------------------------------------------#

# Load the elevation data
elevation_data <- read_excel(paste0(source_path, "GMBA_project/Raw_datasets/Reptiles/Elevation/Supplementary_Table_S1_-_squamBase1.xlsx")) %>%
  select("Species name (Binomial)", "Minimum elevation (m)", "Maximum elevation (m)") %>%
  rename(sciname = "Species name (Binomial)") %>% 
  rename(min_elevation = "Minimum elevation (m)") %>%
  rename(max_elevation = "Maximum elevation (m)")

# Change column type of elevation limits to numeric
elevation_data[, 2:3] <- lapply(elevation_data[, 2:3], as.numeric)

#----------------------------------------------------------#
# 3.3. Left join data -----
#----------------------------------------------------------#

# Add extracted range limits to our base dataframe
reptile_dataframe <- reptile_dataframe %>%
  left_join(elevation_data, by = "sciname") %>% 
  arrange(sciname)

# A bit of cleaning. Remove negative elevation data (set to 0)
reptile_dataframe <- reptile_dataframe %>%
  mutate(min_elevation = ifelse(min_elevation < 0, 0, min_elevation))


#----------------------------------------------------------#
#  4. Get elevations with DEM 
#----------------------------------------------------------#

# In this script I extract quartiles for species min and max elevations from their range shps using SRTMGL3
# Shuttle Radar Topography Mission (SRTM GL3) Global 90m
# https://portal.opentopography.org/raster?opentopoID=OTSRTM.042013.4326.1

#----------------------------------------------------------#
# 4.2. Load species data and set API key  -----
#----------------------------------------------------------#

# From the dataframe with species selected for each mountain range, we add their range distribution as a new column
reptile_mountain <- reptile_dataframe %>%
  left_join(reptile_shapes %>% select(sciname, geometry), by = "sciname")

#-------------------------------------------------------------#
# 4.2. Crop species distribution in each mountain range  -----
#-------------------------------------------------------------#
reptile_mountain_sf <- st_as_sf(reptile_mountain) %>%
  st_make_valid()

# Intersect for each row the species distribution with the corresponding mountain shp
{
  sf_use_s2(FALSE)
reptile_intersect <- reptile_mountain_sf %>%
  rowwise() %>%
  mutate(
    geometry = st_intersection(
      geometry,
      mountain_shapes03 %>% 
        filter(Level_03 == Mountain_range) %>% 
        st_geometry()
    )
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
  theme_perso()

# Now we have a dataframe with all the species and their distribution in each mountain ranges specifically

#------------------------------#
# 4.2. Add the DEM  -----
#------------------------------#
dem <- terra::rast(paste0(source_path, "GMBA_project/demMountains_GLO90.tif"))

#----------------------------------------#
# 4.2. Estimate the best quantile  -----
#----------------------------------------#

quantiles <- estimate.quantile(reptile_intersect, dem)

ggplot(quantiles, aes(x = quantile)) +
  geom_col(aes(y = mean_dev_min, fill = "red", alpha = 0.5)) + 
  geom_col(aes(y = mean_dev_max, fill = "blue", alpha = 0.5)) +
  theme_minimal()


#------------------------------------------------------------------------#
# 4.4. Get reptile elevational ranges with DEM -----
#-------------------------------------------------------------------------#

quantile_min <- quantiles %>%
  filter(quantile <= 0.49) %>%
  filter(mean_abs_dev == min(mean_abs_dev))
quantile_min
quantile_max <- quantiles %>%
  filter(quantile >= 0.51) %>%
  filter(mean_abs_dev == min(mean_abs_dev))
quantile_max

reptile_elevations_DEM <- extract.elevational.limits.DEM(reptile_intersect, dem, quantile_min, quantile_max)

reptile_dataframe <- reptile_dataframe %>%
  left_join(reptile_elevations_DEM, by = c("sciname", "Mountain_range"))

#------------------------------------------------------------------------#
# 4.4. Get reptile elevational ranges with GBIF -----
#-------------------------------------------------------------------------#

# Import GBIF dataset
reptile_GBIF <- arrow::open_dataset(paste0(source_path, "GBIF_data/data/Squamata_parquetclean"))

# Collect Parquet dataset to R
reptile_GBIF <- reptile_GBIF %>%
  dplyr::select(species, decimalLatitude, decimalLongitude, Level_01, Level_02,
                Level_03) %>%
  collect()

reptile_GBIF <- reptile_GBIF %>%
  rename(sciname = "species")

# TAKE A SUBSET (TO BE REMOVED)
reptile_GBIF <- reptile_GBIF %>%
  filter(sciname %in% (distinct(., sciname) %>% slice_sample(n = 50) %>% pull(sciname)))

# Fill empty Level_03 by the Level_02 or Level_01
reptile_GBIF <- reptile_GBIF %>%
  mutate(
    Level_03 = coalesce(Level_03, Level_02, Level_01),
    Level_02 = coalesce(Level_02, Level_01))

# ---- Standardize species names
species_names <- standardize.species.names(reptile_GBIF, reptile_mountain)
GBIF_clean <- species_names$gbif
reptile_clean <- species_names$litterature

reptiles_GBIF_elev <- extract.elevational.limits.GBIF(GBIF_clean, dem)

# A bit of cleaning
reptiles_GBIF_elev <- reptiles_GBIF_elev %>%
  rename(Mountain_range = "Level_03")

# Add GBIF elev to the base dataframe
reptile_dataframe <- reptile_dataframe %>%
  left_join(reptiles_GBIF_elev, by = c("sciname", "Mountain_range"))
 
#---------------------------#
# 4.6. Save data -----
#--------------------------#

# Save the file
writexl::write_xlsx(reptile_dataframe, paste0(source_path, "GMBA_project/files_processed/reptile_dataframe.xlsx"))

#----------------------------------------------------------#
# 6. Clean and sort for expert validation
#----------------------------------------------------------#

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
