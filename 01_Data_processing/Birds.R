#------------------------------------------------------------------------------#
#  1. Source and check out data from BirdLife International
#-----------------------------------------------------------------------------#

# Data has been received from BirdLife International after formal application on the 09.05.2023 
# BirdLife International and Handbook of the Birds of the World (2022) Bird species distribution maps of the world. Version 2022.2. 
# Sourced from http://datazone.birdlife.org/species/requestdis.

#----------------------------------------------------------#
# 1. 1. Set up and load the data  -----
#----------------------------------------------------------#
library(here); library(data.table); library(dplyr)
library(tidyverse); library(readxl); library(terra)
library(sf); library(arrow); library(rgbif); library(gpkg)

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
# Load the data  -----
#----------------------------------------------------------#
geopac <- geopackage(paste0(source_path, "GMBA_project/Raw_datasets/Birds/species/BOTW_2025.gpkg"), connect = TRUE)
gpkg_list_tables(geopac)
gpkg_tbl(geopac, "all_species")


#----------------------------------------------------------#
# Explore the data  -----
#----------------------------------------------------------#

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
birds_shapes <- birds_shapes[sample(nrow(birds_shapes), 50), ]
############

birds_shapes <- birds_shapes %>%
  rename(sciname = "sci_name")

# Visual check
ggplot(birds_shapes) +
  geom_sf(data = birds_shapes[3,], fill = "lightblue") +
  theme_perso()

#--------------------------------------------------------------------#
#  Cropping duplicated species -----
#--------------------------------------------------------------------#

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

#----------------------------------------------------------#
#  3. Overlap Birds ranges with GMBA shapefile
#----------------------------------------------------------#

# This script overlaps birds distribution ranges (BirdLife International) with GMBA mountain ranges (level 03) and alpine biome 
# In total there are 10367 species to process
# I divided in 10 chunks: chunk_1 : 10

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
# 3.3. Intersect species ranges with GMBA and Alpine Biome and calculate % of overlap -----
#-----------------------------------------------------------------------------------------#

# The function intersect_species_mountain ranges:
# 1. creates bboxes for mountain ranges and for the species that is beeing processed. 
# 2. If sp and mountain bbox intersect 
#   2.1. it takes the area of a species in km2 (is already in reptile dataset)
#   2.2. the percentage of overlap of the species range with the mountain range 
#   2.3. the percentage of overlap with the alpine biome in that mountain range
# 3. removes all species with < 1% overlap with a GMBA Mountain range

# To test function
#birds_shapes_filtered <- birds_shapes |> 
#filter(sciname == "Ardea alba" | sciname == "Acrocephalus scirpaceus")

# Execute the main function
results <- overlap.mountain(mountain_shapes03, birds_shapes_clean)

# Result is a list with two dataframes:
# processed contains all species that have succesfully been processed
# not processed contains species where an error occured

birds_success <- results$results
birds_failures <- results$failures

# Let's create a base dataframe in which we will add the different columns throughout the process
birds_dataframe <- birds_success

#----------------------------------------------------------#
#  4. Bind Elevations to Species 
#----------------------------------------------------------#

# This script binds elevation data to species names sourced from Global database of birds (quintero and Jetz, 2018)

#----------------------------------------------------------#
# 4.2. Load data -----
#----------------------------------------------------------#

birds_elev_limits <- readxl::read_excel(paste0(source_path, "GMBA_project/Raw_datasets/Birds/elevation/birds_elevational_limits.xlsx"))
mountain_ID <- readxl::read_excel(paste0(source_path, "GMBA_project/Raw_datasets/Birds/elevation/mountain_range_ID.xlsx"))

# A bit of cleaning
birds_elev_limits <- birds_elev_limits %>% 
  select(-7) %>%
  rename(Mountain_ID = "Mountain ID")
mountain_ID <- mountain_ID %>%
  select(Mountain_ID, `Mountain Range`) %>%
  filter(!is.na(Mountain_ID) & !is.na(`Mountain Range`)) %>%
  rename(Mountain_system = "Mountain Range")

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

#----------------------------------------------------------#
#  5. Get elevations with DEM 
#----------------------------------------------------------#

# In this script I extract quartiles for species min and max elevations from their range shps using DEM


-------------------------------------------------#
# 5.2. Load species data and set API key  -----
#----------------------------------------------------------#


#------------------------------------------------------------------------#
# 5.4. Get birds elevational ranges with DEM -----
#-------------------------------------------------------------------------#

# Define the focus GMBA systems 
Focus_GMBA_systems<-unique(Checklist_Elev_DEM$Mountain_system)

# Validate the shapes in the df to process
Checklist_Elev_DEM<- validate_shapes_individually(Checklist_Elev_DEM)


# This is the old function from the mammal workflow --> new one has to be refined
results_dem_df <- extract_elevational_ranges(Checklist_Elev_DEM, Focus_GMBA_systems)


# Bind the dataframes togeher
results_dem_df_b <- Checklist_Elev_DEM|> 
  left_join(results_dem_df,by=c("sciname","Mountain_range","Mountain_system"))|>
  rename(max_elevation_validation = max_elevation)|>
  rename(min_elevation_validation = min_elevation)|>
  sf::st_as_sf(results_dem_df_b)|> 
  sf::st_set_geometry(NULL)

results_dem_df_b <- results_dem_df_b|> 
  sf::st_set_geometry(NULL)
#---------------------------#
# 5.6. Save data -----
#--------------------------#

# write the individual chunks
writexl::write_xlsx(results_dem_df_b, data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Checklist_Elevations_DEM_16001_19617.xlsx")
