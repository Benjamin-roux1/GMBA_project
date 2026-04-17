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
library(here)
library(tidyverse)
library(sf)

# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# 1.2. Load the range shapefiles  -----
#----------------------------------------------------------#
reptile_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Reptiles/GARD_2022/Gard_1_7_ranges.shp", sep = "/"),options = "ENCODING=ISO-8859-1")

reptile_shapes <- make_shapes_valid(reptile_shapes)

#----------------------------------------------------------#
# 1.3. Check out data  -----
#----------------------------------------------------------#
# Check out data structure
reptile_shapes_df <- as.data.frame(reptile_shapes)


reptile_shapes_df |> 
  group_by(group) |> 
  summarise(num_species = n_distinct(binomial))

# there are 6 groups of reptiles
# lizards are the biggest group with > 6000 species

#----------------------------------------------------------#
# 1.4. Split the data by group  -----
#----------------------------------------------------------#

# Split the data by 'group'
groups_list <- split(reptile_shapes, reptile_shapes$group)

# Loop through each group and save as a new shapefile
for (group_name in names(groups_list)) {
  # output file name
  output_file_name <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/GARD_2022/groups/", group_name, ".shp")
  
  # Write the shapefile
  sf::st_write(groups_list[[group_name]], output_file_name, delete_dsn = TRUE)
}




#----------------------------------------------------------#
#  2. Overlap Reptile ranges with GMBA shapefile
#----------------------------------------------------------#

# This script overlaps reptile distribution ranges with GMBA mountain ranges (level 03) and alpine biome 
# The species range shps are partly very large files. Therefore, I process each group seperately
# There are 6 groups (see 00_Source_data_GARD)

#----------------------------------------------------------#
# 2.1. Set up  -----
#----------------------------------------------------------#
library(here)
library(sf)
library(dplyr)
library(openxlsx)
library(furrr)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 2.2. Define the group name and load the data -----
#----------------------------------------------------------#
# These are the 6 groups

# Rhynchocephalia           
# amphisbaenian           
# croc
# lizard
# snake
# turtle

# Define the group name
group_name <- "lizard" # Replace this with the name of the group

# Construct the file path 
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/GARD_2022/groups/", group_name, ".shp")

# Load the shapefile
reptile_shapes <- sf::st_read(file_path, options = "ENCODING=ISO-8859-1")|> 
  dplyr::rename(sciname = binomial)


#----------------------------------------------------------#
# 2.2. Source gmba mountain and alpine biome shps   -----
#----------------------------------------------------------#

#source the gmba regions whith alpine biome
mountain_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_Mountains_Input.shp", 
                                     sep = "/"))|>
  rename(Mountain_system = Mntn_sy)|> 
  rename(Mountain_range = Mntn_rn)


# source the alpine biome 
alpine_biome <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/alpine_biome.shp", sep = "/"))|>
  rename(Mountain_range = Mntn_rn)

# check if there are any invalid shapes
mountain_shapes <- make_shapes_valid(mountain_shapes) 

alpine_biome <- make_shapes_valid(alpine_biome) 

#----------------------------------------------------------------------------------------#
# 2.3. Intersect species ranges with GMBA and Alpine Biome and calculate % of overlap -----
#-----------------------------------------------------------------------------------------#

# The function intersect_species_mountain ranges:
# 1. creates bboxes for mountain ranges and for the species that is beeing processed. 
# 2. If sp and mountain bbox intersect 
#   2.1. it takes the area of a species in km2 (is already in reptile dataset)
#   2.2. the percentage of overlap of the species range with the mountain range 
#   2.3. the percentage of overlap with the alpine biome in that mountain range
# 3. removes all species with < 1% overlap with a GMBA Mountain range


# To test function
#reptile_shapes_filtered <- reptile_shapes |>
#filter(sciname == "Amphisbaena camura" | sciname == "Amphisbaena pericensis")
#filter(sciname == "Amphisbaena pericensis")


# Execute the main function
results <- overlap_mountains_and_alpinebiome(reptile_shapes, mountain_shapes, alpine_biome)

# Result is a list with two dataframes:
# processed contains all species that have succesfully been processed
# not processed contains species where an error occured

results_processed <- results$processed
results_not_processed <- results$not_processed


#-----------------------------------------------------------------------------
# 2.4. Remove all species which s distribution ranges overlap < 1% with GMBA range
#-----------------------------------------------------------------------------

results_filtered <- results_processed |> filter(overlap_percentage_mountain >= 1)


#-------------------------
# 2.5. Restructure dataframes
#-------------------------

# Join the  dataset with the intersection results
reptiles_final <- inner_join(reptile_shapes, results_filtered[, c("sciname",
                                                                  "Mountain_range",
                                                                  "overlap_percentage_mountain",
                                                                  "overlap_percentage_alpine")], 
                             by = "sciname")
# Write to a checklist
reptiles_checklist <- reptiles_final|>
  sf::st_set_geometry(NULL) |> # to remove the geometries for the checklist
  select(TaxonID,
         group,
         family,
         sciname,
         Mountain_range,
         area,
         overlap_percentage_mountain,
         overlap_percentage_alpine)|> rename(species_area = area)

#-------------------------------------------#
# 2.6. Save the data with geometries  -----
#-------------------------------------------#

# assign the order name to save it
assign(group_name, reptiles_final, envir = .GlobalEnv)

# this is the 
RUtilpol::save_latest_file(
  object_to_save =paste0(group_name),
  dir = paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/processed/geom"),
  prefered_format = "rds",
  use_sha = TRUE) 

#------------------------------------------#
# 2.7. Save the data as checklist  -----
#------------------------------------------#

# Define the path to your Excel file
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/processed/Reptiles_Checklist.xlsx")

# function to write the data to an excel file: each order is written to a seperate sheet
save_excel_sheet(file_path, group_name, reptiles_checklist)



#----------------------------------------------------------#
#  3. Bind Elevations to Species 
#----------------------------------------------------------#

# This script binds elevation data to species names (GARD)
# elevation data has been obtained by Squambase, Meiri 2024
# https://onlinelibrary.wiley.com/doi/10.1111/geb.13812 

#----------------------------------------------------------#
# 3.1. Set up  -----
#----------------------------------------------------------#
library(here)
library(sf)
library(visdat)
library(tidyverse)
library(readxl)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 3.2. Load data -----
#----------------------------------------------------------#


file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/processed/Reptiles_Checklist.xlsx")

# this binds the different sheets into one dataframe
Reptile_Checklist <- readxl::excel_sheets(file_path) |>
  map_df(~read_excel_sheets(.x))



# Load the elevation data
Elevation_data_Reptiles <- read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/processed/additional_data/Meiri_2024_elevation_data.xlsx")) |> 
  rename(sciname = 'Species name (Binomial)')|>
  rename(min_elevation = "Minimum elevation (m)")|>
  rename(max_elevation = "Maximum elevation (m)")

#----------------------------------------------------------#
# 3.3. Left join data -----
#----------------------------------------------------------#

# Left join to see only the data where we have distribution data
Reptile_Elevations <- Reptile_Checklist|> left_join(Elevation_data_Reptiles,by = "sciname")|> 
  arrange(sciname)

GMBA_names_level_03 <- readRDS(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_names_level_03_04.rds"))|>
  filter(Hier_Lvl =="3")|>
  group_by(Mountain_range) |>
  summarise(gmba_ID = first(gmba_ID), 
            Mountain_system = first(Mountain_system))


Reptile_Elevations <- Reptile_Elevations |>
  left_join(GMBA_names_level_03, by = "Mountain_range")|>
  select(TaxonID, group, family, sciname, GMBA_ID, Mountain_system, 
         Mountain_range, species_area, overlap_percentage_mountain, 
         overlap_percentage_alpine, min_elevation, max_elevation)


#----------------------------------------------------------#
# 3.4. Save data -----
#----------------------------------------------------------#

writexl::write_xlsx(Reptile_Elevations, data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/processed/Reptiles_Checklist_Elevations.xlsx")




#----------------------------------------------------------#
#  4. Get elevations with DEM 
#----------------------------------------------------------#

# In this script I extract quartiles for species min and max elevations from their range shps using SRTMGL3
# Shuttle Radar Topography Mission (SRTM GL3) Global 90m
# https://portal.opentopography.org/raster?opentopoID=OTSRTM.042013.4326.1

#----------------------------------------------------------#
# 4.1. Set up  -----
#----------------------------------------------------------#
library(here)
library(sf)
library(tidyverse)
library(data.table)
library(openxlsx)


# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# 4.2. Load species data and set API key  -----
#----------------------------------------------------------#

# These are the 6 groups

# Rhynchocephalia           
# amphisbaenian           
# croc
# lizard
# snake
# turtle

# Define the group name
group_name <- "lizard" # Replace this with the name of the group

# Read the checklist that includes the elevation data
Checklist_Elev <- readxl::read_xlsx(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Reptiles/processed/Reptiles_Checklist_Elevations.xlsx"))|>
  filter(group==group_name)


# Load the shapefiles 
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/GARD_2022/groups/", group_name, ".shp")

# Load the shapefile
reptile_shapes <- sf::st_read(file_path, options = "ENCODING=ISO-8859-1")|> 
  dplyr::rename(sciname = binomial)

# insert API elvatr package
topo_key <-"" #insert you API key
elevatr::set_opentopo_key(topo_key)

#------------------------------#
# 4.2. Load the mountains  -----
#------------------------------#

#source the gmba regions whith alpine biome
mountain_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_Mountains_Input.shp", 
                                     sep = "/"))|>
  rename(Mountain_system = Mntn_sy)|> 
  rename(Mountain_range = Mntn_rn)

# check if there are any invalid shapes
mountain_shapes <- make_shapes_valid(mountain_shapes) 

#-----------------------------------------------------------#
# 4.3. Load the species geometries (distribution ranges)  -----
#------------------------------------------------------------#

# merge the geometries to the checklist
Checklist_Elev_DEM <- merge(Checklist_Elev, reptile_shapes, by = c("sciname"), all.x = TRUE)

#------------------------------------------------------------------------#
# 4.4. Get reptile elevational ranges with DEM -----
#-------------------------------------------------------------------------#

# Filter group to process
group <- Checklist_Elev_DEM |> 
  filter(group == group_name)|>
  distinct(sciname, Mountain_range, Mountain_system, .keep_all = TRUE)

# Define the focus GMBA systems 
Focus_GMBA_systems<-unique(group$Mountain_system)

# Validate the shapes in the df to process
group<- validate_shapes_individually(group)

# This is the old function from the mammal workflow --> new one has to be refined
results_dem_df <- extract_elevational_ranges(group, Focus_GMBA_systems)


# Bind the dataframes togeher
results_dem_df_b <- group|> 
  left_join(results_dem_df,by=c("sciname","Mountain_range","Mountain_system"))|>
  rename(max_elevation_validation = max_elevation)|>
  rename(min_elevation_validation = min_elevation)|>
  sf::st_as_sf(results_dem_df_b)|> 
  sf::st_set_geometry(NULL)

#-------------------------------#
# 4.5. Restructure Dataframes -----
#--------------------------------#

quantile_info <- results_dem_list$quantile_info
results_dem_df <- results_dem_list$results

results_dem_df <- test|> 
  left_join(results_dem_df,by=c("sciname","Mountain_range","Mountain_system"))|>
  select(-geometry)|>
  rename(max_elevation_validation = max_elevation)|>
  rename(min_elevation_validation = min_elevation)|>
  left_join(quantile_info,by=c("sciname","Mountain_range"))


#---------------------------#
# 4.6. Save data -----
#--------------------------#

# Define the dynamic file path
file_path <- file.path(data_storage_path, 
                       "subm_global_alpine_biodiversity/Data/Reptiles/processed/", 
                       paste0("Reptiles_Checklist_Elevations_DEM_", group_name, ".xlsx"))

# Save the file
writexl::write_xlsx(results_dem_df_b, file_path)





#-----------------------------------------------------------------------------------#
#  5. Data Preparation to visualize and analayse Reptiles above the treeline
#------------------------------------------------------------------------------------#


# Load the elevation data
Data_reptiles <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Reptiles/processed//Reptiles_Checklist_Elevations_DEM.xlsx")) |>
  rename(min_elevation = min_elevation_validation)|>
  rename(max_elevation = max_elevation_validation)

# check for duplicates
duplicates <- Data_reptiles|>
  distinct(sciname, Mountain_range, Mountain_system, .keep_all = TRUE)

# 
#----------------------------------------------------------#
# 5.2. Create Conditions which elevations are used for reptiles  ----
#----------------------------------------------------------#
# If species occurs in one mountain system and has min and max elevation (GARD) - USE
# If species occurs in one mountain system and has only min OR max (GARD) - Use GARD and respective other DEM
# If species occurs in > one mountain system OR has NO min and max (GARD) USE DEM

Data_reptiles <- Data_reptiles |>
  # Add a column that counts the number of unique mountain systems per species
  group_by(sciname) |>
  mutate(unique_mountain_systems = n_distinct(Mountain_system)) |>
  ungroup() |>
  # Apply conditions
  rowwise() |>
  mutate(
    min_elevation_USE = case_when(
      # If more than one mountain system, always use min_elev_DEM
      unique_mountain_systems > 1 ~ min_elev_DEM,
      # If only one mountain system and both elevations available
      unique_mountain_systems == 1 & !is.na(min_elevation) & !is.na(max_elevation) ~ min_elevation,
      # If only one mountain system and min available but max is not
      unique_mountain_systems == 1 & !is.na(min_elevation) & is.na(max_elevation) ~ min_elevation,
      # If only one mountain system and max available but min is not
      unique_mountain_systems == 1 & is.na(min_elevation) & !is.na(max_elevation) ~ min_elev_DEM,
      # Otherwise
      TRUE ~ min_elev_DEM
    ),
    max_elevation_USE = case_when(
      # If more than one mountain system, always use max_elev_DEM
      unique_mountain_systems > 1 ~ max_elev_DEM,
      # If only one mountain system and both elevations available
      unique_mountain_systems == 1 & !is.na(min_elevation) & !is.na(max_elevation) ~ max_elevation,
      # If only one mountain system and max available but min is not
      unique_mountain_systems == 1 & is.na(max_elevation) & !is.na(min_elevation) ~ max_elev_DEM,
      # If only one mountain system and min available but max is not
      unique_mountain_systems == 1 & !is.na(max_elevation) & is.na(min_elevation) ~ max_elevation,
      # Otherwise
      TRUE ~ max_elev_DEM
    )
  ) |>
  # drop the temporary columns
  select(-unique_mountain_systems)|>distinct()

#-----------------------------------------------------------------------------------------------------------------------------#
# 5.2. Mutate the treeline elevations and calculate how much min elevation is below the treeline 
#------------------------------------------------------------------------------------------------------------------------------#

Treeline_Elevations <- readxl::read_excel(file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/Treeline_Lapse_Rate_04_05.xlsx"))

# Join with treeline elevations
Data_reptiles <- Data_reptiles|>
  left_join(Treeline_Elevations,by = c("Mountain_range","Mountain_system"))|>
  rename(Mean_elevation_treeline = Mean_elevation) |># calculate how much of species min and max limit is above and below the treeline
  mutate(
    min_rel_treeline = min_elevation_USE - Mean_elevation_treeline,
    max_rel_treeline = max_elevation_USE - Mean_elevation_treeline
  )

# The column to use now is min/max elevation USE
species_richness_reptiles <- Data_reptiles |>
  group_by(Mountain_range) |>
  summarise(species_richness = n_distinct(sciname))

#--------------------------------------------------#
# 5.3. Mutate information about species endemism
#---------------------------------------------------#

Data_reptiles <- Data_reptiles |> 
  group_by(sciname)|> 
  mutate(unique_mountain_range = n_distinct(Mountain_range))|>
  ungroup()|>
  mutate(endemic = if_else(unique_mountain_range==1, "YES","NO"))





#----------------------------------------------------------#
# 6. 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)


# run the Data Preparation file 
source(
  here::here("R/01_Data_processing/03_Reptiles/04_00_reptile_data_prep_for:expert_validation.R")
)

# read in the maximum elevation
max_elev <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/GMBA_mountains_max_elevation.xlsx")) 

# mutate the source of the elevational ranges
Data_reptiles_experts <- Data_reptiles |>
  mutate(
    source_distribution_data = "Global Assessment of Reptile Distribution (GARD)",
    source_reference = "Roll et. al. 2017 The global distribution of tetrapods reveals a need for targeted reptile conservation. Nature Ecology & Evolution 1:1677-1682",
    source_min_elevation = case_when(
      min_elevation_USE == min_elevation ~ "provided by Shai Meiri,GARD",
      min_elevation_USE == min_elev_DEM ~ "extracted with DEM",
      TRUE ~ NA_character_ 
    ),
    source_max_elevation = case_when(
      max_elevation_USE == max_elevation ~ "provided by Shai Meiri, GARD",
      max_elevation_USE == max_elev_DEM ~ "extracted with DEM",
      TRUE ~ NA_character_ 
    )
  ) |>
  left_join(max_elev,by="Mountain_range")|>
  mutate(Mean_elevation_treeline = round(Mean_elevation_treeline, 0)) |>
  select(
    sciname, TaxonID, group, family, 
    GMBA_ID, Mountain_system, Mountain_range,overlap_percentage_mountain,
    min_elevation_USE, max_elevation_USE,
    Mean_elevation_treeline,max_elevation_mountain_range,
    source_distribution_data, source_reference, source_min_elevation, source_max_elevation)|>
  rename(
    min_elevation = min_elevation_USE,
    max_elevation = max_elevation_USE,
    overlap_perc_mountain_range = overlap_percentage_mountain,
    mean_elevation_treeline = Mean_elevation_treeline
  )|>
  arrange(Mountain_range, desc(max_elevation), desc(overlap_perc_mountain_range))|>
  mutate(
    reviewer_comments = "",
    reviewer_certainty = "",
    reviewer_alpine =""
  )




#----------------------------------------------------------#
# 7. 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 7.1. Load data -----
#----------------------------------------------------------#

# read in the cleaned experts list

expert_list_reptiles <- readxl::read_excel(paste0(data_storage_path, "Biodiversity_combined/Expert_validation/experts_list_cleaned.xlsx"))|>filter(group=="reptiles")


# Load the Data Preparation file 
source(
  here::here("R/02_Main_analyses/Reptiles/00_Reptile_Data_Preparations.R")
)

# read in the maximum elevation
max_elev <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/GMBA_mountains_max_elevation.xlsx")) 

# mutate the source of the elevational ranges
Data_reptiles_experts <- Data_reptiles |>
  mutate(
    source_distribution_data = "Global Assessment of Reptile Distribution (GARD)",
    source_reference = "Roll et. al. 2017 The global distribution of tetrapods reveals a need for targeted reptile conservation. Nature Ecology & Evolution 1:1677-1682",
    source_min_elevation = case_when(
      min_elevation_USE == min_elevation ~ "provided by Shai Meiri,GARD",
      min_elevation_USE == min_elev_DEM ~ "extracted with DEM",
      TRUE ~ NA_character_ 
    ),
    source_max_elevation = case_when(
      max_elevation_USE == max_elevation ~ "provided by Shai Meiri, GARD",
      max_elevation_USE == max_elev_DEM ~ "extracted with DEM",
      TRUE ~ NA_character_ 
    )
  ) |>
  left_join(max_elev,by="Mountain_range")|>
  mutate(Mean_elevation_treeline = round(Mean_elevation_treeline, 0)) |>
  select(
    sciname, TaxonID, group, family, 
    GMBA_ID, Mountain_system, Mountain_range,overlap_percentage_mountain,
    min_elevation_USE, max_elevation_USE,
    Mean_elevation_treeline,max_elevation_mountain_range,
    source_distribution_data, source_reference, source_min_elevation, source_max_elevation)|>
  rename(
    min_elevation = min_elevation_USE,
    max_elevation = max_elevation_USE,
    overlap_perc_mountain_range = overlap_percentage_mountain,
    mean_treeline = Mean_elevation_treeline
  )|>
  arrange(Mountain_range, desc(max_elevation), desc(overlap_perc_mountain_range))|>
  mutate(mountain_range_corrected ="",
         min_corrected = "",
         max_corrected = "",
         validated_elevation_data = "",
         confidence_assessment = "",
         alpine_status ="",
         reviewer_comments = ""
  )

#-------------------------------------------------------------------#
# filter mountain ranges where we do have experts ---
#--------------------------------------------------------------------#

# Get unique mountain ranges from df_mountain_ranges
unique_mr <- expert_list_reptiles |> 
  filter(!is.na(email))|>
  distinct(mountain_range) |> 
  pull(mountain_range)

# Subset Data_birds_experts based on unique mountain ranges
subset_reptiles <- Data_reptiles_experts |> 
  filter(Mountain_range %in% unique_mr)

## this part if you want to get the individual lists for all ranges (not only where we have experts for)
# Get unique mountain ranges from df_mountain_ranges

unique_mr <- Data_reptiles_experts |> 
  distinct(Mountain_range) |> 
  pull(Mountain_range)

# Subset Data_birds_experts based on unique mountain ranges
subset_reptiles <- Data_reptiles_experts |> 
  filter(Mountain_range %in% unique_mr)

#--------------------------------------------#
# subset checklists to these mountain ranges---
#---------------------------------------------#

# Loop through each unique mountain range and save as Excel files
for (range in unique_mr) {
  # Replace slashes and spaces with underscores in the mountain range name
  safe_range_name <- gsub("[ /]", "_", range)  # Replaces slashes and spaces with underscores
  
  # Subset
  subset_range <- Data_reptiles_experts |>
    filter(Mountain_range == range)
  
  # 
  wb <- createWorkbook()
  addWorksheet(wb, "Reptiles")
  writeData(wb, "Reptiles", subset_range)
  
  # 
  file_path <- paste0(data_storage_path, "Biodiversity_combined/Expert_validation/Checklists/Reptiles/all_lists/Reptiles_", safe_range_name, ".xlsx")
  
  # Save 
  saveWorkbook(wb, file_path, overwrite = TRUE)
}


