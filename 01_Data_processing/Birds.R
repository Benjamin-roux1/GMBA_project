#------------------------------------------------------------------------------#
#  1. Source and check out data from BirdLife International
#-----------------------------------------------------------------------------#

# Data has been received from BirdLife International after formal application on the 09.05.2023 
# BirdLife International and Handbook of the Birds of the World (2022) Bird species distribution maps of the world. Version 2022.2. 
# Sourced from http://datazone.birdlife.org/species/requestdis.

#----------------------------------------------------------#
# 1. 1. Set up and load the data  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(readxl)
library(sf)

# Load configuration file
source(here::here("R/00_Config_file.R"))

birds_data <- sf::st_read(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife/BOTW.gdb"))

# set geometry false to investigate data a bit
birds_no_geom <- sf::st_set_geometry(birds_data, NULL)

#----------------------------------------------------------#
# Check out data  -----
#----------------------------------------------------------#

# get all the unique species names
unique_birds <- birds_no_geom |> 
  distinct(sci_name)

# filter birds with breeding range polygon
birds_breeding <- birds_no_geom |> 
  filter(seasonal==2)|> 
  distinct(sci_name)

birds_breeding_native <- birds_no_geom |> 
  filter(seasonal==2)|> 
  filter(origin==1)|> 
  filter(presence==1)|>
  distinct(sci_name)

# how many birds in BirdLife have a breeding range polygon
nrow(birds_breeding)/nrow(unique_birds)*100
# --> only 14 % 


# filter birds with breeding range polygon
birds_resident <- birds_no_geom |> 
  filter(seasonal==1)|> 
  distinct(sci_name)

# how many birds in BirdLife have a resident range polygon
nrow(birds_resident)/nrow(unique_birds)*100
# --> 94 % 

# I need to filter birds with breeding range, extant and native origin  
# filter birds with breeding range polygon
# some birds still have several ranges .. also from different sources 
# moved to Arcgis pro here to handle the dataset 
birds_resident_native <- birds_data |> 
  filter(seasonal==1)|> 
  filter(origin==1)|> 
  filter(presence==1)


birds_resident_native <- birds_no_geom |> 
  filter(seasonal==1)|> 
  filter(origin==1)|> 
  filter(presence==1)|>
  distinct(sci_name)


birds_breeding_native <- birds_no_geom |> 
  filter(seasonal==2)|> 
  filter(origin==1)|> 
  filter(presence==1)|>distinct(sci_name)



## Test 
aburria <- birds_data |> 
  filter(sci_name =="Aburria aburri")|> 
  filter(seasonal==1)|> 
  filter(origin==1)|> 
  filter(presence==1)

birds_shapes <- sf::st_read(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife/birdlife.shp"))

birds_shapes <- make_shapes_valid(birds_shapes)
length(unique(birds_no_geom$sci_nam))


#--------------------------------------------------------------------#
# Writing multipolygon ranges -----
#--------------------------------------------------------------------#

unprocessed_species <- character()
# keep species names which cannot be processed (with errors)
# 16 of 10383 unique species cannot be processed

# Function to merge ranges if species has more than one range
process_species <- function(species_name, data) {
  tryCatch({
    species_data <- data|> 
      filter(sci_nam == species_name) |> 
      summarise(sci_nam = first(sci_nam), 
                geometry = sf::st_union(geometry), do_union = FALSE) |> # merge ranges into multipolygon
      st_cast("MULTIPOLYGON")
    return(species_data)
  }, error = function(e) {
    # On error, print the message and return NULL
    message("Error processing species: ", species_name, "\nError message: ", e$message)
    unprocessed_species <<- c(unprocessed_species, species_name)
    return(NULL)
  })
}

# Unique species names
species_names <- unique(birds_shapes$sci_nam)

# Process each species and combine results
results <- do.call(rbind, lapply(species_names, process_species, data = birds_shapes))
results_sf <- st_as_sf(results)

# data frame for unprocessed species 
unprocessed_species_df <- data.frame(sci_nam = unprocessed_species)

# set geom 0 to handle data more easily
results_no_geom<- sf::st_set_geometry(results_sf, NULL)

# keep metadata for species 
birds_metadata <- birds_no_geom|>
  inner_join(results_no_geom, by = "sci_nam") |>
  add_count(sci_nam, name = "occurrences") 


occurrence_distribution <- birds_metadata |>
  group_by(occurrences) |>
  summarise(species_count = n_distinct(sci_nam), .groups = 'drop')

# plot the number of polygons per species
x11()
ggplot(occurrence_distribution, aes(x = occurrences, y = species_count)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  geom_text(aes(label = species_count),
            position = position_dodge(width = 0.9), vjust = -0.25, check_overlap = TRUE) +
  labs(x = "count of range polygons per species",
       y = "count of Species (log scale)",
       title = "Counts of range polygons per species") +
  theme_minimal()

sp2 <- birds_metadata |> filter(occurrences==2)

sitpyg <- results_sf |> filter(sci_nam =="Geranoaetus albicaudatus")
x11()
plot(sitpyg$geometry)

#---------------------#
# Write Results ----
#----------------------#

# the merged shapefile
sf::st_write(results_sf,paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife/birdlife_merge.shp"))




#----------------------------------------------------------#
# 2. Source Selected BirdLife Data
#----------------------------------------------------------#

# 

#----------------------------------------------------------#
# 2.1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(sf)

# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# 2.2. Load the range shapefiles  -----
#----------------------------------------------------------#

birds_shapes <- sf::st_read(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife/birdlife_merge.shp"))|>
  rename(sciname=sci_nam)

#------------------------------------------------------------------------#
#  Slice the data into 10 chunks to reduce computation time -----
#----------------------------------------------------------------------------#

# In total there are 10367 species to process
# I divided in 10 chunks: chunk_1 : 10
n <- nrow(birds_shapes)

# Calculate the size of each chunk
chunk_size <- ceiling(n / 10)

# Create a new column that assigns each row to a chunk
birds_shapes <- birds_shapes |>
  mutate(chunk = ((row_number() - 1) %/% chunk_size) + 1)

# split the data into a list of 10 sf data frames
sf_chunks <- split(birds_shapes, birds_shapes$chunk)

lapply(sf_chunks, nrow)

# Define the directory to save shapefiles
output_directory <- paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife")

# Loop through chunks and write it as shapefile
lapply(seq_along(sf_chunks), function(i) {
  chunk <- sf_chunks[[i]]
  # Construct the shapefile name
  shapefile_name <- paste0(output_directory, "/chunk_", i, ".shp")
  # Write the chunk to a shapefile
  st_write(chunk, shapefile_name, delete_layer = TRUE, quiet = TRUE)
})




#----------------------------------------------------------#
#  3. Overlap Birds ranges with GMBA shapefile
#----------------------------------------------------------#

# This script overlaps birds distribution ranges (BirdLife International) with GMBA mountain ranges (level 03) and alpine biome 
# In total there are 10367 species to process
# I divided in 10 chunks: chunk_1 : 10

#----------------------------------------------------------#
# 3.1. Set up  -----
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
# 3.2. Define the chunk name and load the data -----
#----------------------------------------------------------#

# Define the chunk
chunk_name <- "chunk_10" # Replace this with the the different chunks 

# specify file path 
file_path <- paste0(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife/", chunk_name, ".shp"))

# Load the shapefile
birds_shapes <- sf::st_read(file_path, options = "ENCODING=ISO-8859-1")|> 
  dplyr::rename(sciname = sci_nam)


# set geometry false to investigate data 
birds_no_geom <- sf::st_set_geometry(birds_shapes, NULL)


#----------------------------------------------------------#
# 3.2. Source gmba mountain and alpine biome shps   -----
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

# there shouldnt be any invalid shapes

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
results <- overlap_mountains_and_alpinebiome(birds_shapes, mountain_shapes, alpine_biome)

# Result is a list with two dataframes:
# processed contains all species that have succesfully been processed
# not processed contains species where an error occured

results_processed <- results$processed
results_not_processed <- results$not_processed

#-----------------------------------------------------------------------------
# 3.4. Remove all species which s distribution ranges overlap < 1% with GMBA range
#-----------------------------------------------------------------------------

results_filtered <- results_processed |> filter(overlap_percentage_mountain >= 1)


#-------------------------
# 3.5. Restructure dataframes
#-------------------------

# Join the  dataset with the intersection results
birds_final <- inner_join(birds_shapes, results_filtered[, c("sciname",
                                                             "Mountain_range",
                                                             "overlap_percentage_mountain",
                                                             "overlap_percentage_alpine","species_area")], 
                          by = "sciname")
# Write to a checklist
birds_checklist <- birds_final|>
  sf::st_set_geometry(NULL) |> # to remove the geometries for the checklist
  select(sciname,
         Mountain_range,
         species_area,
         overlap_percentage_mountain,
         overlap_percentage_alpine)


#------------------------------------------#
# 3.7. Save the data as checklist  -----
#------------------------------------------#

## The checklist:

# Define the path to your Excel file
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Checklist.xlsx")

# function to write the data to an excel file: each order is written to a seperate sheet
save_excel_sheet(file_path, chunk_name, birds_checklist)


#-------------------------------------------#
# 3.6. Save the data with geometries  -----
#-------------------------------------------#

# assign the order name to save it
assign(chunk_name, birds_final, envir = .GlobalEnv)

# this is the 
RUtilpol::save_latest_file(
  object_to_save =paste0(chunk_name),
  dir = paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/geom"),
  prefered_format = "rds",
  use_sha = TRUE) 




#----------------------------------------------------------#
#  4. Bind Elevations to Species 
#----------------------------------------------------------#

# This script binds elevation data to species names sourced from Global database of birds (quintero and Jetz, 2018)

#----------------------------------------------------------#
# 4.1. Set up  -----
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
# 4.2. Load data -----
#----------------------------------------------------------#

file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Checklist.xlsx")

# this binds the different sheets into one dataframe
Birds_Checklist <- readxl::excel_sheets(file_path) |>
  map_df(~read_excel_sheets(.x))

length(unique(Birds_Checklist$sciname))

#----------------------------------------------------------#
# 4.3. Load elevation data -----
#----------------------------------------------------------#
# Dataset by Quintero and Jetz
# mountain dataset has mountain IDs (need to be linked to elevations)
qu_j_mountain_data <- readxl::read_excel(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/processed/additional_data/Quintero_Jetz_Mountains.xlsx"))|>
  janitor::clean_names()

# this dataset has the elevations and mountain IDs
qu_j_elevations <- readxl::read_excel(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/processed/additional_data/Quintero_Jetz_elev_ranges_birds.xlsx"))|>
  janitor::clean_names()|> 
  left_join(qu_j_mountain_data |> # join the mountain names 
              select(mountain_range,mountain_id),by="mountain_id")|>
  select(-x7)|>
  rename(Mountain_range_Qu_J = mountain_range)|>
  rename(min_elevation = minimum_elevation)|>
  rename(max_elevation = maximum_elevation)|>
  rename(sciname = species)

#----------------------------------------------------------#
#  save data -----
#----------------------------------------------------------#

writexl::write_xlsx(qu_j_elevations,data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Elevations_Qu_J.xlsx")


#----------------------------------------------------------#
# check out data -----
#----------------------------------------------------------#

Elevation_data_Birds <- readxl::read_excel(paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Elevations_Qu_J.xlsx"))|> 
  group_by(sciname) |>
  summarize(
    min_elevation = round(mean(min_elevation, na.rm = TRUE), 0),
    max_elevation = round(mean(max_elevation, na.rm = TRUE), 0))

#----------------------------------------------------------#
# investigate data availability --
#----------------------------------------------------------#

Birds_Elevations <- Birds_Checklist|> 
  left_join(Elevation_data_Birds,by = "sciname")|> 
  arrange(sciname)

GMBA_names_level_03 <- readRDS(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_names_level_03_04.rds"))|>
  filter(Hier_Lvl =="3")|>
  group_by(Mountain_range) |>
  summarise(gmba_ID = first(gmba_ID), 
            Mountain_system = first(Mountain_system))


Birds_Elevations <- Birds_Elevations |>
  left_join(GMBA_names_level_03_unique, by = "Mountain_range")|>
  select(sciname, gmba_ID, Mountain_system, 
         Mountain_range, species_area, overlap_percentage_mountain, 
         overlap_percentage_alpine, min_elevation, max_elevation)



writexl::write_xlsx(Birds_Elevations,data_storage_path, "subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Checklist_Elevations_Qu_J.xlsx")

#----------------------------------------------------------#
# visualize missing data --
#----------------------------------------------------------#
vismis <- Birds_Elevations |>
  select(sciname,min_elevation)|>
  group_by(sciname)|>
  summarise(min_elevation = mean(min_elevation, na.rm = FALSE)) |>
  rename("Missing Elevation Data" = min_elevation)|>
  select("Missing Elevation Data")


# Using vis_miss to visualize missing data
x11()
vis_miss(vismis) +
  ggtitle("Birlife Data with elevations from Quintero Jetz") +
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none",
                                                      axis.ticks = element_blank())



#----------------------------------------------------------#
#  5. Get elevations with DEM 
#----------------------------------------------------------#

# In this script I extract quartiles for species min and max elevations from their range shps using DEM

#----------------------------------------------------------#
# 5.1. Set up  -----
#----------------------------------------------------------#
library(here)
library(sf)
library(tidyverse)
library(data.table)
library(openxlsx)


# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# 5.2. Load species data and set API key  -----
#----------------------------------------------------------#

# Read the checklist that includes the elevation data
Checklist_Elev <- readxl::read_xlsx(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/processed/Birds_Elevations_Qu_J.xlsx"))

# load shapefile
file_path <- paste0(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Birds/BirdLife/birdlife_merge.shp"))

# 
birds_shapes <- sf::st_read(file_path, options = "ENCODING=ISO-8859-1")|> 
  dplyr::rename(sciname = sci_nam)|> 
  select(sciname,geometry)

# insert API for elevatr package
# https://cran.r-project.org/web/packages/elevatr/elevatr.pdf
topo_key <-"" #insert you API key
elevatr::set_opentopo_key(topo_key)

#------------------------------#
# 5.2. Load the mountains  -----
#------------------------------#

#source the gmba regions whith alpine biome
mountain_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_Mountains_Input.shp", 
                                     sep = "/"))|>
  rename(Mountain_system = Mntn_sy)|> 
  rename(Mountain_range = Mntn_rn)

# check if there are any invalid shapes
mountain_shapes <- make_shapes_valid(mountain_shapes) 

#-----------------------------------------------------------#
# 5.3. Define chunks to reduce computation time   -----
#------------------------------------------------------------#

# merge the geometries to the checklist
Checklist_Elev_DEM_merge <- merge(Checklist_Elev, birds_shapes, by = c("sciname"), all.x = TRUE)

#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[1:500,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[501:1000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[1001:1500]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[1501:3500,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[3501:7000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[7001:10001,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[10001:13000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[13001:16000,]
Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[16001:19617,]
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
