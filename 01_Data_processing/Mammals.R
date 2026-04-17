# ---------------------------------------------------#
#  1. Source Mammal distribution Data from MDD
# ----------------------------------------------------#

# This script unpacks and opens zip folders containing gpkg for all mammals 
# zips can be downloaded via the Mammal Diversity Database: https://www.mammaldiversity.org/assets/data/MDD.zip

# There are some large files --> processing each order seperately

#----------------------------------------------------------#
# 1.1 Set up  -----
#----------------------------------------------------------#
library(here)

# Load configuration file
source(here::here("R/00_Config_file.R"))

order_name <-"Afrosoricida"
#----------------------------------------------------------#
# 1.2 Enzip folder containing range shapefiles  -----
#----------------------------------------------------------#

# Define the zip folder
zip_folder <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/MDD_zips/MDD_", order_name, ".zip")

# List files in the zip folder and extract the name of the geopackage file
zip_contents <- unzip(zip_folder, list = TRUE)
gpkg_file <- zip_contents$Name[grepl("\\.gpkg$", zip_contents$Name)][1]

# Unzip and read the geopackage file as an sf object
unzip(zip_folder, files = gpkg_file, exdir = tempdir())
mammals <- sf::st_read(file.path(tempdir(), gpkg_file), quiet = TRUE)


# These are the names of all orders: 
all_order_names <- c(
  "Afrosoricida",
  "Artiodactyla",
  "Carnivora",
  "Chiroptera",
  "Cingulata",
  "Dasyuromorphia",
  "Dermoptera",
  "Didelphimorphia",
  "Diprotodontia",
  "Eulipotyphla",
  "Hyracoidea",
  "Lagomorpha",
  "Macroscelidea",
  "Microbiotheria",
  "Monotremata",
  #"Notoryctemorphia", no overlapping species
  "Paucituberculata",
  "Peramelemorphia",
  "Perissodactyla",
  "Pholidota",
  "Pilosa",
  "Primates",
  "Proboscidea",
  "Rodentia",
  "Scandentia",
  "Sirenia",
  "Tubulidentata")


# ----------------------------------------------------------#
# 2. Overlap Mammal ranges with GMBA shapefile
# ----------------------------------------------------------#

# This script overlaps mammal distribution ranges with GMBA mountain ranges (level 03) and alpine biome 
# in the end the geometries of the species a checklist is saved with each order as seperate sheet. 

# ❗ The species range shps are partly very large files. Therefore, I process each order seperately

#----------------------------------------------------------#
# 2.1. Set up  -----
#----------------------------------------------------------#
library(here)
library(sf)
library(dplyr)
library(openxlsx)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 2.2. Define the order name  -----
#----------------------------------------------------------#

# Define the order name
order_name <- "Afrosoricida" # Replace this with the name of the order

# The range shps are stored in zip files. this script loads the range polygons for the defined order, unpacks and loads them. 
source(
  here::here("R/01_Data_processing/01_Mammals/00_source_mammal_data_MDD.R")
)

# These are all the orders that need to be processed
print(all_order_names)

#----------------------------------------------------------#
# 2.3 Source gmba mountain and alpine biome shps   -----
#----------------------------------------------------------#

#source the gmba regions whith alpine biome
mountain_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_Mountains_Input.shp", 
                                     sep = "/"))|>
  rename(Mountain_system = Mntn_sy)|> 
  rename(Mountain_range = Mntn_rn)


# source the alpine biome shapefile 
alpine_biome <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/alpine_biome.shp", sep = "/"))|>
  rename(Mountain_range = Mntn_rn)



mountain_shapes <- make_shapes_valid(mountain_shapes) 

alpine_biome <- make_shapes_valid(alpine_biome) 
#----------------------------------------------------------------------------------------#
# 2.4. Intersect species ranges with GMBA and Alpine Biome and calculate % of overlap -----
#-----------------------------------------------------------------------------------------#

# The function intersect_species_mountain ranges:
# 1. creates bboxes for mountain ranges and for the species that is beeing processed. 
# 2. If sp and mountain bbox intersect 
#   2.1. it calculates the area of a species in km2
#   2.2. the percentage of overlap of the species range with the mountain range 
#   2.3. the percentage of overlap with the alpine biome in that mountain range
# 3. removes all species with < 1% overlap with a GMBA Mountain range


results <- overlap_mountains_and_alpinebiome(mammals, mountain_shapes, alpine_biome)


# Result is a list with two dataframes:

# processed contains all species that have succesfully been processed
# in case an error occurs - species data is saved fur further debugging in the not processed dataframe 

results_processed <- results$processed
results_not_processed <- results$not_processed

length(unique(results_processed$sciname))
length(unique(results_not_processed$sciname))


#-----------------------------------------------------------------------------
# 2.5. Remove all species which s distribution ranges overlap < 1% with GMBA range
#-----------------------------------------------------------------------------

results_filtered <- results_processed |> 
  filter(overlap_percentage_mountain >= 1)


#-------------------------
# 2.6. Restructure dataframes
#-------------------------

# Join the  dataset with the intersection results
mammals_final <- inner_join(mammals, results_filtered[, c("sciname",
                                                          "Mountain_range",
                                                          "species_area",
                                                          "overlap_percentage_mountain",
                                                          "overlap_percentage_alpine")], 
                            by = "sciname")


# Write to a checklist
mammals_checklist <- mammals_final|>
  sf::st_set_geometry(NULL) |> # to remove the geometries for the checklist
  select(order,
         family,
         sciname,
         Mountain_range,
         species_area,
         overlap_percentage_mountain,
         overlap_percentage_alpine,
         author,
         year,
         citation,
         rec_source)


#-------------------------------------------#
# 2.7. Save the data with geometries  -----
#-------------------------------------------#

# assign the order name to save it
assign(order_name, mammals_final, envir = .GlobalEnv)

# this is the 
RUtilpol::save_latest_file(
  object_to_save =paste0(order_name),
  dir = paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/geom"),
  prefered_format = "rds",
  use_sha = TRUE) 

#------------------------------------------#
# 2.8. Save the data as checklist  -----
#------------------------------------------#

# Define the path to your Excel file
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/Mammals_Checklist.xlsx")

# function to write the data to an excel file: each order is written to a seperate sheet
save_excel_sheet(file_path, order_name, mammals_checklist)



#----------------------------------------------------------#
# 3. Bind Species Geometries
#----------------------------------------------------------#

# After 02_Overlap_Mountain ranges all species occuring in mountain ranges (overlap > 1% with GMBA) are saved as rds 
# This script creates a spatial df to combine the geometries for unique species which is needed for the next step: extracting elevational ranges

#----------------------------------------------------------#
# 3.1. Set up  -----
#----------------------------------------------------------#
library(data.table)
library(sf)

# these are all the order names
order_names <- c(
  "Afrosoricida",
  "Artiodactyla",
  "Carnivora",
  "Chiroptera",
  "Cingulata",
  "Dasyuromorphia",
  "Dermoptera",
  "Didelphimorphia",
  "Diprotodontia",
  "Eulipotyphla",
  "Hyracoidea",
  "Lagomorpha",
  "Macroscelidea",
  "Microbiotheria",
  "Monotremata",
  #"Notoryctemorphia", no overlapping species
  "Paucituberculata",
  "Peramelemorphia",
  "Perissodactyla",
  "Pholidota",
  "Pilosa",
  "Primates",
  "Proboscidea",
  "Rodentia",
  "Scandentia",
  "Sirenia",
  "Tubulidentata"
)


#----------------------------------------------------------#
# 3.2. Loop through the orders to get the geometry -----
#----------------------------------------------------------#

mammal_geometries_list <- list()

for (order_name in order_names) {
  
  # Load the latest mammal file
  mammals <- RUtilpol::get_latest_file(
    file_name = order_name,
    dir = paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/geom"))
  
  # Convert your dataframe to a data.table
  as.data.table(mammals)
  
  # Select the 'geom' and 'sciname' columns for unique 'sciname'
  mammals_geom <- mammals[, .(geom = first(geom)), by = sciname]
  
  # Store result in the list
  mammal_geometries_list[[order_name]] <- mammals_geom
  
  cat("Processing for order", order_name, "completed!\n")
}


mammal_geometries <- rbindlist(mammal_geometries_list, idcol = "order")

#----------------------------------------------------------#
# 3.3. Save the data -----
#----------------------------------------------------------#

RUtilpol::save_latest_file(
  object_to_save =mammal_geometries,
  dir = paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mammals/processed/geom"),
  prefered_format = "rds",
  use_sha = TRUE) 


# ------------- For individual orders or individual species -------------# 

# Define the order name 
order_name <- "Lagomorpha" # Replace with the order you want to read

# Load the latest mammal file
mammals<-RUtilpol::get_latest_file(
  file_name = paste0(order_name),
  dir = paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/geom"))

# Convert your dataframe to a data.table
mammals <- as.data.table(mammals)

# Select the 'geom' and 'sciname' columns for unique 'sciname'
order_geom <- mammals[, .(geom = first(geom)), by = sciname]
#marmota_geom <- mammals_dt[sciname == "Marmota marmota", .(geom = first(geom)), by = sciname]
#sf::st_write(marmota_geom,paste0(data_storage_path,"/Mammals/marmota.shp"))




#--------------------------------------------------------------#
#  4. Clean the data from Handbook of Mammals 
#--------------------------------------------------------------#

# Physical copies of Handbook of the Mammals of the World available at
# https://github.com/jhpoelen/hmw

# This script cleans textual information from the handbook of mammals to min and max elevational ranges for mammals. 

# ❗ ATTENTION !! the functions below do not clean HMW completely. there are still elevationa data that can not be grasped by the functions
# after running this script the cleaning of the output file has been finalized manually


#----------------------------------------------------------#
# 4.1. Set up  -----
#----------------------------------------------------------#
library(here)
library(stringr)
library(tidyverse)
library(purrr)
library(readxl)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)


#------------------------#
# 4.2. Download the data
#-------------------------#

# Loop to read each CSV file (for single files) 
#for (i in 1:9) {
#assign(paste0("hmw_v", i), 
#      read.csv(paste0("https://raw.githubusercontent.com/jhpoelen/hmw/main/hmw-volume-", i, ".csv")))
#}

# This is the single files combined
url <- "https://raw.githubusercontent.com/jhpoelen/hmw/main/hmw.csv"
hmw_data <- read.csv(url)

# Load The checklist
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/Mammals_Checklist.xlsx")


# bind sheets into one dataframe
MDD_checklist <- excel_sheets(file_path) |>
  map_df(~process_sheet(.x))


#------------------------------------------------------------------#
# 4.3. Filter for species in the checklist which are also in the HMW
#-------------------------------------------------------------------#

matched_species <- inner_join(MDD_checklist, hmw_data, by = c("sciname" = "name"))

# reduce the data 
reduced_hmw <- matched_species |> select(sciname, habitat)|> distinct(sciname,.keep_all = TRUE)



#-------------------------------------------------------#
# 4.4. first clean out the common typos
#-------------------------------------------------------#

# common patterns numbers
pattern <- "(\\w+\\s+){0,5}(\\d+\\s*\\-?\\s*\\d*\\s*m)(\\s+\\w+){0,5}"

# Clean common typos in the habitat column ¢
reduced_hmw <- reduced_hmw |>
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

# Step 1
# With this function I clean if there is a clear pattern of number 1 - number 2 m --> min and max elevation

extract_elevation <- function(elevation_info) {
  # Extract the pattern of two numbers separated by a hyphen
  pattern <- str_extract(elevation_info, "\\b\\d+-\\d+\\b")
  
  # Count the number of hyphens and numbers in the pattern
  hyphen_count <- str_count(elevation_info, "-")
  number_count <- str_count(elevation_info, "\\d+")
  
  # If there are exactly two numbers and one hyphen, extract the min and max elevations
  if (!is.na(pattern) && str_detect(elevation_info, pattern) && hyphen_count == 1 && number_count == 2) {
    elevations <- str_split(pattern, "-")[[1]]
    min_elevation <- as.numeric(elevations[1])
    max_elevation <- as.numeric(elevations[2])
  } else {
    min_elevation <- NA_real_ # using NA_real_ to ensure numeric NA
    max_elevation <- NA_real_ # using NA_real_ to ensure numeric NA
  }
  
  # Return a tibble (small data frame)
  return(tibble(min_elevation = min_elevation, max_elevation = max_elevation))
}

# Apply the function to the 'elevation_info' column and unnest the results
reduced_hmw_step1 <- reduced_hmw |>
  mutate(elevation_data = map(elevation_info, extract_elevation)) |>
  tidyr::unnest(cols = c(elevation_data)) # spread the nested data frame into separate columns


# Step 2
# Function to extract elevation based on keywords
extract_keyword_elevation <- function(text, keywords_max, keywords_min) {
  max_val <- NA_real_
  min_val <- NA_real_
  
  # Regular expression to match any characters between keywords and numbers, non-greedy
  regex_between <- ".*?"
  
  # Check if text contains any max keywords and extract the number that follows
  for (keyword in keywords_max) {
    if (str_detect(text, paste0("\\b", keyword, "\\b"))) {
      # Extract the first number that follows the keyword, possibly after other content
      number_after_keyword <- str_extract(text, paste0("(?<=\\b", keyword, "\\b)", regex_between, "\\d+"))
      if (!is.na(number_after_keyword)) {
        # Extract the numeric part from the result
        max_val <- as.numeric(str_extract(number_after_keyword, "\\d+"))
        break
      }
    }
  }
  
  # Check if text contains any min keywords and extract the number that follows
  for (keyword in keywords_min) {
    if (str_detect(text, paste0("\\b", keyword, "\\b"))) {
      # Extract the first number that follows the keyword, possibly after other content
      number_after_keyword <- str_extract(text, paste0("(?<=\\b", keyword, "\\b)", regex_between, "\\d+"))
      if (!is.na(number_after_keyword)) {
        # Extract the numeric part from the result
        min_val <- as.numeric(str_extract(number_after_keyword, "\\d+"))
        break
      }
    }
  }
  
  # Return a list with min and max values
  return(list(min_val = min_val, max_val = max_val))
}


# Define the keywords for max and min elevation
keywords_max <- c("over", "up to","below","as high as","to elevations of", "to at least","less than","not exceeding","upper elevations of","elevations that reach") 
keywords_min <- c("down to","above","greater than","higher than") 

# Apply the function to extract additional elevations
reduced_hmw_step_2 <- reduced_hmw_step1 |>
  mutate(
    keyword_elevation = map(elevation_info, extract_keyword_elevation, keywords_max = keywords_max, keywords_min = keywords_min),
    keyword_min = map_dbl(keyword_elevation, ~ .x$min_val),
    keyword_max = map_dbl(keyword_elevation, ~ .x$max_val)
  ) |>
  mutate(
    min_elevation = if_else(is.na(min_elevation) & !is.na(keyword_min), keyword_min, min_elevation),
    max_elevation = if_else(is.na(max_elevation) & !is.na(keyword_max), keyword_max, max_elevation)
  ) |>
  select(-keyword_elevation, -keyword_min, -keyword_max) # remove the intermediate columns


# Step 3 there are key words from Sea level to .. elevation 
extract_general_elevation <- function(text) {
  max_val <- NA_real_
  min_val <- NA_real_
  
  # Patterns for variations of 'sea level' followed by various phrases, then a number
  sea_level_patterns <- c(
    "sea\\s*level\\s*to\\s*(\\d+)",                   # pattern a
    "sea\\s*level\\s*to\\s*elevation(?:s)?\\s*of\\s*(\\d+)", # pattern b
    "sea\\s*level\\s*up\\s*to\\s*(\\d+)",              # pattern c
    "sea\\s*level\\s*to\\s*about\\s*(\\d+)",           # pattern d
    "sealevel\\s*to\\s*(\\d+)"                        # pattern for 'sealevel' variation
    # add more patterns here as needed
  )
  
  for (pattern in sea_level_patterns) {
    if (str_detect(text, regex(pattern, ignore_case = TRUE))) {
      matches <- str_match(text, regex(pattern, ignore_case = TRUE))
      min_val <- 0  # 'sea level' implies a starting elevation of 0
      if (!is.na(matches[1,2])) { # if there's a number following 'sea level'
        max_val <- as.numeric(matches[1,2]) # this should be the captured number, indicating the max elevation
      }
      return(list(min_val = min_val, max_val = max_val)) # return immediately if 'sea level' pattern was found
    }
  }
  
  # General pattern for number-to-number, applies if 'sea level' wasn't matched
  general_pattern <- "(\\d+)\\s+(?:.*?\\s+)?to(?:\\s+.*?\\s+)?(\\d+)"  # adjusted to potentially capture more varied text
  if (str_detect(text, regex(general_pattern, ignore_case = TRUE))) {
    matches <- str_match(text, regex(general_pattern, ignore_case = TRUE))
    min_val <- as.numeric(matches[1,2]) # this should be the first captured number
    max_val <- as.numeric(matches[1,3]) # this should be the second captured number
  }
  
  return(list(min_val = min_val, max_val = max_val))
}


reduced_hmw_step3 <- reduced_hmw_step_2 |>
  mutate(general_elevation = map(elevation_info, extract_general_elevation), # apply the new function
         general_min = map_dbl(general_elevation, ~ .x$min_val), # extract min_val from the list
         general_max = map_dbl(general_elevation, ~ .x$max_val)) |> # extract max_val from the list
  mutate(
    min_elevation = if_else(is.na(min_elevation) & !is.na(general_min), general_min, min_elevation), # update min_elevation if needed
    max_elevation = if_else(is.na(max_elevation) & !is.na(general_max), general_max, max_elevation) # update max_elevation if needed
  ) |>
  select(-general_elevation, -general_min, -general_max) # remove the temporary columns



### Clean out more common patterns
extract_at_elevation <- function(text) {
  min_val <- NA_real_
  max_val <- NA_real_
  
  # Define the patterns
  pattern_an_elevation <- "at\\s+an\\s+elevation\\s+of\\s+(\\d+)" # single number pattern with "an elevation of"
  pattern_up_to_sea_level <- "up\\s+to\\s+(\\d+)\\s+above\\s+sea\\s+level" # single number pattern with "up to" above sea level
  pattern_at_elevations_around <- "at\\s+elevations?\\s+around\\s+(\\d+)" # single number pattern with "around"
  pattern_between <- "between\\s+(\\d+)\\s*(?:m)?\\s+and\\s+(\\d+)\\s*(?:m)?" # range pattern: number and number, optionally followed by 'm'
  pattern_from_to <- "from\\s+(\\d+)\\s*m?\\s+to\\s+(\\d+)\\s*m?\\s*(elevation)?" # range pattern: number to number, optionally followed by 'm' and 'elevation'
  pattern_to_elevation <- "to\\s+an\\s+elevation\\s+of\\s+(\\d+)" # single number pattern with "to an elevation of"
  pattern_range <- "at\\s+elevations?\\s+of\\s+(\\d+)\\s*-\\s*(\\d+)" # range pattern: number - number
  pattern_single <- "at\\s+elevations?\\s+of\\s+(?:around\\s+)?(\\d+)" # single pattern: number or around number
  
  # Check for each pattern, starting with the new ones
  if (str_detect(text, regex(pattern_to_elevation, ignore_case = TRUE))) {
    matches <- str_match(text, pattern_to_elevation)
    max_val <- as.numeric(matches[1,2])
  } else if (str_detect(text, regex(pattern_an_elevation, ignore_case = TRUE))) {
    matches <- str_match(text, pattern_an_elevation)
    min_val <- as.numeric(matches[1,2])
  } else if (str_detect(text, regex(pattern_up_to_sea_level, ignore_case = TRUE))) {
    matches <- str_match(text, pattern_up_to_sea_level)
    max_val <- as.numeric(matches[1,2])
  } else if (str_detect(text, regex(pattern_at_elevations_around, ignore_case = TRUE))) {
    matches <- str_match(text, pattern_at_elevations_around)
    min_val <- as.numeric(matches[1,2])
  } else if (str_detect(text, regex(pattern_between, ignore_case = TRUE))) {
    matches <- str_match(text, pattern_between)
    min_val <- as.numeric(matches[1,2])
    max_val <- as.numeric(matches[1,3])
  } else if (str_detect(text, regex(pattern_from_to, ignore_case = TRUE))) {
    matches <- str_match(text, pattern_from_to)
    min_val <- as.numeric(matches[1,2])
    max_val <- as.numeric(matches[1,3])
  } else if (str_detect(text, regex(pattern_range, ignore_case = TRUE))) { # existing patterns
    matches <- str_match(text, pattern_range)
    min_val <- as.numeric(matches[1,2])
    max_val <- as.numeric(matches[1,3])
  } else if (str_detect(text, regex(pattern_single, ignore_case = TRUE))) { # existing patterns
    matches <- str_match(text, pattern_single)
    min_val <- as.numeric(matches[1,2])
  }
  
  return(list(min_val = min_val, max_val = max_val))
}

# 
reduced_hmw_step_4 <- reduced_hmw_step3 |>
  mutate(at_elevation = map(elevation_info, extract_at_elevation), 
         at_min = map_dbl(at_elevation, ~ .x$min_val), # extract min_val from the list
         at_max = map_dbl(at_elevation, ~ .x$max_val)) |> # extract max_val from the list
  mutate(
    min_elevation = if_else(is.na(min_elevation) & !is.na(at_min), at_min, min_elevation), # update min_elevation if needed
    max_elevation = if_else(is.na(max_elevation) & !is.na(at_max), at_max, max_elevation) # update max_elevation if needed
  ) |>
  select(-at_elevation, -at_min, -at_max) # remove the temporary columns



cleaned_hnw <- reduced_hmw_step_4|>mutate(alpine = ifelse(grepl("alpine", habitat, ignore.case = TRUE), "yes", "no"))|>
  mutate(min_elevation_regional = "",
         max_elevation_regional = "",
         regional_elevation_info = "")


#-------------------------------------#
# 4.6. Check which are still NA
#------------------------------------#

NA_elevation <- cleaned_hnw|>
  filter(is.na(min_elevation) & is.na(max_elevation))

#--------------------------------------------#
# 4.7. save file
#---------------------------------------------#


file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/HMW_cleaned.xlsx")

writexl::write_xlsx(cleaned_hnw, file_path)



# ------------ To filter for key words alpine, mountain, mountainous, elevation,...

#filter_words <- function(df, words) {
#pattern <- paste(words, collapse = "|")

#df |>
# rowwise() |>
#filter(if_any(everything(), ~str_detect(.x, regex(pattern, ignore_case = TRUE)))) |>
#ungroup()
#}

# here instead of filtering for alpine, mountain specific words use the checklist to filter for species once it is finalized
# Filter for words 
#words_to_filter <- c("alpine", "elevation","mountain","mountainous")


#filtered_hmw <- filter_words(hmw_data, words_to_filter)

# remove columns
#filtered_hmw <- filtered_hmw |>
# select(docId, name, habitat, activityPatterns, movementsHomeRangeAndSocialOrganization, statusAndConservation, verbatimText) |>
#filter(!is.na(name) & name != "") |>
#mutate(alpine = ifelse(grepl("alpine", habitat, ignore.case = TRUE), "yes", "no"))# add a column if "alpine" occurs

# reduce the data 
#reduced_hmw <- filtered_hmw |>
# select(name, habitat, alpine)




#---------------------------------------------------------------------------------#
#  5. Match the cleaned Handbook of Mammals Dataset with MDD checklist
#--------------------------------------------------------------------------------#

# This script loads the cleaned handbook of mammals and the checklist (with overlaps mountain ranges) 
# and joins the available data by species names

#----------------------------------------------------------#
# 5.1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(readxl)


# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#------------------------#
# 5.2. Load the data
#-------------------------#

# The elevations
HMW_elevations <- readxl::read_xlsx(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mammals/processed/HMW_cleaned.xlsx"))

# The checklist
file_path <- paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/Checklist_Mammals.xlsx")


# bind sheets into one dataframe
MDD_checklist <- excel_sheets(file_path) |>
  map_df(~process_sheet(.x))

#---------------------------------------#
# 5.3. Join the HMW data to the checklist
#---------------------------------------#

# Join the mountain system. below a list of mountain ranges with respective names at different lefels
mountain_shapes <- readRDS(file.path(data_storage_path, 
                                     "subm_global_alpine_biodiversity/Data/Mountains/GMBA_names_level_03_04.rds"))|>
  filter(Hier_Lvl=="3")


# left join the mountain system 
MDD_checklist <- MDD_checklist|>
  left_join(mountain_shapes|>
              select(Mountain_range, Mountain_system),by = "Mountain_range") |>
  reorder(Mountain_system,Mountain_range,order,family,sciname,species_area,overlap_percentage_mountain,overlap_percentage_alpine)


# join the HMW data by species
HMW_MDD_match <- left_join(MDD_checklist,HMW_elevations, by = "sciname")


#---------------------------------------#
# 5.4. Save the checklist
#---------------------------------------#

writexl::write_xlsx(HMW_MDD_match,paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mammals/processed/Checklist_Mammals_elevations_HMW.xlsx"))




#---------------------------------------------------------------------------------#
# 6. Explore availability of elevational data 
#--------------------------------------------------------------------------------#


#----------------------------------------------------------#
# 6.1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(readxl)


# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#------------------------#
# 6.2. Load data
#-------------------------#

MDD_checklist <- readxl::read_xlsx(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mammals/processed/Checklist_Mammals_elevations_HMW.xlsx"))

#------------------------#
# 6.3. Explore data
#-------------------------#

# Count in how many different mountain ranges and mountain systems a species occurs
species_mountain_count <- MDD_checklist |> 
  group_by(sciname) |>
  summarise(
    number_mountain_ranges = n_distinct(Mountain_range),
    number_mountain_systems = n_distinct(Mountain_system),
    .groups = 'drop'
  ) |>
  arrange(-number_mountain_ranges)


# How much elevational info are available per mountain range
percentage_elevation_range <- MDD_checklist |>
  # Create a helper column to flag available information
  mutate(min_elevation_info = ifelse(!is.na(min_elevation), 1, 0),
         max_elevation_info = ifelse(!is.na(max_elevation), 1, 0),
         both_elevation_info = ifelse(!is.na(min_elevation) & !is.na(max_elevation), 1, 0)) |>
  # Group by range and calculate percentage
  group_by(Mountain_range) |>
  summarise(total_species = n_distinct(sciname),
            
            species_with_min_elevation_info = sum(min_elevation_info, na.rm = TRUE),
            percentage_min_elevation = round((species_with_min_elevation_info / total_species) * 100,2),
            
            species_with_max_elevation_info = sum(max_elevation_info, na.rm = TRUE),
            percentage_max_elevation = round((species_with_max_elevation_info / total_species) * 100,2),
            
            species_with_both_elevation_info = sum(both_elevation_info, na.rm = TRUE),
            percentage_both_elevation = round((species_with_both_elevation_info / total_species) * 100,2),
            
            .groups = 'drop')|>
  select(total_species,Mountain_range,percentage_min_elevation,percentage_max_elevation,percentage_both_elevation)


# How much elevational info are available per mountain system
percentage_elevation_system <- MDD_checklist |>
  # Create a helper column to flag available information
  mutate(min_elevation_info = ifelse(!is.na(min_elevation), 1, 0),
         max_elevation_info = ifelse(!is.na(max_elevation), 1, 0),
         both_elevation_info = ifelse(!is.na(min_elevation) & !is.na(max_elevation), 1, 0)) |>
  
  # Group by Mountain_system and calculate percentage
  group_by(Mountain_system) |>
  summarise(total_species = n_distinct(sciname),
            
            species_with_min_elevation_info = sum(min_elevation_info, na.rm = TRUE),
            percentage_min_elevation = round((species_with_min_elevation_info / total_species) * 100,2),
            
            species_with_max_elevation_info = sum(max_elevation_info, na.rm = TRUE),
            percentage_max_elevation = round((species_with_max_elevation_info / total_species) * 100,2),
            
            species_with_both_elevation_info = sum(both_elevation_info, na.rm = TRUE),
            percentage_both_elevation = round((species_with_both_elevation_info / total_species) * 100,2),
            
            .groups = 'drop')|>
  select(total_species,Mountain_system,percentage_min_elevation,percentage_max_elevation,percentage_both_elevation)


# Info in the habitat column 
percentage_habitat_info <- MDD_checklist |>
  # Create a helper column to flag available information
  mutate(habitat_info = ifelse(!is.na(habitat), 1, 0)) |>
  
  # Group by range and calculate percentage
  group_by(Mountain_range) |>
  summarise(total_species = n_distinct(sciname),
            species_with_habitat_info = sum(habitat_info, na.rm = TRUE),
            percentage_habitat = round((species_with_habitat_info / total_species) * 100, 2),
            .groups = 'drop') |>
  select(Mountain_range, total_species, percentage_habitat)




#----------------------------------------------------------#
# 7. Get elevations with DEM 
#----------------------------------------------------------#

# In this script I extract quartiles for species min and max elevations from their range shps using SRTMGL3
# Shuttle Radar Topography Mission (SRTM GL3) Global 90m
# https://portal.opentopography.org/raster?opentopoID=OTSRTM.042013.4326.1

#----------------------------------------------------------#
# 7.1. Set up  -----
#----------------------------------------------------------#
library(here)
library(sf)
library(tidyverse)
library(data.table)
library(openxlsx)


# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# 7.2. Load species data and set API key  -----
#----------------------------------------------------------#

# Read the checklist that includes the elevation data
Checklist_Elev <- readxl::read_xlsx(paste0(data_storage_path,"subm_global_alpine_biodiversity/Data/Mammals/processed/Checklist_Mammals_elevations_HMW.xlsx"))


topo_key <-"" #insert you API key
elevatr::set_opentopo_key(topo_key)

#------------------------------#
# 7.2. Load the mountains  -----
#------------------------------#

#source the gmba regions whith alpine biome
mountain_shapes <- sf::st_read(paste(data_storage_path,"subm_global_alpine_biodiversity/Data/Mountains/GMBA_Mountains_Input.shp", 
                                     sep = "/"))|>
  rename(Mountain_system = Mntn_sy)|> 
  rename(Mountain_range = Mntn_rn)

# check if there are any invalid shapes
mountain_shapes <- make_shapes_valid(mountain_shapes) 

#-----------------------------------------------------------#
# 7.3. Load the species geometries (distribution ranges)  -----
#------------------------------------------------------------#

# Load the latest mammal file
mammals_geom <- RUtilpol::get_latest_file(
  file_name = "mammal_geometries",
  dir = paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/geom/"))

# merge the geometries to the checklist
Checklist_Elev_DEM_merge <- merge(Checklist_Elev, 
                                  mammals_geom, by = c("sciname", "order"), all.x = TRUE)


#------------------------------------------------------------------------#
# 7.4. Get mammal elevational ranges with DEM -----
#-------------------------------------------------------------------------#

# The files are large, I therefore process them in chunks

#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[1:1000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[1001:2000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[2001:3000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[3001:4000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[4001:5000,]
#Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[5001:6000,]
Checklist_Elev_DEM <- Checklist_Elev_DEM_merge[6001:7397,]

Checklist_Elev_DEM <- Checklist_Elev_DEM |> 
  rename(geometry = geom)|>
  filter(Mountain_system != "East Siberian Mountains")

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

#---------------------------#
# 7.6. Save data -----
#--------------------------#

# 
RUtilpol::save_latest_file(
  object_to_save = results_dem_df_b,
  file_name = "Mammals_Checklist_Elevations_DEM_6001_7397",
  dir = paste0(data_storage_path, "subm_global_alpine_biodiversity/Data/Mammals/processed/DEM"),
  prefered_format = "rds",
  use_sha = TRUE) 


check_and_write_xlsx(results_dem_df_b, data_storage_path, "Mammals/Output/Checklist/Mammals_Checklist_Elevations_DEM_6001_7397.xlsx")




#-----------------------------------------------------------------------------------#
#  8. Data Preparation to visualize and analayse Mammals above the treeline
#------------------------------------------------------------------------------------#


# Load the elevation data
Data_mammals <- readxl::read_excel(paste0(data_storage_path, "Mammals/Output/Checklist/Checklist_Mammals_elevations_DEM_all_mountains.xlsx"))|>
  rename(min_elevation = min_elevation_validation,
         max_elevation = max_elevation_validation)

# check for duplicates
duplicates <- Data_mammals|>
  distinct(sciname, Mountain_range, Mountain_system, .keep_all = TRUE)

# 
#----------------------------------------------------------#
# 8.2. Create Conditions which elevations are used for mammals ---
#----------------------------------------------------------#
# If species occurs in one mountain system and has min and max elevation (MDD) - USE
# If species occurs in one mountain system and has only min OR max (GARD) - Use GARD and respective other DEM
# If species occurs in > one mountain system OR has NO min and max (GARD) USE DEM

Data_mammals <- Data_mammals |>
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
  select(-unique_mountain_systems)

#-----------------------------------------------------------------------------------------------------------------------------#
# 8.2. Mutate the treeline elevations and calculate how much min elevation is below the treeline 
#------------------------------------------------------------------------------------------------------------------------------#
Treeline_Elevations <- readxl::read_excel(file.path(data_storage_path, "subm_global_alpine_biodiversity/Data/Mountains/Treeline_Lapse_Rate_04_05.xlsx"))

# Join with treeline elevations
Data_mammals <- Data_mammals|>
  left_join(Treeline_Elevations,by =c("Mountain_range","Mountain_system")) |> 
  rename(Mean_elevation_treeline = Mean_elevation) |># calculate how many m of species min and max limit is above and below the treeline
  mutate(
    min_rel_treeline = min_elevation_USE - Mean_elevation_treeline,
    max_rel_treeline = max_elevation_USE - Mean_elevation_treeline
  )

# The column to use now is min/max elevation USE
species_richness_mammals <- Data_mammals |>
  group_by(Mountain_range) |>
  summarise(species_richness = n_distinct(sciname))

#--------------------------------------------------#
# 8.3. Mutate information about species endemism
#---------------------------------------------------#

Data_mammals <- Data_mammals |> 
  group_by(sciname)|> 
  mutate(unique_mountain_range = n_distinct(Mountain_range))|>
  ungroup()|>
  mutate(endemic = if_else(unique_mountain_range==1, "YES","NO"))




#----------------------------------------------------------#
# 9.1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(openxlsx)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 9.1. Load data -----
#----------------------------------------------------------#

# read in the cleaned experts list

expert_list_mammals <- readxl::read_excel(paste0(data_storage_path, "Biodiversity_combined/Expert_validation/experts_list_cleaned.xlsx"))|>filter(group=="mammals")

# Load the Data Preparation file 
source(
  here::here("R/01_Data_processing/01_Mammals/08_00_prep_mammal_data_expert_validation.R")
)

# read in the maximum elevation
max_elev <- readxl::read_excel(paste0(data_storage_path, "Mountains/Suzette_Alpine_biome/GMBA_mountains_max_elevation.xlsx")) 

#----------------------------------------------------------#
# add columns for sources, alpine, uncertainty etc., ---
#----------------------------------------------------------#

# mutate the source of the elevational ranges
Data_mammals_experts <- Data_mammals |>
  mutate(
    source_distribution_data = "Mammal Diversity Database (MDD)",
    source_reference = "Marsh et al., 2022. Expert range maps of global mammal distributions harmonised to three taxonomic authorities. Journal of Biogeography 49 (5): 979-992.",
    source_min_elevation = case_when(
      min_elevation_USE == min_elevation ~ "Handbook of the mammals of the world through Nathan Upham (HMW)",
      min_elevation_USE == min_elev_DEM ~ "extracted with DEM",
      TRUE ~ NA_character_ 
    ),
    source_max_elevation = case_when(
      max_elevation_USE == max_elevation ~ "Handbook of the mammals of the world through Nathan Upham (HMW)",
      max_elevation_USE == max_elev_DEM ~ "extracted with DEM",
      TRUE ~ NA_character_ 
    )
  ) |>
  left_join(max_elev,by="Mountain_range")|>
  mutate(Mean_elevation_treeline = round(Mean_elevation_treeline, 0)) |>
  select(
    sciname, order, family, 
    Mountain_system, Mountain_range,overlap_percentage_mountain,
    habitat,
    min_elevation_USE, max_elevation_USE,Mean_elevation_treeline,max_elevation_mountain_range,
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
         alpine_status ="",reviewer_comments = ""
  )

#-------------------------------------------------------------------#
# filter mountain ranges where we do have experts ---
#--------------------------------------------------------------------#

# Get unique mountain ranges from df_mountain_ranges
unique_mr <- expert_list_mammals |> 
  filter(!is.na(email))|>
  distinct(mountain_range) |> 
  pull(mountain_range)

unique_mr <- Data_mammals_experts |> 
  distinct(Mountain_range) |> 
  pull(Mountain_range)

# Subset Data_birds_experts based on unique mountain ranges
subset_mammals <- Data_mammals_experts |> 
  filter(Mountain_range %in% unique_mr)

#--------------------------------------------#
# subset checklists to these mountain ranges---
#---------------------------------------------#

# Loop through each unique mountain range and save as Excel files
for (range in unique_mr) {
  # Replace slashes and spaces with underscores in the mountain range name
  safe_range_name <- gsub("[ /]", "_", range)  # Replaces slashes and spaces with underscores
  
  # Subset
  subset_range <- Data_mammals_experts |>
    filter(Mountain_range == range)
  
  # 
  wb <- createWorkbook()
  addWorksheet(wb, "Mammals")
  writeData(wb, "Mammals", subset_range)
  
  # 
  file_path <- paste0(data_storage_path, "Biodiversity_combined/Expert_validation/Checklists/Mammals/All_Lists/Mammals_", safe_range_name, ".xlsx")
  
  # Save 
  saveWorkbook(wb, file_path, overwrite = TRUE)
}
