##-----------------------
#  1.1. Set up -----
##-----------------------
library(here); library(data.table); library(dplyr)
library(tidyverse); library(readxl); library(terra)
library(sf); library(arrow); library(rgbif); library(writexl)
library(exactextractr)

# Load configuration
#source(
#here::here("R/00_Config_file.R")
#)

# define data path OR even config.R file with libraries & path
source_path <- "C:/Users/berou1714/OneDrive - Norwegian University of Life Sciences/Desktop/PhD_project/"

# Import Lotta's checklist
Lotta_checklist <- readxl::read_xlsx(paste0(source_path, "Lotta_files/vertebrate_data/vertebrate_data_Benjamin.xlsx"))

# Import my checklist
my_checklist <- readxl::read_xlsx(paste0(source_path, "GMBA_project/files_processed/Reptiles/reptiles_dataframe.xlsx"))

# ----------------
# keep only reptiles in Lotta's checklist
Lotta_reptiles <- Lotta_checklist %>%
  filter(group == "reptiles")

# now keep only the mountain ranges in mine that are in Lotta's
mountains_list <- Lotta_reptiles %>%
  distinct(Mountain_range) %>%
  pull(Mountain_range)

my_checklist <- my_checklist %>%
  filter(Mountain_range %in% mountains_list)

# I have ~ 2000 more species
# let's investigate a bit more

Lotta_reptiles %>%
  filter(Mountain_range == "Albertine Rift Mountains") %>%
  count()
# 166 species here for Lotta
my_checklist %>%
  filter(Mountain_range == "Albertine Rift Mountains") %>%
  count()
# 310 species here for me

# Now let's see what are the species that I have and that Lotta don't
species_list <- Lotta_reptiles %>%
  filter(Mountain_range == "Albertine Rift Mountains") %>%
  distinct(sciname) %>%
  pull(sciname)

species_diff <- my_checklist %>%
  filter(Mountain_range == "Albertine Rift Mountains" & !sciname %in% species_list)

species_diff %>%
  filter(overlap_pct > 1) %>%
  count()

ggplot(species_diff, aes(x = reorder(sciname, -overlap_pct), y = overlap_pct)) +
  geom_col() +
  geom_hline(yintercept = 1, col = "red", linetype = "dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Species",
    y = "Overlap %",
    subtitle = "Species not in Lotta's dataset"
  )

# ---------------------------  
# I have now my species list, I will extract the range maps for these and investigate

reptiles_shapes <- sf::st_read(paste0(source_path, "GMBA_project/Raw_datasets/Reptiles/Distribution/doi_10_5061_dryad_9cnp5hqmb__v20220427/Gard_1_7_ranges.shp"), 
                               options = "ENCODING=ISO-8859-1") %>%
  st_make_valid()

reptiles_shapes <- reptiles_shapes %>%
  rename(sciname = binomial) %>%
  filter(sciname %in% species_diff$sciname)

#------------------
#source the gmba regions
mountain_shapes03 <- sf::st_read(paste0(source_path, "GMBA_project/GMBA_mountains/mountain_shapes03/mountain_shapes03.shp")) %>%
  st_make_valid()

# --------------------
ggplot() +
  geom_sf(data = mountain_shapes03 %>% filter(Level_03 == "Albertine Rift Mountains"), fill = NA, color = "grey50") +
  geom_sf(data = reptiles_shapes %>% filter(sciname == "Bitis arietans"), fill = "lightblue", alpha = 0.6) +
  theme_minimal()


