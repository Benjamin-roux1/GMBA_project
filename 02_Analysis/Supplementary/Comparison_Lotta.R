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
my_checklist <- readxl::read_xlsx(paste0(source_path, "GMBA_project/files_processed/Reptiles/reptile_dataframe.xlsx"))

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
    y = "Overlap %"
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

# --------------------
ggplot() +
  geom_sf(data = mountain_shapes03 %>% filter(Level_03 == "Albertine Rift Mountains"), fill = NA, color = "grey50") +
  geom_sf(data = reptiles_shapes %>% filter(sciname == "Gerrhosaurus nigrolineatus"), fill = "lightblue", alpha = 0.6) +
  theme_minimal()


