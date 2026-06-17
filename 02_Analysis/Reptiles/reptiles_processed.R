#-----------------------
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

# source all functions
list.files(path = paste0(source_path, "GMBA_project/Functions"), pattern = "*.R", full.names = TRUE) %>%
  purrr::walk(source)

# -----------------------------
# Load the dataset
reptiles <- read_xlsx(paste0(source_path, "GMBA_project/files_processed/Reptiles/reptiles_dataframe.xlsx"))
reptiles_GBIF <- reptiles %>%
  filter(!is.na(NumberOcc))

# source the gmba regions
# We source as usual but this time we keep low and high elevation for each MR + save a new shp file

# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
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
              Elev_Low = min(Elev_Low), 
              Elev_High = max(Elev_High),
              .groups = "drop") %>%
    st_make_valid()
  sf_use_s2(TRUE)
  }
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
# save the shp
sf::st_write(mountain_shapes03,
             paste0(source_path, "GMBA_project/GMBA_mountains/mountain_shapes03/mountain_shapes03.shp"))
# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------

# Source the GMBA level03 regions
mountain_shapes03 <- sf::st_read(paste0(source_path, "GMBA_project/GMBA_mountains/mountain_shapes03/mountain_shapes03.shp")) %>%
  st_make_valid()

# ----------------------------
# Investigate the range size
reptiles_GBIF <- reptiles_GBIF %>%
  mutate(range = max_elevation_GBIF - min_elevation_GBIF)

ggplot(reptiles_GBIF, aes(x=range)) +
  geom_density(fill = "blue4", alpha = 0.1, color = "black") +
  theme_minimal()

# group by mountain range
mountain_range <- reptiles_GBIF %>%
  group_by(Mountain_system) %>%
  count(name = "number of species")

reptiles_GBIF %>%
  filter(NumberOcc > 1) %>%
  group_by(Mountain_system) %>%
  filter(n_distinct(sciname) > 10) %>%
  ungroup() %>%
  ggplot(aes(x=range, fill = Mountain_system)) +
  geom_density(alpha = 0.1, color = "black") +
  facet_wrap(~Mountain_system) +
  theme_minimal()+
  theme(
    legend.position = "none"
  )
