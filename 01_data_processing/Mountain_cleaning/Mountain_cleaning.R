##-----------------------
#  1.1. Set up -----
##-----------------------
library(here); library(data.table); library(dplyr)
library(tidyverse); library(readxl); library(terra)
library(sf); library(arrow); library(rgbif); library(writexl)
library(exactextractr); library(ggExtra); library(patchwork)

# Load configuration
#source(
#here::here("R/00_Config_file.R")
#)

# define data path OR even config.R file with libraries & path
source_path <- "C:/Users/berou1714/OneDrive - Norwegian University of Life Sciences/Desktop/PhD_project/"

# source all functions
list.files(path = paste0(source_path, "GMBA_project/Functions"), pattern = "*.R", full.names = TRUE) %>%
  purrr::walk(source)

# ---------------------------
#source the gmba regions
mountain_shapes <- sf::st_read(paste0(source_path, "GMBA_project/GMBA_mountains/GMBA_Inventory_v2.0_standard_300/GMBA_Inventory_v2.0_standard_300.shp")) %>%
  st_make_valid()

mountain_shapes_broad <- sf::st_read(paste0(source_path, "GMBA_project/GMBA_mountains/GMBA_Inventory_v2.0_broad_300/GMBA_Inventory_v2.0_broad_300.shp")) %>%
  st_make_valid()

mountain_version <- mountain_shapes_broad

# Group by Level_03 (scale of mountain system chosen)
{
  sf_use_s2(FALSE)
  mountain_version03 <- mountain_version %>%
    st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
    st_make_valid() %>%
    group_by(Level_01, Level_02, Level_03) %>%
    summarise(geometry = st_union(geometry),
              Elev_Low = min(Elev_Low),
              Elev_High = max(Elev_High),
              .groups = "drop") %>%
    st_make_valid()
  sf_use_s2(TRUE)
# so we have 137 distinct combinations level_02 - level_03, i.e 137 mountain systems

# A bit of cleaning the dataframe
# some rows are not defined at Level03 or Level02
# in these cases, we fill the NA with the closest filled superior level
  sf_use_s2(FALSE)
  mountain_version03 <- mountain_version03 %>%
    st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
    st_make_valid() %>%
    mutate(
      Level_03 = coalesce(Level_03, Level_02, Level_01),
      Level_02 = coalesce(Level_02, Level_01)
    ) %>%
    st_make_valid()
  sf_use_s2(TRUE)
}

# --------------

mountain_shapes_broad03 <- mountain_version03
mountain_shapes03 <- sf::st_read(paste0(source_path, "GMBA_project/GMBA_mountains/mountain_shapes03/mountain_shapes03.shp")) %>%
  st_make_valid()

ggplot() +
  geom_sf(data = mountain_shapes_broad03 %>%
            filter(Level_03 == "Tibetan Plateau"), fill = "red") +
  theme_minimal()

