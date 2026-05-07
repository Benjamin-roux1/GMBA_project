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

reptiles <- read_xlsx(paste0(source_path, "GMBA_project/files_processed/Reptiles/reptiles_dataframe.xlsx"))

reptiles_GBIF <- reptiles %>%
  filter(!is.na(NumberOcc))

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
  filter(NumberOcc > 10) %>%
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

# ---------------------------
# we will estimate the link between the number of occurrences and the range size 
# we first do it at the mountain range level, and we select only the mountain ranges that have at least 10,000 occ 
# we run a rarefaction method with bootstrapping and calculate the range size

# We will process by mountain range, from the parquet files with DEM elevation 

# --------------------
# Boostrapping 
