##-----------------------
#  1.1. Set up -----
##-----------------------
library(here); library(data.table); library(dplyr)
library(tidyverse); library(readxl); library(terra)
library(sf); library(arrow); library(rgbif); library(writexl)
library(exactextractr); library(ggExtra); library(patchwork)
library(future); library(furrr)

# Load configuration
#source(
#here::here("R/00_Config_file.R")
#)

# define data path OR even config.R file with libraries & path
source_path <- "C:/Users/berou1714/OneDrive - Norwegian University of Life Sciences/Desktop/PhD/"

# source all functions
list.files(path = paste0(source_path, "GMBA_project/Functions"), pattern = "*.R", full.names = TRUE) %>%
  purrr::walk(source)

# -------------------------
# Load & clean the dataset
# -------------------------
reptiles_GBIF <- arrow::open_dataset(paste0(source_path, "GBIF_data/processed_files/reptiles_gbif_parquet")) %>%
  collect()

mountains_summary <- reptiles_GBIF %>%
  group_by(Level_03) %>%
  summarise(n_occ = n())

# some mountains have 1-10 occurrences, we remove them
mountains_names <- mountains_summary %>%
  filter(n_occ < 10) %>%
  pull(Level_03)

# ---- OPTIONAL: remove mountains with less than X occurrences
mountains_names <- mountains_summary %>%
  filter(n_occ < 1000) %>%
  pull(Level_03)


reptiles_GBIF <- reptiles_GBIF %>%
  filter(!Level_03 %in% mountains_names)

# -------------------------

# first, we group by species and mountain range and compute 2 new columns with min/max records
reptiles_GBIF <- reptiles_GBIF %>%
  group_by(sciname, Level_03) %>%
  mutate(low_occ = min(elevation, na.rm = TRUE),
         high_occ = max(elevation, na.rm = TRUE)) %>%
  ungroup()

# we put all species on a same relative gradient from 0 to 1000m
reptiles_01 <- reptiles_GBIF %>%
  group_by(sciname, Level_03) %>%
  mutate(elev_rel = ((elevation - low_occ) / (high_occ - low_occ)) * 1000,
         high_rel = 1000,
         low_rel = 0) %>%
  ungroup()

# plot --> relative position of occurrences across mountain ranges
x11();reptiles_01 %>%
  mutate(Level_03 = factor(Level_03, 
                           levels = reptiles_01 %>%
                             group_by(Level_03) %>%
                             summarise(max_high = max(high_occ, na.rm = TRUE)) %>%
                             arrange(max_high) %>%
                             pull(Level_03))) %>%
  ggplot(aes(x = elev_rel, fill = Level_03)) +
  geom_density() +
  facet_wrap(~Level_03, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank()) +
  labs(subtitle = "Relative position of occurrences")

# plot --> absolute position of occurrences across mountain ranges
x11();reptiles_01 %>%
  mutate(Level_03 = factor(Level_03, 
                           levels = reptiles_01 %>%
                             group_by(Level_03) %>%
                             summarise(max_high = max(high_occ, na.rm = TRUE)) %>%
                             arrange(max_high) %>%
                             pull(Level_03))) %>%
  ggplot(aes(x = elevation, fill = Level_03)) +
  geom_density() +
  facet_wrap(~Level_03, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(subtitle = "Absolute position of occurrences")

# plot --> absolute number of occurrences across mountain ranges
x11();reptiles_01 %>%
  mutate(Level_03 = factor(Level_03, 
                           levels = reptiles_01 %>%
                             group_by(Level_03) %>%
                             summarise(max_high = max(high_occ, na.rm = TRUE)) %>%
                             arrange(max_high) %>%
                             pull(Level_03))) %>%
  ggplot(aes(x = elevation, fill = Level_03)) +
  geom_bar(stat = "count") +
  facet_wrap(~Level_03, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none")

# -------------------------------------------------------
# we first do the analysis with an example
mountain <- "Southwest European Highlands"

reptiles_02 <- reptiles_01 %>%
  filter(Level_03 == mountain)

# Plot the relative position of occurrences for each SPECIES
reptiles_02 %>%
  ggplot(aes(x = elev_rel)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_minimal()

# We compute the distribution of occurrences now separately depending on the
# elevational category
# We define species categories depending on the midpoint: we split the mountain 
# in five bands of equal length
band_length <- (max(reptiles_02$high_occ) - min(reptiles_02$low_occ))/5

# define each species midpoint + attribute a band length
reptiles_02 <- reptiles_02 %>%
  group_by(sciname) %>%
  mutate(midpoint = (high_occ + low_occ)/2,
  elev_cat = case_when(
    midpoint < band_length ~ "band_1",
    midpoint < 2 * band_length ~ "band_2",
    midpoint < 3 * band_length ~ "band_3",
    midpoint < 4 * band_length ~ "band_4",
    TRUE ~ "band_5"
  ),
  elev_cat = factor(elev_cat, levels = c("band_1", "band_2", "band_3", "band_4", "band_5"))
  )

# Plot the relative position of occurrences for each band
reptiles_02 %>%
  ggplot(aes(x = elev_rel, fill = elev_cat)) +
  geom_density() +
  facet_wrap(~elev_cat) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )


# ---------------------------------------------------------------
# Understand sampling biases

mountain <- "Southwest European Highlands"

reptiles_02 <- reptiles_01 %>%
  filter(Level_03 == mountain)

# raw distribution of occurrences
reptiles_02 %>%
  ggplot(aes(x = elevation)) +
  geom_density(fill = "blue", alpha = 0.2) +
  theme_minimal()

# calculate range size for each species
reptiles_02 <- reptiles_02 %>%
  group_by(sciname) %>%
  mutate(range_size = quantile(elevation, 0.95, na.rm = TRUE) - quantile(elevation, 0.05, na.rm = TRUE)) %>%
  ungroup()

# attribute a category according to the the size of the range
reptiles_02 <- reptiles_02 %>%
  mutate(rs_cat = case_when(
    range_size < 500 ~ 1,
    range_size < 1000 & range_size >= 500 ~ 2,
    range_size < 1500 & range_size >= 1000 ~ 3,
    range_size < 2000 & range_size >= 1500 ~ 4,
    range_size >= 2000 ~ 5,
    TRUE ~ NA_real_
  ))

# plot distribution of occurrences across different categories of range sizes
reptiles_02 %>%
  ggplot(aes(x = elev_rel)) +
  geom_density(fill = "blue", alpha = 0.2) +
  facet_wrap(~rs_cat) +
  theme_minimal()

# -------------------------------
# 1. Quantify absolute sampling bias

# observe mountain profile
dem <- terra::rast(paste0(source_path, "DEM/demMountains_GLO90.tif"))
mountain_shape <- sf::st_read(paste0(source_path, "GMBA_project/GMBA_mountains/mountain_shapes03/mountain_shapes03.shp"))

mountain_shape1 <- mountain_shape %>%
  filter(Level_03 == mountain)

dem1 <- terra::crop(dem, mountain_shape1)
plot(dem1)

# ------------
# "availability" in function of elevational band, i.e. available area in each elevational band
# create elevational band
elev_band <- seq(floor(min(elev)), ceiling(max(elev)), by = 100)
# extract elevations
elev <- values(dem1, na.rm = TRUE)
# compute number of pixels in each elevational band
hypsometry <- data.frame(elevation = as.numeric(elev)) %>%
  mutate(band = cut(elevation,
                    breaks = elev_band,
                    include.lowest = TRUE)) %>%
  count(band)

ggplot(hypsometry, aes(x = band, y = n)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Elevational bands",
       y = "Number of pixels")

# ------------
# "accessibility", i.e steepness in each elevational band
slope <- terra::terrain(dem1, v = "slope", unit = "degrees")
plot(slope)

steepness <- data.frame(
  elevation = as.numeric(values(dem1)),
  slope = as.numeric(values(slope))
) %>%
  mutate(band = cut(elevation,
                    breaks = elev_band,
                    include.lowest = TRUE))

steepness_by_band <- steepness %>%
  group_by(band) %>%
  summarise(
    mean_slope = mean(slope, na.rm = TRUE),
    sd_slope = sd(slope, na.rm = TRUE),
    n = n()
  )

ggplot(steepness_by_band, aes(x = band, y = mean_slope)) +
  geom_col() +
  theme_minimal()


# ----------------------------------------
# Rarefaction framework
# let's first work on the total number of occurrences per mountain range

raref_tot <- read.csv(paste0(source_path, "GMBA_project/files_processed/bootstrap_OccTotal.csv"))

mountain <- "Levant Ranges"

test <- raref_tot %>%
  filter(Mountain_range == mountain)

x11();ggplot(test, aes(x = elev_bin_mid, y = mean_density, fill = subsample_occ)) +
  geom_col() +
  facet_wrap(~subsample_occ) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(subtitle = mountain,
       x = "Elevation (m.a.s.l)",
       y = "Density of occurrences")

ggplot(test, aes(x = elev_bin_mid, y = mean_density, 
                 group = subsample_occ, color = subsample_occ)) +


# -----------------------------
raref_spp <- read.csv(paste0(source_path, "GMBA_project/files_processed/bootstrap_OccSpp.csv"))

mountain <- "Altai-Sayan region"

test <- raref_spp %>%
  filter(Mountain_range == mountain)

# Create a summary label per subsample (one value per facet)
labels_df <- test %>%
  distinct(subsample_occ, n_species)

ggplot(test, aes(x = elev_bin_mid, y = mean_density, fill = subsample_occ)) +
  geom_col() +
  geom_text(data = labels_df, 
            aes(label = paste0("n_sp = ", n_species)),
            x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
            inherit.aes = FALSE) +
  facet_wrap(~subsample_occ) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(subtitle = mountain,
       x = "Elevation (m.a.s.l)",
       y = "Density of occurrences")














## ----------------------
#  1. Load dataset
## ----------------------
reptiles_GBIF <- arrow::open_dataset(paste0(source_path, "GBIF_data/processed_files/reptiles_gbif_parquet")) %>%
  collect() %>%
  rename(Mountain_range = "Level_03")

## -------------------------------------------------------------
#  2. Compute a 'model' mountain for distribution of occurrences
## -------------------------------------------------------------

## --------------------------------
#  2.1. Filter & clean the dataset
## --------------------------------
# look at the number of occurrences in each MR
mountains_summary <- reptiles_GBIF %>%
  group_by(Mountain_range) %>%
  summarise(n_occ = n())

# we will work with mountain ranges with enough occurrences, ie > 50,000 occ
mountains_names <- mountains_summary %>%
  filter(n_occ > 50000) %>%
  pull(Mountain_range)

reptiles_GBIF <- reptiles_GBIF %>%
  filter(Mountain_range %in% mountains_names) %>%
  drop_na()

## ------------------------------------------
#  2.2. Scale on a 0-1000m elevation each MR
## ------------------------------------------
# we group by mountain range and compute 2 new columns with min/max records
reptiles_01 <- reptiles_GBIF %>%
  group_by(Mountain_range) %>%
  mutate(low_lim = min(elevation, na.rm = TRUE),
         high_lim = max(elevation, na.rm = TRUE),
         elev_rel = ((elevation - low_lim) / (high_lim - low_lim)) * 1000) %>%
  ungroup()

# plot the occurrences distribution of all mountains
reptiles_01 %>%
  ggplot(aes(x = elevation)) +
  geom_density(fill = "red", alpha = 0.3) +
  theme_minimal() +
  labs(y = "Density of occurrences") +
  facet_grid(Mountain_range ~ ., scales = "free")

## ------------------------------------------
#  2.3. Extract the % of occurrences in each elevational band (50m band)
## ------------------------------------------
# then, we need to cut per elevational band and extract the proportion of occurrences in each band
mountains_band <- reptiles_01 %>%
  group_by(Mountain_range) %>%
  mutate(elev_bin = cut(elev_rel, 
                        breaks = seq(0, max(elev_rel, na.rm = TRUE) + 50, by = 50),
                        right = FALSE),
         n_tot = n()) %>%
  group_by(elev_bin, Mountain_range) %>%
  summarise(n_occ = n(), n_tot = first(n_tot), .groups = "drop") %>%
  mutate(prop_occ = n_occ / n_tot) %>%
  ungroup()

ggplot(mountains_band, aes(x = elev_bin, y = n_occ)) +
  geom_col(fill = "red", alpha = 0.3) +
  theme_minimal() +
  facet_grid(Mountain_range ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

# We now see the distribution of occurrences in the different mountain ranges
# We are not taking one as a reference, but rather an 'averaged' one across all elevational band
# We compute the proportion in each band for each MR, and take the average to create a new model distribution
# This is to avoid the fact that some mountains have very different distributions, and so drag down the total
# number of occurrences in some cases 

## ------------------------------------------
#  2.4. Compute average proportion across all MR in each elevational band
## ------------------------------------------
model_band <- mountains_band %>%
  group_by(elev_bin) %>%
  summarise(prop_model = mean(prop_occ, na.rm =TRUE), .groups = "drop")

ggplot(mountains_band, aes(x = elev_bin, y = prop_occ)) +
  geom_col(aes(fill = "Observed"), alpha = 0.3) +
  geom_col(data = model_band, aes(y = prop_model, fill = "Model"), alpha = 0.3) +
  scale_fill_manual(values = c("Observed" = "red", "Model" = "purple")) +
  theme_minimal(base_family = "calibri") +
  facet_grid(Mountain_range ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  labs(fill = NULL,
       x = "Elevation", y = "Proportion of occurrences")


## ------------------------------------------
#  2.4. Visualise the difference between the number of occurrences sampled and original dataset
## ------------------------------------------
# Join with model distribution and compute sampling targets
sampling_sum <- mountains_band %>%
  left_join(model_band, by = "elev_bin") %>%
  group_by(Mountain_range, elev_bin) %>%
  mutate(
    n_tot_min = min(n_occ / prop_model, na.rm = TRUE),
    n_bin_tosample = round(prop_model * n_tot_min)
  )

ggplot(mountains_band, aes(x = elev_bin, y = n_occ)) +
  geom_col(aes(fill = "Observed"), alpha = 0.3) +
  geom_col(data = sampling_sum, aes(y = n_bin_tosample, fill = "Model"), alpha = 0.3) +
  geom_text(data = sampling_sum, aes(label = paste0("ntot = ", round(n_tot))), 
            x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, inherit.aes = FALSE, size = 3) +
  geom_text(data = sampling_sum, aes(label = paste0("nsample = ", round(n_tot_min))), 
            x = Inf, y = Inf, hjust = 1.1, vjust = 3.5, inherit.aes = FALSE, size = 3) +
  scale_fill_manual(values = c("Observed" = "red", "Model" = "purple")) +
  theme_minimal(base_family = "calibri") +
  facet_grid(Mountain_range ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  labs(fill = NULL,
       x = "Elevation", y = "Proportion of occurrences")

## ------------------------------------------
#  2.5. Run the rarefaction
## ------------------------------------------
options(future.globals.maxSize = 10 * 1024^3)  # 10 GiB
plan(multisession, workers = 10)

# rarefaction on the number of occurrences per species
results <- rarefaction.Nsp.reshuffled(reptiles_GBIF, model_band, replications = 1000, n_occ = 50000)
write.csv(results, paste0(source_path, "GMBA_project/Outputs/rarefaction_Nsp.csv"), row.names = FALSE)
# rarefaction on the total of occurrences in the mountain range
# results <- rarefaction.Ntot.reshuffled(reptiles_GBIF, model_band, replications = 1000, n_occ = 50000)
# write.csv(results, paste0(source_path, "GMBA_project/Outputs/rarefaction_Ntot.csv"), row.names = FALSE)

plan(sequential)

# -----------------------
# Analysis of the results
Nsp_original <- read.csv(paste0(source_path, "GMBA_project/files_processed/Reptiles/Rarefaction/rarefaction_Nsp_original.csv"))
Ntot_original <- read.csv(paste0(source_path, "GMBA_project/files_processed/Reptiles/Rarefaction/rarefaction_Ntot_original.csv"))
Nsp_reshuffled <- read.csv(paste0(source_path, "GMBA_project/files_processed/Reptiles/Rarefaction/rarefaction_Nsp_reshuffled.csv"))
Ntot_reshuffled <- read.csv(paste0(source_path, "GMBA_project/files_processed/Reptiles/Rarefaction/rarefaction_Ntot_reshuffled.csv"))

# keep in the original dataset only the MR used in the reshuffled distribution analysis
mountain_names <- Ntot_reshuffled %>%
  distinct(Mountain_range) %>%
  pull(Mountain_range)
Ntot_original <- Ntot_original %>%
  filter(Mountain_range %in% mountain_names)

# --------------
# For Ntot

# PLOT -- range size density for resampled distribution at the final n_tot
Ntot_reshuffled %>%
  group_by(Mountain_range) %>%
  filter(subsample_occ == max(subsample_occ)) %>%
  ungroup() %>%
  ggplot(aes(x = mean_elev_range, fill = Mountain_range)) +
  geom_density(alpha = 0.5) +
  theme_minimal(base_family = "calibri") +
  facet_grid(Mountain_range ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "none") +
  labs(fill = NULL,
       x = "Elevation", y = "Range size density")

# PLOT -- range size density for original distribution at the final n_tot
Ntot_original %>%
  group_by(Mountain_range) %>%
  filter(subsample_occ == max(subsample_occ)) %>%
  ungroup() %>%
  ggplot(aes(x = mean_elev_range)) +
  geom_density(aes(fill = "original"), alpha = 0.5) +
  geom_density(data = Ntot_reshuffled %>%
                 group_by(Mountain_range) %>%
                 filter(subsample_occ == max(subsample_occ)), aes(x = mean_elev_range, fill = "reshuffled"),
               alpha = 0.3) +
  scale_fill_manual(values = c("original" = "steelblue", "reshuffled" = "purple")) +
  theme_minimal(base_family = "calibri") +
  facet_grid(Mountain_range ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "bottom") +
  labs(fill = NULL,
       x = "Elevation", y = "Range size density")

# let's look specifically at the rarefaction plot for the mountains with enough occurrences
Ntot_reshuffled %>%
  filter(Mountain_range == "Pacific Coast Ranges") %>%
  ggplot(aes(x = mean_elev_range, fill = subsample_occ)) +
  geom_density(alpha = 0.5) +
  theme_minimal(base_family = "calibri") +
  facet_wrap(~ subsample_occ, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "none", axis.text.y = element_text(size = 5)) +
  labs(fill = NULL,
       x = "Elevation", y = "Range size density")

Ntot_original %>%
  filter(Mountain_range == "Pacific Coast Ranges" & subsample_occ < 10000) %>%
  ggplot(aes(x = mean_elev_range)) +
  geom_density(aes(fill = "original"), alpha = 0.5) +
  geom_density(data = Ntot_reshuffled %>%
                 filter(Mountain_range == "Pacific Coast Ranges"), 
               aes(x = mean_elev_range, fill = "reshuffled"), alpha = 0.5) +
  theme_minimal(base_family = "calibri") +
  scale_fill_manual(values = c("original" = "steelblue", "reshuffled" = "purple")) +
  facet_wrap(~ subsample_occ, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "bottom", axis.text.y = element_text(size = 5)) +
  labs(fill = NULL,
       x = "Elevation", y = "Range size density")

# ------- OPTION: GIF
library(gganimate)

anim <- rarefaction_Ntot %>%
  filter(Mountain_range == "Great Dividing Range") %>%
  ggplot(aes(x = mean_elev_range, fill = subsample_occ)) +
  geom_density(alpha = 0.5) +
  scale_fill_viridis_c() +
  theme_minimal() +
  transition_states(subsample_occ, transition_length = 2, state_length = 1) +
  labs(title = "n occ: {closest_state}", x = "Elevation", y = "Density")

animate(anim, renderer = gifski_renderer("rarefaction.gif"))



# --------------
# For Nsp

# PLOT -- range size density for resampled distribution at the final n_tot
Nsp_reshuffled %>%
  group_by(Mountain_range) %>%
  filter(subsample_occ == max(subsample_occ)) %>%
  ungroup() %>%
  ggplot(aes(x = mean_elev_range, fill = Mountain_range)) +
  geom_density(alpha = 0.5) +
  theme_minimal(base_family = "calibri") +
  facet_grid(Mountain_range ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "none") +
  labs(fill = NULL,
       x = "Elevation", y = "Range size density")

# PLOT -- range size density for original distribution at the final n_tot
#create a label
label_ori <- Nsp_original %>%
  group_by(Mountain_range) %>%
  filter(subsample_occ == max(subsample_occ)) %>%
  summarise(n_species = first(n_species))

label_resh <- Nsp_reshuffled %>%
  group_by(Mountain_range) %>%
  filter(subsample_occ == max(subsample_occ)) %>%
  summarise(n_species = first(n_species))

Nsp_original %>%
  group_by(Mountain_range) %>%
  filter(subsample_occ == max(subsample_occ)) %>%
  ungroup() %>%
  ggplot(aes(x = mean_elev_range)) +
  geom_density(aes(fill = "original"), alpha = 0.5) +
  geom_density(data = Nsp_reshuffled %>%
                 group_by(Mountain_range) %>%
                 filter(subsample_occ == max(subsample_occ)), aes(x = mean_elev_range, fill = "reshuffled"),
               alpha = 0.3) +
  geom_text(data = label_ori, aes(label = paste0("ntot = ", n_species)), 
            x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, inherit.aes = FALSE, size = 3) +
  geom_text(data = label_resh, aes(label = paste0("nsample = ", n_species)), 
            x = Inf, y = Inf, hjust = 1.1, vjust = 3.5, inherit.aes = FALSE, size = 3) +
  scale_fill_manual(values = c("original" = "steelblue", "reshuffled" = "purple")) +
  theme_minimal(base_family = "calibri") +
  facet_grid(Mountain_range ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "bottom") +
  labs(fill = NULL,
       x = "Elevation", y = "Range size density")

# let's look specifically at the rarefaction plot for the mountains with enough occurrences
Nsp_reshuffled %>%
  filter(Mountain_range == "Intermountain West") %>%
  ggplot(aes(x = mean_elev_range, fill = subsample_occ)) +
  geom_density(alpha = 0.5) +
  theme_minimal(base_family = "calibri") +
  facet_wrap(~ subsample_occ, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "none", axis.text.y = element_text(size = 5)) +
  labs(fill = NULL,
       x = "Elevation", y = "Range size density")

#create a label
label_ori <- Nsp_original %>%
  group_by(Mountain_range, subsample_occ) %>%
  summarise(n_species = first(n_species))

label_resh <- Nsp_reshuffled %>%
  group_by(Mountain_range, subsample_occ) %>%
  summarise(n_species = first(n_species))

Nsp_original %>%
  filter(Mountain_range == "Intermountain West") %>%
  ggplot(aes(x = mean_elev_range)) +
  geom_density(aes(fill = "original"), alpha = 0.5) +
  geom_density(data = Nsp_reshuffled %>%
                 filter(Mountain_range == "Intermountain West"), 
               aes(x = mean_elev_range, fill = "reshuffled"), alpha = 0.5) +
  geom_text(data = label_ori %>% filter(Mountain_range == "Intermountain West"), aes(label = paste0("ntot = ", n_species)), 
            x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, inherit.aes = FALSE, size = 3) +
  geom_text(data = label_resh %>% filter(Mountain_range == "Intermountain West"), aes(label = paste0("nsample = ", n_species)), 
            x = Inf, y = Inf, hjust = 1.1, vjust = 3.5, inherit.aes = FALSE, size = 3) +
  theme_minimal(base_family = "calibri") +
  scale_fill_manual(values = c("original" = "steelblue", "reshuffled" = "purple")) +
  facet_wrap(~ subsample_occ, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "bottom", axis.text.y = element_text(size = 5)) +
  labs(fill = NULL,
       x = "Elevation", y = "Range size density")

# PLOT -- global of all mountains 
Nsp_original %>%
  ggplot(aes(x = mean_elev_range, fill = Mountain_range)) +
  geom_density(alpha = 0.5) +
  theme_minimal(base_family = "calibri") +
  facet_wrap(~ subsample_occ, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "bottom") +
  labs(fill = NULL,
       x = "Elevation", y = "Range size density")
