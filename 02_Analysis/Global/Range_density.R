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
source_path <- "C:/Users/berou1714/OneDrive - Norwegian University of Life Sciences/Desktop/PhD/"

# source all functions
list.files(path = paste0(source_path, "GMBA_project/Functions"), pattern = "*.R", full.names = TRUE) %>%
  purrr::walk(source)

# -----------------------------

# here is the bootstrapping file 
boot_file <- read.csv(paste0(source_path, "GMBA_project/files_processed/Reptiles/bootstrap_results.csv"))

# source the gmba regions
# We source as usual but this time we keep low and high elevation for each MR + save a new shp file

# Source the GMBA level03 regions
mountain_shapes03 <- sf::st_read(paste0(source_path, "GMBA_project/GMBA_mountains/mountain_shapes03/mountain_shapes03.shp")) %>%
  st_make_valid()

# ---------------------------
# ---------------------------
# we will estimate the link between the number of occurrences and the range size 
# we first do it at the mountain range level, and we select only the mountain ranges that have at least 10,000 occ 
# we run a rarefaction method with bootstrapping and calculate the range size

# We will process by mountain range, from the parquet files with DEM elevation 

# --------------------
# Boostrapping 

# lets see what are the characteristics of our moutains
boot_summary <- boot_file %>%
  group_by(Mountain_range) %>%
  summarise(n_species = n_distinct(sciname),
          max_occ = max(subsample_occ),
          .groups = "drop") %>%
  arrange(desc(max_occ))

boot_summary %>%
  ggplot(aes(x = max_occ, y = n_species, size = n_species, color = n_species)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(3, 10)) +
  scale_color_gradient(low = "lightblue", high = "red4") +
  theme_minimal() +
  labs(x = "Total number of occurrences",
       y = "Number of species")

# let s see a bit more what we have

# -- Average number of occurrences per species in the diff MR
boot_file %>%
  mutate(subsample_occ = as.factor(subsample_occ)) %>%
  group_by(Mountain_range, subsample_occ) %>%
  summarise(mean_occ_species = mean(mean_occ_species),
            sd = sd(mean_occ_species), .groups = "drop") %>%
  ggplot(aes(x = subsample_occ, y = mean_occ_species, colour = Mountain_range)) +
  geom_point() +
  geom_errorbar(aes(ymin = pmax(mean_occ_species - sd, 0), 
                    ymax = mean_occ_species + sd),
                width = 0.2) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), length.out = 5)]) +
  facet_wrap(~Mountain_range, scales = "free_x") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45)) +
  labs(x = "Total occurrences (subsamples)",
       y = "Number of occurrences per species")


# -- Global distribution of range size across all mountain ranges
# first we add a label of the mean number of species and the mean number of occurrences
n_labels <- boot_file %>%
  filter(subsample_occ < 20000) %>%
  mutate(subsample_occ = as.factor(subsample_occ)) %>%
  group_by(subsample_occ) %>%
  summarise(n = n_distinct(sciname),
            m = mean(mean_occ_species), .groups = "drop")

# Range distribution for all species across all mountains
boot_file %>%
  filter(subsample_occ < 20000) %>% # & n_occ_species > 10
  mutate(subsample_occ = as.factor(subsample_occ)) %>%
  ggplot(aes(x = mean_elev_range, fill = subsample_occ)) +
  geom_density(alpha = 0.5) +
  geom_text(data = n_labels,
            aes(x = Inf, y = Inf, label = paste0("n = ", n)),
            hjust = 1.1, vjust = 1.5, inherit.aes = FALSE) +
  geom_text(data = n_labels,
            aes(x = Inf, y = Inf, label = paste0("m = ", round(m, 1))),
            hjust = 1.1, vjust = 3.5, inherit.aes = FALSE) +
  facet_wrap(~subsample_occ, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Elevational range", y = "density", subtitle = "Range distribution for all species across all mountains")

# Range distribution only for species with at least 10 occurrences in mean
n_labels <- boot_file %>%
  filter(subsample_occ < 20000 & mean_occ_species > 10) %>%
  mutate(subsample_occ = as.factor(subsample_occ)) %>%
  group_by(subsample_occ) %>%
  summarise(n = n_distinct(sciname),
            m = mean(mean_occ_species), .groups = "drop")

boot_file %>%
  filter(subsample_occ < 20000 & mean_occ_species > 10) %>% 
  mutate(subsample_occ = as.factor(subsample_occ)) %>%
  ggplot(aes(x = mean_elev_range, fill = Mountain_range)) +
  geom_density(alpha = 0.5) +
  geom_text(data = n_labels,
            aes(x = Inf, y = Inf, label = paste0("n = ", n)),
            hjust = 1.1, vjust = 1.5, inherit.aes = FALSE) +
  geom_text(data = n_labels,
            aes(x = Inf, y = Inf, label = paste0("m = ", round(m, 1))),
            hjust = 1.1, vjust = 3.5, inherit.aes = FALSE) +
  facet_wrap(~subsample_occ, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Elevational range", y = "density", subtitle = "Range distribution only for species with at least 10 occurrences in mean")

# -----------
# add high and low elevation per MR
boot_file <- boot_file %>% left_join(mountain_shapes03 %>%
                                       st_drop_geometry %>% 
                                       select(Level_03, Elev_Low, Elev_High), by = c("Mountain_range" = "Level_03"))
boot_file <- boot_file %>%
  group_by(Mountain_range) %>%
  mutate(
    Mountain_size = Elev_High - Elev_Low) %>%
  ungroup()

boot_file %>%
  group_by(Mountain_range) %>%
  filter(subsample_occ == max(subsample_occ)) %>%
  mutate(mean_occ_species = mean(mean_occ_species, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Mountain_range = fct_reorder(Mountain_range, mean_occ_species)) %>%
  ggplot(aes(x = mean_elev_range, fill = Mountain_size)) +
  geom_density(alpha = 0.5) +
  scale_fill_gradient2(low = "#d53e4f", mid = "#e6f598", high = "#3288bd", midpoint = median(boot_file$Mountain_size, na.rm = TRUE)) +
  geom_text(
    aes(label = paste0("m = ", round(mean_occ_species, 0))),
    x = Inf, y = Inf,           # top-right corner
    hjust = 1.1, vjust = 1.5,  # nudge inside the panel
    size = 3,
    inherit.aes = FALSE
  ) +
  facet_wrap(~Mountain_range, ncol = 3, scales = "free_y", strip.position = "right", dir = "v") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    strip.text.y.right = element_text(angle = 0, size = 8),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = "grey99")
    ) +
  labs(x = "Elevational range", y = "density", subtitle = "Range size distribution (all species)")


# ---------------------------------------------
# we take a study case for preliminary analysis
# We will 

mountain <- "Pacific Coast Ranges"
boot_subs <- boot_file %>% filter(Mountain_range == mountain)

# Plot of range size density for maximum subsample
boot_subs %>%
  filter(subsample_occ < 20000) %>%
  ggplot(aes(x = mean_elev_range, fill = subsample_occ)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~subsample_occ, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none"
        )

# ----------
# let's look at the same plot but separating the species in 3 categories: low, middle, high of the elevational range
# first, we need to define hard boundaries for the mountain range

mountain_shapes03 %>%
  filter(Level_03 == mountain) %>%
  st_drop_geometry() %>%
  select(Level_03, Elev_Low, Elev_High)

# we extract the low/high limits for this mountain range + define the 1st and 2nd terciles
# 1. First, we do it by using the "true" high and low elevation limits from GMBA

true_terc <- mountain_shapes03 %>%
  filter(Level_03 == mountain) %>%
  st_drop_geometry() %>%
  summarise(terc_33 = quantile(c(Elev_Low, Elev_High), probs = (1/3), na.rm = TRUE),
            terc_66 = quantile(c(Elev_Low, Elev_High), probs = (2/3), na.rm = TRUE))

# this is based on the assumption that the elevation gradient is perfectly linear 
# to have the real terciles, we would need to add the dem 

# 2. We can also do it by using the limits set by the highest and lowest occurrences recorded

occ_terc <- boot_subs %>%
  summarise(
    min_elev = min(mean_minelev, na.rm = TRUE),
    max_elev = max(mean_maxelev, na.rm = TRUE)
  ) %>%
  summarise(terc_33 = quantile(c(min(min_elev), max(max_elev)), probs = (1/3), na.rm = TRUE),
            terc_66 = quantile(c(min(min_elev), max(max_elev)), probs = (2/3), na.rm = TRUE))

# this is also based on the assumption that the gradient is linear
# best would be option 2 + DEM

# ----------
# we define the categories for each species
# we need to know first how the spp is distributed, or maybe what we know of this species based on what we have
# we have the occurrences from GBIF, so let's get based on this. We need to have all the occurrence of the species to estimate RS
# so we keep the estimated RS from the highest number n_occ, i.e. the total number of occ extracted during the bootstrap, to make 
# sure that we have all the occurrences included
# best practice would be to use the original GBIF dataset

# we defined 3 categories, based on if the species ranges cross or not the 33 and/or 66 terciles
# first, we add 2 columns for each quantile TRUE/FALSE, to say if it cross each terciles
# then based on this, we have low = cross 33 or lower, medium = cross both, high = cross 66 or higher

boot_subs <- boot_subs %>%
  group_by(sciname) %>%
  mutate(
    terc33 = case_when(
    mean_maxelev[mean_occ_species == max(mean_occ_species)] > occ_terc$terc_33 & mean_minelev[mean_occ_species == max(mean_occ_species)] < occ_terc$terc_33 ~ TRUE,
    TRUE ~ FALSE
  ),
    terc66 = case_when(
      mean_maxelev[mean_occ_species == max(mean_occ_species)] > occ_terc$terc_66 & mean_minelev[mean_occ_species == max(mean_occ_species)] < occ_terc$terc_66 ~ TRUE,
      TRUE ~ FALSE
  ),
   elev_cat = case_when(
     
     terc33[mean_occ_species == max(mean_occ_species)] == TRUE & terc66[mean_occ_species == max(mean_occ_species)] == TRUE ~ "middle",
     terc33[mean_occ_species == max(mean_occ_species)] == TRUE & terc66[mean_occ_species == max(mean_occ_species)] == FALSE ~ "low",
     terc33[mean_occ_species == max(mean_occ_species)] == FALSE & terc66[mean_occ_species == max(mean_occ_species)] == TRUE ~ "high",
     
     terc33[mean_occ_species == max(mean_occ_species)] == FALSE & terc66[mean_occ_species == max(mean_occ_species)] == FALSE & 
       mean_maxelev[mean_occ_species == max(mean_occ_species)] < occ_terc$terc_33 ~ "low",
     
     terc33[mean_occ_species == max(mean_occ_species)] == FALSE & terc66[mean_occ_species == max(mean_occ_species)] == FALSE & 
       mean_minelev[mean_occ_species == max(mean_occ_species)] > occ_terc$terc_66 ~ "high",
     
     terc33[mean_occ_species == max(mean_occ_species)] == FALSE & terc66[mean_occ_species == max(mean_occ_species)] == FALSE & 
       mean_minelev[mean_occ_species == max(mean_occ_species)] > occ_terc$terc_33 & mean_maxelev[mean_occ_species == max(mean_occ_species)] < occ_terc$terc_66 ~ "middle"
     
   )
  ) %>%
  ungroup()


boot_subs <- boot_subs %>%
  mutate(across(c(elev_cat), 
                ~ factor(., levels = c("low", "middle", "high"))))

# now plot again but separating between categories

boot_subs %>%
  filter(mean_occ_species < 20000) %>%
  mutate(mean_occ_species = as.factor(mean_occ_species)) %>%
  ggplot(aes(x = mean_elev_range, fill = elev_cat)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~mean_occ_species, scales = "free_y") +
  theme_minimal() +
  labs(subtitle = mountain)
  

# ------------------------------------------------------------------
# Now we look at the distribution of species and range sizes along the gradient from another perspective
# We plot all species on y with their range on x (elevation)

boot_subs %>%
  filter(subsample_occ == max(subsample_occ) & mean_elev_range != 0) %>%
  ggplot(aes(x = mean_maxelev, xend = mean_minelev, 
             y = reorder(sciname, (mean_maxelev + mean_minelev) / 2),
             color = mean_elev_range)) +
  geom_segment(linewidth = 2) +
  geom_point(aes(x = (mean_maxelev + mean_minelev) / 2, y = reorder(sciname, (mean_maxelev + mean_minelev) / 2)), 
             size = 1, shape = 21, color = "white", fill = "black") +
  scale_color_gradient(low = "lightblue", high = "red4") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "Elevation (m)", y = "Species", 
       color = "Range size (m)",
       subtitle = paste0(mountain, " | n = ", unique(max(boot_subs$subsample_occ))))

# The same but separating into 3 categories of range sizes (small, medium, big)
boot_subs %>%
  filter(subsample_occ == max(subsample_occ) & mean_elev_range != 0) %>%
  mutate(cat_RS = case_when(
    mean_elev_range < 0.33 * max(mean_elev_range) ~ "small",
    mean_elev_range < 0.66 * max(mean_elev_range) ~ "medium",
    TRUE ~ "large"
  )) %>%
  ggplot(aes(x = mean_maxelev, xend = mean_minelev, 
             y = reorder(sciname, (mean_maxelev + mean_minelev) / 2),
             color = log(mean_occ_species))) +
  geom_segment(linewidth = 2) +
  geom_point(aes(x = (mean_maxelev + mean_minelev) / 2, y = reorder(sciname, (mean_maxelev + mean_minelev) / 2)), 
             size = 1, shape = 21, color = "white", fill = "black") +
  scale_color_gradient(low = "lightblue", high = "red4") +
  facet_wrap(~cat_RS)+
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "Elevation (m)", y = "Species", 
       color = "Log (number \nof species)",
       subtitle = paste0("Great Dividing Range | n = ", unique(max(boot_subs$subsample_occ))))


# ------------------------------------------------------------
# Let's see what is the elevational sampling bias in GBIF
reptiles_GBIF <- arrow::open_dataset(paste0(source_path, "GBIF_data/processed_files/reptiles_gbif_parquet")) %>%
  collect()
# this is the processed gbif file with an elevation colum added. Exactly the same number of occurrences as the raw one.

# first, we group by species and mountain range and compute 2 new columns with min/max records
reptiles_GBIF <- reptiles_GBIF %>%
  group_by(sciname, Level_03) %>%
  mutate(low_occ = min(elevation, na.rm = TRUE),
         high_occ = max(elevation, na.rm = TRUE)) %>%
  ungroup()

# how many species per MR?
n_species <- reptiles_GBIF %>%
  group_by(Level_03, sciname) %>%
  summarise(n_occ = n(), .groups = "drop") %>%
  group_by(Level_03) %>%
  summarise(
    n_species = n_distinct(sciname),
    median_occ = median(n_occ)
  )

# let's investigate a study case
sp <- "Ablepharus anatolicus"
mountain <- "Anatolian / Armenian Highlands"

# first we represent a segment for the range and the points inside are the occurrences
p <- reptiles_GBIF %>%
  filter(sciname == sp & Level_03 == mountain) %>%
  ggplot(aes(x = elevation, y = sciname)) +
  # outer line
  geom_segment(aes(x = low_occ, xend = high_occ, yend = sciname),
               linewidth = 6,
               color = "grey") +
  # inner white line
  geom_segment(aes(x = low_occ, xend = high_occ, yend = sciname),
               linewidth = 5,
               color = "white") +
  geom_point(size = 3, alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(x = "Elevation (m)", y = "",
       subtitle = mountain)
p  
# let s now look at the same but with a density plot

ggMarginal(
  p,
  type = "density",
  margins = "x",
  groupColour = TRUE,
  groupFill = TRUE
)


# ----------------
# New example
mountain <- "Caucasus Mountains"

# Now we do the same but with species with at least n occurrences
reptiles_test <- reptiles_GBIF %>%
  filter(Level_03 == mountain) %>%
  group_by(sciname) %>%
  filter(n() > 20) %>%
  ungroup()
n_distinct(reptiles_test$sciname)  # we have 38 species, let's see how it goes

# let s see first what is the repartition along the gradient
p <- reptiles_test %>%
  ggplot(aes(x = elevation, y = sciname)) +
    geom_segment(aes(x = low_occ, xend = high_occ, yend = sciname),
               linewidth = 6,
               color = "grey30") +
    geom_segment(aes(x = low_occ, xend = high_occ, yend = sciname),
               linewidth = 5,
               color = "white") +
  geom_point(size = 1, alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(x = "elevation (m)", y = "")

ggMarginal(
  p,
  type = "density",
  margins = "x",
  groupColour = TRUE,
  groupFill = TRUE
)

# we need to put all the species on a same "relative gradient" to compare them on a same plot
# 1. we keep the length of each segment and just put the bottom at 0 all the time

p <- reptiles_test %>%
  mutate(
    elev_rel = elevation - low_occ,
    high_rel = high_occ - low_occ,
    low_rel = 0) %>%
  ggplot(aes(x = elev_rel, y = sciname)) +
  geom_segment(aes(x = low_rel, xend = high_rel, yend = sciname),
               linewidth = 6,
               color = "grey") +
  geom_segment(aes(x = low_rel, xend = high_rel, yend = sciname),
               linewidth = 5,
               color = "white") +
  geom_point(size = 1, alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(x = "elevation (m)")

ggMarginal(
  p,
  type = "density",
  margins = "x",
  groupColour = TRUE,
  groupFill = TRUE
)

# 2. we standardize the length of every segments and take the relative position of every points
p <- reptiles_test %>%
  mutate(elev_rel = ((elevation - low_occ) / (high_occ - low_occ)) * 1000,
         high_rel = 1000,
         low_rel = 0) %>%
  ggplot(aes(x = elev_rel, y = sciname)) +
  geom_segment(aes(x = low_rel, xend = high_rel, yend = sciname),
               linewidth = 6,
               color = "grey") +
  geom_segment(aes(x = low_rel, xend = high_rel, yend = sciname),
               linewidth = 5,
               color = "white") +
  geom_point(size = 1, alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(x = "elevation (m)")

ggMarginal(
  p,
  type = "density",
  margins = "x",
  groupColour = TRUE,
  groupFill = TRUE
)

# ------------
# now we would like to see the repartition of occurrences also between low, mid and high ranges
# we keep for example all the species with a range crossing 100 meters in elevation, and we create
# an other categorie for the one that don't

# ------------------
# Preliminary analysis
reptiles_test <- reptiles_test %>%
  group_by(sciname, Level_03) %>%
  mutate(cat_elev = case_when(
    low_occ > 200 ~ "high",
    TRUE ~ "low"
  ))
reptiles_test$cat_elev <- as.factor(reptiles_test$cat_elev)

# Main plot
p_main <- reptiles_test %>%
  ggplot(aes(x = elevation, y = sciname)) +
  
  geom_segment(aes(x = low_occ, xend = high_occ, yend = sciname),
               linewidth = 6,
               color = "grey") +
  
  geom_segment(aes(x = low_occ, xend = high_occ, yend = sciname),
               linewidth = 5,
               color = "white") +
  
  geom_point(size = 1, alpha = 0.5, color = "blue") +
  
  facet_wrap(~cat_elev, scales = "free_y") +
  
  theme_minimal() +
  labs(x = "Elevation (m)", y = NULL)

# Density plot
p_density <- reptiles_test %>%
  ggplot(aes(x = elevation, fill = cat_elev)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~cat_elev, scales = "free_y") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Combine
p_density / p_main +
  plot_layout(heights = c(1, 4))

# ------------------
# Define elevational band, and then categorize species with the midpoint method.
# Per elevational band (100m), keep species only which midpoint is falling in it.

# add the midpoint
reptiles_test <- reptiles_GBIF %>%
  filter(Level_03 == mountain) %>%
  group_by(sciname, Level_03) %>%
  mutate(midpoint = (high_occ + low_occ)/2,
         n_occ = n()) %>%
  ungroup()

# define the elevational band for midpoint + occ band for occurrences
reptiles_test <- reptiles_test %>%
  group_by(sciname, Level_03) %>%
  mutate(midpoint_band = ceiling(midpoint / 100) * 100) %>%
  ungroup() %>%
  mutate(occ_band = ceiling(elevation / 100) * 100)

# the function ceiling() get the smallest integer greater than or equal to a given number
# e.g., if x = 2.3, ceiling(x) = 3. So here we divide by 100 because we define 
# elevational band each 100 meters, so we round up the hundreds.

# 1. we plot per elevational band the absolute number of records first
reptiles_test %>%
  ggplot() +
  geom_histogram(aes(x = occ_band), binwidth = 100, fill = "#378ADD", 
                 color = "white", 
                 linewidth = 0.3) +
  theme_minimal() +
  labs(x = "Elevation (m)", y = "Number of occurrences")



# ------------------------------------------------------------
# SIMULATION OF MODEL SPECIES
# mountain size = 2000m
# species range size = N(1000, 500)

# 1. Mid-domain effect model (MDE), hard-boundaries
# All species are within the boundaries and therefore constrained by these
# Midpoints are uniformly distributed within the domain, and ranges are constrained
# afterward with the domain boundaries

data_fr <- data.frame(
  midp = runif(100, min = 0, max = 2000),
  true_range = abs(rnorm(100, mean = 1000, sd = 500))
) %>%
  mutate(
    true_low  = midp - true_range / 2,
    true_high = midp + true_range / 2,
    obs_low   = pmax(true_low, 0),
    obs_high  = pmin(true_high, 2000),
    obs_range = obs_high - obs_low
  ) %>%
  filter(obs_range > 0) %>%
  rowid_to_column("ID")

density_plot <- ggplot(data_fr, aes(x = obs_range)) +
  geom_density(fill = "firebrick", alpha = 0.5) +
  theme_minimal() +
  labs(x = "Range size") +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank())
density_plot

# 3. Main segment plot
main_plot <- ggplot(data_fr, aes(x = midp, y = ID)) +
  geom_segment(aes(x = true_low, xend = true_high, yend = ID),
               linewidth = 1, color = "pink") +
  geom_segment(aes(x = obs_low, xend = obs_high, yend = ID),
               linewidth = 1, color = "red") +
  theme_minimal() +
  labs(x = "Elevation (m)", y = "Species")
main_plot

# 4. Combine
density_plot / main_plot + plot_layout(heights = c(1, 4))

# NOTE --> in this case, it follow a normal distribution, so-called mid-domain effect
# Ranges along the edges are cut and therefore are smaller than in the middle of
# the domaim


# 2. Soft boundaries, extended domain
# Species are evenly distributed along a gradient that goes beyond the min
# and max of the mountain limits, as the maximum would be a species that has 
# a mid point at the lowest elevation and has a range going all the way up to 
# top. So we assume no hard-boundaries, and the domain is just a window into a 
# larger domain.

data_fr <- data.frame(
  midp = runif(100, min = -2000, max = 4000),
  true_range = abs(rnorm(100, mean = 1000, sd = 500))
) %>%
  mutate(
    true_low  = midp - true_range / 2,
    true_high = midp + true_range / 2,
    obs_low   = pmax(true_low, 0),
    obs_high  = pmin(true_high, 2000),
    obs_range = obs_high - obs_low
  ) %>%
  filter(obs_range > 0) %>%
  rowid_to_column("ID")

density_plot <- ggplot(data_fr, aes(x = obs_range)) +
  geom_density(fill = "firebrick", alpha = 0.5) +
  theme_minimal() +
  labs(x = "Range size")
density_plot

# 3. Main segment plot
main_plot <- ggplot(data_fr, aes(x = midp, y = ID)) +
  geom_segment(aes(x = true_low, xend = true_high, yend = ID),
               linewidth = 1, color = "pink") +
  geom_segment(aes(x = obs_low, xend = obs_high, yend = ID),
               linewidth = 1, color = "red") +
  theme_minimal() +
  labs(x = "Elevation (m)", y = "Species")
main_plot

# 4. Combine
density_plot / main_plot + plot_layout(heights = c(1, 4))

# NOTE --> in this case, curve is left-skewed, with a higher number of small range size
# species, because these large range size species are cut and increase the number
# of small range size around the edges


# 3. One-side truncated model. One soft boundary, one hard boundary.
# Species are evenly distributed along a gradient that goes beyond the min
# BUT NOT beyond the max of the mountain limits. That means that we have an hard 
# boundary only on one side.

data_fr <- data.frame(
  midp = runif(100, min = -2000, max = 2000),
  true_range = abs(rnorm(100, mean = 1000, sd = 500))
) %>%
  mutate(
    true_low  = midp - true_range / 2,
    true_high = midp + true_range / 2,
    obs_low   = pmax(true_low, 0),
    obs_high  = pmin(true_high, 2000),
    obs_range = obs_high - obs_low
  ) %>%
  filter(obs_range > 0) %>%
  rowid_to_column("ID")

density_plot <- ggplot(data_fr, aes(x = obs_range)) +
  geom_density(fill = "firebrick", alpha = 0.5) +
  theme_minimal() +
  labs(x = "Range size") +
  theme(axis.title.x = element_blank())
density_plot

# 3. Main segment plot
main_plot <- ggplot(data_fr, aes(x = midp, y = ID)) +
  geom_segment(aes(x = true_low, xend = true_high, yend = ID),
               linewidth = 1, color = "pink") +
  geom_segment(aes(x = obs_low, xend = obs_high, yend = ID),
               linewidth = 1, color = "red") +
  theme_minimal() +
  labs(x = "Elevation (m)", y = "Species")
main_plot

# 4. Combine
density_plot / main_plot + plot_layout(heights = c(1, 4))

# NOTE --> here, we have a kind of unimodal distribution, but the peak
# is slightly left-skewed. 

