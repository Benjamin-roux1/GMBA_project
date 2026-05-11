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

# source all functions
list.files(path = paste0(source_path, "GMBA_project/Functions"), pattern = "*.R", full.names = TRUE) %>%
  purrr::walk(source)

# -----------------------------
# Load the dataset
reptiles <- read_xlsx(paste0(source_path, "GMBA_project/files_processed/Reptiles/reptiles_dataframe.xlsx"))
reptiles_GBIF <- reptiles %>%
  filter(!is.na(NumberOcc))

# here is the bootstrapping file with all mountains that have more than 2,000 occurrences
boot_file <- read.csv(paste0(source_path, "GMBA_project/files_processed/bootstrap_results.csv"))

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
  theme.perso()

# let s see a bit more what we have

# -- Average number of occurrences per species in the diff MR
boot_file %>%
  filter(subsample_occ < 20000) %>%
  mutate(subsample_occ = as.factor(subsample_occ)) %>%
  group_by(Mountain_range, subsample_occ) %>%
  summarise(mean_occ_species = mean(n_occ_species),
            sd = sd(n_occ_species), .groups = "drop") %>%
  ggplot(aes(x = subsample_occ, y = mean_occ_species, colour = Mountain_range)) +
  geom_point() +
  geom_errorbar(aes(ymin = pmax(mean_occ_species - sd, 0), 
                    ymax = mean_occ_species + sd),
                width = 0.2) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), length.out = 5)]) +
  facet_wrap(~Mountain_range, scales = "free") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45)) +
  labs(x = "Total occurrences (subsamples)",
       y = "Number of occurrences per species")


# -- Global distribution of range size across all mountain ranges
# first we add a label of the mean number of species and the mean number of occurrences
n_labels <- boot_file %>%
  filter(subsample_occ < 20000) %>% # & n_occ_species > 10
  mutate(subsample_occ = as.factor(subsample_occ)) %>%
  group_by(subsample_occ) %>%
  summarise(n = n_distinct(sciname),
            m = mean(n_occ_species), .groups = "drop")

# Range distribution for all species across all mountains
boot_file %>%
  filter(subsample_occ < 20000) %>% # & n_occ_species > 10
  mutate(subsample_occ = as.factor(subsample_occ)) %>%
  ggplot(aes(x = elev_range, fill = subsample_occ)) +
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
  filter(subsample_occ < 20000 & n_occ_species > 10) %>%
  mutate(subsample_occ = as.factor(subsample_occ)) %>%
  group_by(subsample_occ) %>%
  summarise(n = n_distinct(sciname),
            m = mean(n_occ_species), .groups = "drop")

boot_file %>%
  filter(subsample_occ < 20000 & n_occ_species > 10) %>% 
  mutate(subsample_occ = as.factor(subsample_occ)) %>%
  ggplot(aes(x = elev_range, fill = Mountain_range)) +
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

boot_file %>%
  group_by(Mountain_range) %>%
  filter(subsample_occ == max(subsample_occ)) %>% 
  ungroup() %>%
  mutate(subsample_occ = as.factor(subsample_occ)) %>%
  ggplot(aes(x = elev_range, fill = Mountain_range)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Mountain_range, ncol = 1, scales = "free_y", strip.position = "right") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    strip.text.y.right = element_text(angle = 0, size = 8),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = "grey99")
    ) +
  labs(x = "Elevational range", y = "density", subtitle = "Range size distribution (all species)")


# -- Range size variation with the size of the subsample
boot_file %>%
  filter(subsample_occ < 30000 & Mountain_range == "Anatolian / Armenian Highlands") %>%
  mutate(subsample_occ = as.factor(subsample_occ)) %>%
  group_by(subsample_occ) %>%
  summarise(range_size = mean(elev_range, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = subsample_occ, y = range_size)) +
  geom_point() +
  geom_line(group = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))


# -------------------  
# we take a study case for preliminary analysis
#mountain <- "Central European Highlands"

mountain_list <- boot_summary$Mountain_range[1:20]

for (mountain in mountain_list) {

  boot_subs <- boot_file %>% filter(Mountain_range == mountain)

# Plot of range sizes density per subsample 

  plot1 <- boot_subs %>%
  filter(n_occ < 20000) %>%
  mutate(n_occ = as.factor(n_occ)) %>%
  ggplot(aes(x = elev_range, fill = n_occ)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~n_occ, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none"
        ) +
  labs(subtitle = boot_subs$Mountain_range)
  
  ggsave(paste0(source_path, "GMBA_project/Figures/bootstrap/", 
                gsub("[^a-zA-Z0-9_-]", "_", mountain), 
                "1.png"),
         plot = plot1, width = 15, height = 10)
  
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
    min_elev = min(minelev, na.rm = TRUE),
    max_elev = max(maxelev, na.rm = TRUE)
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
    maxelev[n_occ == max(n_occ)] > occ_terc$terc_33 & minelev[n_occ == max(n_occ)] < occ_terc$terc_33 ~ TRUE,
    TRUE ~ FALSE
  ),
    terc66 = case_when(
      maxelev[n_occ == max(n_occ)] > occ_terc$terc_66 & minelev[n_occ == max(n_occ)] < occ_terc$terc_66 ~ TRUE,
      TRUE ~ FALSE
  ),
   elev_cat = case_when(
     
     terc33[n_occ == max(n_occ)] == TRUE & terc66[n_occ == max(n_occ)] == TRUE ~ "middle",
     terc33[n_occ == max(n_occ)] == TRUE & terc66[n_occ == max(n_occ)] == FALSE ~ "low",
     terc33[n_occ == max(n_occ)] == FALSE & terc66[n_occ == max(n_occ)] == TRUE ~ "high",
     
     terc33[n_occ == max(n_occ)] == FALSE & terc66[n_occ == max(n_occ)] == FALSE & 
       maxelev[n_occ == max(n_occ)] < occ_terc$terc_33 ~ "low",
     
     terc33[n_occ == max(n_occ)] == FALSE & terc66[n_occ == max(n_occ)] == FALSE & 
       minelev[n_occ == max(n_occ)] > occ_terc$terc_66 ~ "high",
     
     terc33[n_occ == max(n_occ)] == FALSE & terc66[n_occ == max(n_occ)] == FALSE & 
       minelev[n_occ == max(n_occ)] > occ_terc$terc_33 & maxelev[n_occ == max(n_occ)] < occ_terc$terc_66 ~ "middle"
     
   )
  ) %>%
  ungroup()


  boot_subs <- boot_subs %>%
  mutate(across(c(elev_cat), 
                ~ factor(., levels = c("low", "middle", "high"))))

# now plot again but separating between categories

  plot2 <- boot_subs %>%
  filter(n_occ < 20000) %>%
  mutate(n_occ = as.factor(n_occ)) %>%
  ggplot(aes(x = elev_range, fill = elev_cat)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~n_occ, scales = "free_y") +
  theme_minimal() +
  labs(subtitle = mountain)
  
  ggsave(paste0(source_path, "GMBA_project/Figures/bootstrap/", 
                gsub("[^a-zA-Z0-9_-]", "_", mountain), 
                "2.png"),
         plot = plot2, width = 15, height = 10)
}






boot_subs <- boot_file %>% filter(Mountain_range == "Great Dividing Range")

boot_subs %>%
  filter(subsample_occ == max(subsample_occ) & elev_range != 0 & n_occ_species > 10) %>%
  ggplot(aes(x = maxelev, xend = minelev, 
             y = reorder(sciname, (maxelev + minelev) / 2),
             color = elev_range)) +
  geom_segment(linewidth = 2) +
  geom_point(aes(x = (maxelev + minelev) / 2, y = reorder(sciname, (maxelev + minelev) / 2)), 
             size = 1, shape = 21, color = "white", fill = "black") +
  scale_color_gradient(low = "lightblue", high = "red4") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "Elevation (m)", y = "Species", 
       color = "Range size (m)",
       subtitle = paste0("Great Dividing Range | n = ", unique(max(boot_subs$subsample_occ))))

boot_subs %>%
  filter(subsample_occ == max(subsample_occ) & elev_range != 0 & n_occ_species > 10) %>%
  mutate(cat_RS = case_when(
    elev_range < 0.33 * max(elev_range) ~ "small",
    elev_range < 0.66 * max(elev_range) ~ "medium",
    TRUE ~ "large"
  )) %>%
  ggplot(aes(x = maxelev, xend = minelev, 
             y = reorder(sciname, (maxelev + minelev) / 2),
             color = log(n_occ_species))) +
  geom_segment(linewidth = 2) +
  geom_point(aes(x = (maxelev + minelev) / 2, y = reorder(sciname, (maxelev + minelev) / 2)), 
             size = 1, shape = 21, color = "white", fill = "black") +
  scale_color_gradient(low = "lightblue", high = "red4") +
  facet_wrap(~cat_RS)+
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "Elevation (m)", y = "Species", 
       color = "Log (number \nof species)",
       subtitle = paste0("Great Dividing Range | n = ", unique(max(boot_subs$subsample_occ))))
