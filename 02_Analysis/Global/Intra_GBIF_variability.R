


##-------------
#  1. Set up 
##-------------
library(here); library(data.table); library(dplyr)
library(tidyverse); library(readxl); library(terra)
library(sf); library(arrow); library(rgbif); library(writexl)
library(exactextractr); library(Cairo); library(furrr)
library(ggborderline)

# Load configuration
#source(
#here::here("R/00_Config_file.R")
#)

# define data path OR even config.R file with libraries & path
source_path <- "C:/Users/berou1714/OneDrive - Norwegian University of Life Sciences/Desktop/PhD/"

# source all functions
list.files(path = paste0(source_path, "GMBA_project/Functions"), pattern = "*.R", full.names = TRUE) %>%
  purrr::walk(source)

# -----------------------------------

rarefaction <- read.csv(paste0(source_path, "GMBA_project/files_processed/Reptiles/Rarefaction/rarefaction_Nsp_original.csv")) %>%
  mutate(cv_range = sd_elev_range/mean_elev_range,
         cv_low = sd_minelev/mean_minelev,
         cv_high = sd_maxelev/mean_maxelev)

summary <- rarefaction %>%
  group_by(subsample_occ) %>%
  summarise(mean_cvrange = mean(cv_range, na.rm = TRUE),
            sd_cvrange = sd(cv_range, na.rm = TRUE),
            mean_cvlow = mean(cv_low, na.rm = TRUE),
            sd_cvlow = sd(cv_low, na.rm = TRUE),
            mean_cvhigh = mean(cv_high, na.rm = TRUE),
            sd_cvhigh = sd(cv_high, na.rm = TRUE),
            .groups = "drop")

p <- ggplot(summary, aes(x = subsample_occ)) +
  geom_point(data = rarefaction, aes(y = cv_range, color = "Range"), 
             position = position_nudge(x = -1), alpha = 0.1, size = 2) +
  geom_point(data = rarefaction, aes(y = cv_low, color = "Low"), 
             position = position_nudge(x = 0), alpha = 0.1, size = 2) +
  geom_point(data = rarefaction, aes(y = cv_high, color = "High"), 
             position = position_nudge(x = 1), alpha = 0.1, size = 2) +
  geom_borderline(aes(y = mean_cvrange, color = "Range"), linewidth = 1.5) +
  geom_borderline(aes(y = mean_cvlow, color = "Low"), linewidth = 1.5) +
  geom_borderline(aes(y = mean_cvhigh, color = "High"), linewidth = 1.5) +
  scale_color_manual(
    name = "",
    values = c("Range" = "lightblue", "Low" = "darkblue", "High" = "blue")
  ) +
  ylim(0, 1.2) +
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.9)) +
  labs(x = "Number of occurrences/species", y = "Coef. of variation")
p

ggsave(filename = "test.png", plot = p, dpi = 300)

# -----------
summary_mountain <- rarefaction %>%
  group_by(subsample_occ, Mountain_range) %>%
  summarise(mean_cvrange = mean(cv_range, na.rm = TRUE),
            sd_cvrange = sd(cv_range, na.rm = TRUE),
            mean_cvlow = mean(cv_low, na.rm = TRUE),
            sd_cvlow = sd(cv_low, na.rm = TRUE),
            mean_cvhigh = mean(cv_high, na.rm = TRUE),
            sd_cvhigh = sd(cv_high, na.rm = TRUE),
            .groups = "drop")

p <- ggplot(summary_mountain, aes(x = subsample_occ)) +
  geom_point(data = rarefaction, aes(y = cv_range, color = "Range"), 
             position = position_nudge(x = -1), alpha = 0.1, size = 2) +
  geom_point(data = rarefaction, aes(y = cv_low, color = "Low"), 
             position = position_nudge(x = 0), alpha = 0.1, size = 2) +
  geom_point(data = rarefaction, aes(y = cv_high, color = "High"), 
             position = position_nudge(x = 1), alpha = 0.1, size = 2) +
  geom_borderline(aes(y = mean_cvrange, color = "Range"), linewidth = 1.5) +
  geom_borderline(aes(y = mean_cvlow, color = "Low"), linewidth = 1.5) +
  geom_borderline(aes(y = mean_cvhigh, color = "High"), linewidth = 1.5) +
  scale_color_manual(
    name = "",
    values = c("Range" = "lightblue", "Low" = "darkblue", "High" = "blue")
  ) +
  ylim(0, 1.2) +
  facet_wrap(~ Mountain_range) +
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.9)) +
  labs(x = "Number of occurrences/species", y = "Coef. of variation")
p
ggsave(filename = "test2.png", plot = p, dpi = 300)
