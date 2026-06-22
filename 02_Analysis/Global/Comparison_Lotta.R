## ------------------------------------------------------------
#  Relationship between Nocc and the deviation to expert range
## ------------------------------------------------------------

# In this script, I run a rarefaction (in Orion) on the number of occurrences per species,
# each time calculating the deviation of GBIF range estimates to expert estimates.

##---------------
#  1. Set up 
##---------------
library(here); library(data.table); library(dplyr)
library(tidyverse); library(readxl); library(terra)
library(sf); library(arrow); library(rgbif); library(writexl)
library(exactextractr); library(Cairo); library(furrr)
library(ggborderline); library(patchwork); library(matrixStats)

# Load configuration
#source(
#here::here("R/00_Config_file.R")
#)

# define data path OR even config.R file with libraries & path
source_path <- "C:/Users/berou1714/OneDrive - Norwegian University of Life Sciences/Desktop/PhD/"

# source all functions
list.files(path = paste0(source_path, "GMBA_project/Functions"), pattern = "*.R", full.names = TRUE) %>%
  purrr::walk(source)

## -----------------------------
#  2. Load & clean the dataset
## -----------------------------

# Import Lotta's checklist
Lotta_checklist <- readxl::read_xlsx(paste0(source_path, "GMBA_project/Lotta_files/vertebrate_data/vertebrate_data_Benjamin.xlsx"))

## ------------------------------------------
#  3. Compare my checklist with Lotta's one
## ------------------------------------------
# Let's work with reptiles as an exemple here
# Import my checklist
my_checklist <- readxl::read_xlsx(paste0(source_path, "GMBA_project/files_processed/Reptiles/reptiles_dataframe.xlsx"))

# keep only reptiles in Lotta's checklist
Lotta_reptiles <- Lotta_checklist %>%
  filter(group == "reptiles")

# now keep only the mountain ranges in mine that are in Lotta's
mountains_list <- Lotta_reptiles %>%
  distinct(Mountain_range) %>%
  pull(Mountain_range)
my_checklist <- my_checklist %>%
  filter(Mountain_range %in% mountains_list)

# I have ~ 2000 more species compared to Lotta's
# let's investigate a bit more

# We take the 'Albertine Rift Mountains' as an example
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

# We investigate a bit more: how many species have an overlap % > 1
species_diff %>%
  filter(overlap_pct > 1) %>%
  count()
# we have only 8 species, so that means that 136 species that are excluded in Lotta's
# checklist are because of the 1% overlap threshold
# These 8 species are probably a mistake from Lotta or me.

## ----------------------------
#  3.1. Plot the differences
## ----------------------------
p1 <- ggplot(species_diff, aes(x = reorder(sciname, -overlap_pct), y = overlap_pct,
                         fill = overlap_pct < 1, alpha = overlap_pct < 1)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "steelblue"),
                    guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 0.4, "FALSE" = 1), guide = "none") +
  geom_hline(yintercept = 1, col = "red", linetype = "dashed", linewidth = 0.8) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3)]) +  # generates indices (1, 3, 5, 7, ...)
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid = element_line(color = "grey95"),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(expand = expansion(mult = c(0.003, 0.05))) +
  labs(
    x = "Species",
    y = "Overlap %",
    subtitle = "Species excluded in the checklist"
  )
p1
ggsave(file = paste0(source_path, "Figures/species_diff1.svg"), plot = p1)

p2 <- ggplot(species_diff, aes(x = species_area, y = overlap_pct, col = overlap_pct < 1,
                              alpha = overlap_pct < 1)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "steelblue"), guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 0.4, "FALSE" = 1), guide = "none") +
  geom_hline(yintercept = 1, col = "red", linetype = "dashed") +
  ylim(0, 3) +
  theme_classic() +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 18)) +
  labs(x = "Species area", y = "Overlap %")
p2
ggsave(file = paste0(source_path, "Figures/species_diff2.svg"), plot = p2)

## ----------------------------------------------------
#  4. Extract the range maps of the 'difference list'
## ------------------------------------------------------
# I have now my species list, I will extract the range maps for these and investigate
reptiles_shapes <- sf::st_read(paste0(source_path, "GMBA_project/Raw_datasets/Reptiles/Distribution/doi_10_5061_dryad_9cnp5hqmb__v20220427/Gard_1_7_ranges.shp"), 
                               options = "ENCODING=ISO-8859-1") %>%
  st_make_valid()

reptiles_shapes <- reptiles_shapes %>%
  rename(sciname = binomial) %>%
  filter(sciname %in% species_diff$sciname)

## -------------------------------
#  4.1. Source the gmba regions
## -------------------------------
mountain_shapes03 <- sf::st_read(paste0(source_path, "GMBA_project/GMBA_mountains/mountain_shapes03/mountain_shapes03.shp")) %>%
  st_make_valid()

## -------------------------
#  4.2. Plot an example
## -------------------------
ggplot() +
  geom_sf(data = mountain_shapes03 %>% filter(Level_03 == "Albertine Rift Mountains"), fill = NA, color = "grey50") +
  geom_sf(data = reptiles_shapes %>% filter(sciname == "Bitis arietans"), fill = "lightblue", alpha = 0.6) +
  theme_minimal()


## ----------------------------------------------------
#    Rarefaction (True Range - GBIF) ~ n occurrences
## ----------------------------------------------------

# we will see what's the relationship between the "true range" from Lotta's checklist 
# and the GBIF estimates in function of the number of occurrences.
# For this, we're using again a rarefaction method (see function rarefaction.expert)

## --------------------
#  1. Run the function
## --------------------
# Run for each group
options(future.globals.maxSize = 20 * 1024^3)  # 20 GiB
plan(multisession, workers = 5)

groups <- c("reptiles", "mammals", "birds")

for (grp in groups) {

message(paste("---- Processing", grp, "----"))

checklist_group <- Lotta_checklist %>%
 filter(group == grp) %>%
 mutate(expert_range = max_elevation - min_elevation)

 parquet_path <- paste0(source_path, "GBIF_data/processed_files/", grp, "_gbif_parquet")

 results <- rarefaction.expert(checklist = checklist_group, replications = 1000,
                               parquet_path = parquet_path)

 write.csv(results, paste0(source_path, "GMBA_project/Outputs/rarefaction_TR_", 
                           grp, ".csv"),
           row.names = FALSE)

 rm(results)
 gc()
}

plan(sequential)


## -----------------------
#  2. Import the results
## -----------------------
rarefaction_TR <- read.csv(paste0(source_path, "GMBA_project/files_processed/Mammals/rarefaction_TR_mammals.csv"))

## --------------------------------
#  3. Clean & prepare the dataset
## --------------------------------
# We compute the total number of occurrences in each mountain range, useful later
mountain_occ <- rarefaction_TR %>%
  group_by(Mountain_range, sciname) %>%
  summarise(total_occ_spp = first(total_occ_spp),
            .groups = "drop") %>%
  group_by(Mountain_range) %>%
  summarise(n_tot = sum(total_occ_spp),
            .groups = "drop")

# we add the 'true ranges' from Lotta's checklist
rarefaction_TR <- rarefaction_TR %>%
  left_join(Lotta_checklist %>% select(sciname, Mountain_range, min_elevation, max_elevation),
            by = c("sciname", "Mountain_range")) %>%
  mutate(expert_range = max_elevation - min_elevation) %>%
  filter(expert_range != 0) %>%  # we remove 0 values
  filter(expert_range >= 50) %>%  # we remove too narrow ranges because they induce a strong mismatch with GBIF
  group_by(Mountain_range, subsample_occ) %>%
  mutate(n_sp = n_distinct(sciname)) %>%  # Count the number of species per subsample
  ungroup()

# We categorize the ranges according to their size: small, medium, large (based on Lotta's range size)
rarefaction_TR <- rarefaction_TR %>%
  mutate(
    range_cat = case_when(expert_range < 500 ~ "small",
                          expert_range >= 500 & expert_range < 1000 ~ "medium",
                          expert_range >= 1000 ~ "large",
                          TRUE ~ "undefined")
  )

# we sort the quantile pairs as factors
rarefaction_TR <- rarefaction_TR %>%
  mutate(quantile_pair = factor(paste0(low_q, "-", high_q), 
                                levels = c("0-100", "1-99", "2-98", "3-97", "4-96", "5-95",
                                           "10-90", "15-85", "20-80")))

# We group per subsample and quantile to compute the median value across all MR & species
rarefaction_TR <- rarefaction_TR %>%
  group_by(subsample_occ, quantile_pair) %>%
  mutate(
    median_offset = median(mean_offset_range, na.rm = TRUE),    # moved here too
    lower = quantile(mean_offset_range, 0.2, na.rm = TRUE),
    upper = quantile(mean_offset_range, 0.8, na.rm = TRUE)
  ) %>%
  ungroup()

## --------------------
#  4. PLOTS
## --------------------
quantile_levels <- unique(rarefaction_TR$quantile_pair)
quantile_colors <- setNames(
  colorRampPalette(c("#563635", "#5b6057", "#6e9075", "#78c091"))(length(quantile_levels)),
  quantile_levels
)
mountain_colors <- setNames(
  colorRampPalette(c("#563635", "#5b6057", "#6e9075", "#78c091"))(n_distinct(rarefaction_TR$Mountain_range)),
  unique(rarefaction_TR$Mountain_range)
)

## --- Convergence of observed range to true range with increasing number of occurrences
# Median across all MR and species
p1 <- rarefaction_TR %>%
  filter(mean_offset_range >= lower, mean_offset_range <= upper) %>%
  mutate(subsample_occ_jit = subsample_occ + as.numeric(quantile_pair) * 1) %>%
  ggplot(aes(x = subsample_occ_jit, y = mean_offset_range, fill = quantile_pair, 
             colour = quantile_pair, group = quantile_pair)) +
  geom_point(size = 1, alpha = 0.1) +
  geom_borderline(aes(y = median_offset), linewidth = 1.5) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 0.5) +
  theme_minimal() +
  scale_fill_manual(values = quantile_colors) +
  scale_colour_manual(values = quantile_colors) +
  theme(legend.position = "bottom") +
  labs(x = "Subsample level (occurrences/species)",
       y = "GBIF range / Expert range")
p1
ggsave(file = paste0(source_path, "Figures/rangeTR1.png"), plot = p1, dpi = 300)

# --- Number of species per subsample level
p2 <- rarefaction_TR %>%
  group_by(subsample_occ) %>%
  summarise(n_species = n_distinct(sciname)) %>%
  ggplot(aes(x = subsample_occ, y = n_species, fill = n_species)) +
  geom_col(alpha = 0.8) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18)) +
  scale_x_continuous(expand = expansion(mult = c(0.005, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.002, 0.05))) +
  labs(x = "Subsample level (occurrences/species)",
       y = "Number of species")
p2
ggsave(file = paste0(source_path, "Figures/rangeTR2.png"), plot = p2, dpi = 300)

# -- Difference between observed and true limits for both high and low elevational range limits
p3 <- rarefaction_TR %>%
  group_by(subsample_occ, quantile_pair) %>%
  filter(mean_delta_max >= quantile(mean_delta_max, 0.1, na.rm = TRUE),
         mean_delta_max <= quantile(mean_delta_max, 0.9, na.rm = TRUE)) %>%
  mutate(mean_offset = mean(mean_delta_max, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = subsample_occ, y = mean_delta_max, fill = quantile_pair,
             colour = quantile_pair, group = quantile_pair)) +
  geom_point(size = 1, alpha = 0.1) +
  geom_borderline(aes(y = mean_offset), linewidth = 1.5, bordercolour = "black",
                  borderwidth = 0.1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.5) +
  theme_minimal() +
  scale_fill_manual(values = quantile_colors) +
  scale_colour_manual(values = quantile_colors) +
  theme(legend.position = "bottom") +
  labs(x = "Subsample level (occurrences/species)",
       y = "Difference between observed and high limits")
p3
ggsave(file = paste0(source_path, "Figures/highlim_mammals.png"), plot = p3, dpi = 300)

# -- Variation of the 'deltas' with the number of occurrences
p4 <- rarefaction_TR %>%
  group_by(subsample_occ, quantile_pair) %>%
  mutate(sd_delta_min = mean(sd_delta_min, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = subsample_occ, colour = quantile_pair, group = quantile_pair, fill = quantile_pair)) +
  geom_ribbon(aes(ymin = 0 - sd_delta_min, 
                  ymax = 0 + sd_delta_min), 
              alpha = 0.30, color = NA) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.5) +
  ylim(c(-400, 400)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  scale_colour_manual(values = quantile_colors) +
  scale_fill_manual(values = quantile_colors) +
  labs(x = "Subsample level (occurrences/species)", 
       y = "variability in range limit difference (low limit, ±SD)")
p4
ggsave(file = paste0(source_path, "Figures/lowlim_var.png"), plot = p4, dpi = 300)

# ---- Same, but for each range size category (small, medium, large)
rarefaction_TR <- rarefaction_TR %>%
  group_by(subsample_occ, quantile_pair, range_cat) %>%
  mutate(
    median_offset = median(mean_offset_range, na.rm = TRUE),    # moved here too
    lower = quantile(mean_offset_range, 0.2, na.rm = TRUE),
    upper = quantile(mean_offset_range, 0.8, na.rm = TRUE)
  ) %>%
  ungroup()

p1 <- rarefaction_TR %>%
  filter(mean_offset_range >= lower, mean_offset_range <= upper) %>%
  filter(range_cat == "large") %>%
  mutate(subsample_occ_jit = subsample_occ + as.numeric(quantile_pair) * 1) %>%
  ggplot(aes(x = subsample_occ_jit, y = mean_offset_range, fill = quantile_pair, 
             colour = quantile_pair, group = quantile_pair)) +
  geom_point(size = 1, alpha = 0.1) +
  geom_borderline(aes(y = median_offset), linewidth = 1.5) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 0.5) +
  theme_classic() +
  scale_fill_manual(values = quantile_colors) +
  scale_colour_manual(values = quantile_colors) +
  theme(legend.position = "bottom",
        axis.line = element_line(linewidth = 0.7),
        axis.ticks = element_line(linewidth = 0.7),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12, margin = margin(t = 15)),
        axis.title.y = element_text(size = 12, margin = margin(r = 15))) +
  scale_x_continuous(expand = expansion(mult = c(0.005, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.002, 0.05))) +
  labs(x = "Subsample level (occurrences/species)",
       y = "GBIF range / Expert range",
       subtitle = "Range category: large")
p1
ggsave(file = paste0(source_path, "Figures/rangeTR_large1.png"), plot = p1, dpi = 300)

p2 <- rarefaction_TR %>%
  filter(range_cat == "large") %>%
  group_by(subsample_occ) %>%
  summarise(n_species = n_distinct(sciname)) %>%
  ggplot(aes(x = subsample_occ, y = n_species, fill = n_species)) +
  geom_col(alpha = 0.8) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18)) +
  scale_x_continuous(expand = expansion(mult = c(0.005, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.002, 0.05))) +
  labs(x = "Subsample level (occurrences/species)",
       y = "Number of species")
p2
ggsave(file = paste0(source_path, "Figures/rangeTR_large2.png"), plot = p2, dpi = 300)

# --------
# Now we look at the relationship between the GBIF/expert and the total number of
# occurrences for a defined species, and for a defined N subsampled
# We want to see if the total N have an impact on our rarefaction
# We can do this in general (all quantiles) or for each quantiles specifically

p <- rarefaction_TR %>%
  filter(subsample_occ %in% c(2, 5, 10, 15, 20, 30, 50, 70, 100, 140, 200)) %>%
  filter(quantile_pair == "20-80") %>%
  #filter(Mountain_range %in% (mountain_occ %>%
                                #filter(n_tot > 30000) %>%
                                #pull(Mountain_range))) %>%
  mutate(subsample_occ = factor(subsample_occ)) %>%
  ggplot(aes(x = total_occ_spp, y = mean_offset_range, color = subsample_occ, fill = subsample_occ)) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 0.5) +
  geom_point(size = 0.8, alpha = 0.2) +
  geom_smooth(method = "gam", linewidth = 1.3, alpha = 0.2) +
  #geom_smooth(method = "lm", linewidth = 1.3, alpha = 0.2) +
  coord_cartesian(ylim = c(0, 2)) +
  #facet_wrap(~Mountain_range, scales = "free") +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  scale_x_log10(expand = expansion(mult = c(0.005, 0.05))) +
  theme_classic() +
  theme(axis.line = element_line(linewidth = 0.7),
        axis.ticks = element_line(linewidth = 0.7),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12, margin = margin(t = 15)),
        axis.title.y = element_text(size = 12, margin = margin(r = 15))) +
  scale_y_continuous(expand = expansion(mult = c(0.002, 0.05))) +
  labs(x = "Total number of occurrences for the species",
       y = "GBIF range / expert range",
       color = "N subsampled", fill = "N subsampled",
       subtitle = "Quantile: 20-80")
p
ggsave(file = paste0(source_path, "Figures/rangeTR7.png"), plot = p3, dpi = 300)
