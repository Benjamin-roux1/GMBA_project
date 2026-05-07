
# Here, we use the function rgbif::name_backbone_checklist to standardize both GBIF and literature with the same procedure
# The function standardize.species.names() follow the following procedure:
#   1. for both gbif and literature species list, it return a dataframe with original names and corrected names
#   2. in both dataset, it removes the species names flagged as unsufficiently accurate
#   3. in both dataset, it replaces the original species names by the "true ones" from the rgbif function
#   4. then it join both dataset, keeping in the gbif dataset only species found in the literature

# The return is the gbif cleaned version, with standardized species names and only species found in our literature dataset


# Helper to match species names to GBIF backbone
match.backbone <- function(data) {
  data %>%
    distinct(sciname) %>%
    pull(sciname) %>%
    rgbif::name_backbone_checklist()
}

# Helper function to clean and standardize species names
clean.names <- function(data, matched) {
  
  to_remove <- matched %>%
    filter(matchType %in% c("HIGHERRANK", "FUZZY", "NONE")) %>%
    pull(verbatim_name)
  
  data %>%
    filter(!sciname %in% to_remove) %>%
    left_join(
      matched %>% select(verbatim_name, canonicalName),
      by = c("sciname" = "verbatim_name")
    ) %>%
    mutate(sciname = ifelse(!is.na(canonicalName), canonicalName, sciname)) %>%
    select(-canonicalName)
}

# Main function to standardize species names across both datasets
standardize.species.names <- function(gbif_data, literature_data) {
  
  # 1. Match species names to GBIF backbone
  matched_gbif <- match.backbone(gbif_data)
  matched_lit <- match.backbone(literature_data)
  
  # 2. Clean and standardize names in both datasets
  gbif_clean <- clean.names(gbif_data, matched_gbif)
  lit_clean <- clean.names(literature_data, matched_lit)
  
  # 3. Keep only species present in both datasets
  gbif_final <- gbif_clean %>%
    semi_join(lit_clean, by = "sciname")
  
  # 4. Build a name mapping table to update the base dataframe
  name_mapping <- matched_lit %>%
    filter(!matchType %in% c("HIGHERRANK", "FUZZY", "NONE")) %>%
    select(verbatim_name, canonicalName) %>%
    filter(!is.na(canonicalName))
  
  message("Species removed from GBIF: ", n_distinct(gbif_data$sciname) - n_distinct(gbif_clean$sciname))
  message("Species removed from literature: ", n_distinct(literature_data$sciname) - n_distinct(lit_clean$sciname))
  message("Species in GBIF not in literature: ", n_distinct(gbif_clean$sciname) - n_distinct(gbif_final$sciname))
  message("Species in common: ", n_distinct(semi_join(lit_clean, gbif_final, by = "sciname")$sciname))
  
  return(list(
    gbif_clean = gbif_clean,
    literature = lit_clean,
    gbif_final = gbif_final,
    name_mapping = name_mapping
  ))
}
