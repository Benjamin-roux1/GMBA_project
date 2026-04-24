
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
standardize.species.names <- function(gbif_data, litterature_data) {
  
  # 1. Match species names to GBIF backbone
  matched_gbif <- match.backbone(gbif_data)
  matched_litt <- match.backbone(litterature_data)
  
  # 2. Clean and standardize names in both datasets
  gbif_clean <- clean.names(gbif_data, matched_gbif)
  litt_clean <- clean.names(litterature_data, matched_litt)
  
  # 3. Keep only species present in both datasets
  gbif_final <- gbif_clean %>%
    semi_join(litt_clean, by = "sciname")
  
  message("Species in GBIF after cleaning: ", n_distinct(gbif_clean$sciname))
  message("Species in literature after cleaning: ", n_distinct(litt_clean$sciname))
  message("Species in common: ", 
          n_distinct(semi_join(litt_clean, gbif_final, by = "sciname")$sciname))
  
  return(list(
    gbif = gbif_clean,
    litterature = litt_clean
  ))
}
