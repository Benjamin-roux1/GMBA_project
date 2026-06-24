# Function to union ranges if species has more than one range AND the same seasonal code
union.ranges <- function(duplicated_species, species_shapes) {
  
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(TRUE))
  
  # Standardize geometry column name to 'geometry'
  if (attr(species_shapes, "sf_column") != "geometry") {
    species_shapes <- species_shapes %>%
      rename(geometry = !!attr(species_shapes, "sf_column")) %>%
      st_set_geometry("geometry")
  }
  
  # union the duplicated species
  species_unioned <- species_shapes %>% 
    filter(sciname %in% duplicated_species) %>%
    st_make_valid() %>%
    group_by(sciname, seasonal) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    st_make_valid()
  
  # keep non-duplicated species as is
  species_single <- species_shapes %>%
    filter(!sciname %in% duplicated_species) %>%
    st_make_valid()
  
  # combine both
  species_final <- bind_rows(species_single, species_unioned)
  
  return(species_final)
}