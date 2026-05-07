
library(dplyr)
library(sf)
library(ggplot2)
library(tidyterra)
library(tidyr)
library(terra)
library(ggnewscale)

# define data path OR even config.R file with libraries & path
source_path <- "C:/Users/berou1714/OneDrive - Norwegian University of Life Sciences/Desktop/PhD_project/"

# source all functions
list.files(path = paste0(source_path, "GMBA_project/Functions"), pattern = "*.R", full.names = TRUE) %>%
  purrr::walk(source)

#source the gmba regions
mountain_shapes <- sf::st_read(paste0(source_path, "GMBA_project/GMBA_mountains/GMBA_Inventory_v2.0_standard_300/GMBA_Inventory_v2.0_standard_300.shp")) %>%
  st_make_valid()

# Group by Level_03 (scale of mountain system chosen)
{
  sf_use_s2(FALSE)
  mountain_shapes03 <- mountain_shapes %>%
    st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
    st_make_valid() %>%
    group_by(Level_01, Level_02, Level_03) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    st_make_valid()
  sf_use_s2(TRUE)
  }
# so we have 137 distinct combinations level_02 - level_03, i.e 137 mountain systems

# A bit of cleaning the mountain dataframe
# some rows are not defined at Level03 or Level02
# in these cases, we fill the NA with the closest filled upper level
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

# ------------
# PLOT

dem <- terra::rast(paste0(source_path, "DEM/CHELSA_dem_latlong.tif"))
demmask <- dem
demmask[dem>0] <- 1
demmask[dem<=0] <- NA
dem_n <- dem * demmask 

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

test_mountain <- mountain_shapes03[70, ]
bbox <- sf::st_bbox(test_mountain)
dem_crop <- terra::crop(
  dem_n,
  terra::ext(
    bbox["xmin"]-2,
    bbox["xmax"]+2,
    bbox["ymin"]-2,
    bbox["ymax"]+2
  )
)

world_crop <- sf::st_crop(world, terra::ext(
  bbox["xmin"] - 2,
  bbox["xmax"] + 2,
  bbox["ymin"] - 2,
  bbox["ymax"] + 2
))

slope  <- terra::terrain(dem_crop, v = "slope", unit = "radians")
aspect <- terra::terrain(dem_crop, v = "aspect", unit = "radians")
hillshade <- terra::shade(slope, aspect, angle = 10, direction = 270)

ggplot() +
  
  geom_spatraster(data = hillshade) +
  scale_fill_gradientn(
    colours = grey.colors(100, start = 0.9, end = 0.3),
    guide = "none"
  ) +
  
  new_scale_fill() +
  
  geom_spatraster(data = dem_crop, alpha = 0.4) +
  scale_fill_gradientn(
    colours = grey.colors(100, start = 0.9, end = 0.3)
  ) +
  
  # country borders
  geom_sf(data = world_crop, fill = NA, color = "white", linewidth = 0.1) +
  
  # mountain
  geom_sf(data = test_mountain, fill = "red", alpha = 0.7, color = "black", linewidth = 0.1) +
  
  # country labels (cleaner subset)
  geom_sf_text(
    data = world_crop,
    aes(label = name),
    size = 3, fontface = "bold",
    color = "grey20"
  ) +
  
  coord_sf(
    xlim = c(bbox["xmin"]-2, bbox["xmax"]+2),
    ylim = c(bbox["ymin"]-2, bbox["ymax"]+2),
    expand = TRUE
  ) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        plot.subtitle = element_text(face = "bold.italic"),
        panel.grid = element_line(linewidth = 0.5)) +
  labs(subtitle = test_mountain$Level_03)


# Loop over all level_03 of GMBA

for (i in 1:nrow(mountain_shapes03)) {
  
  mountain_shapes <- mountain_shapes03[i, ]
  
  bbox <- sf::st_bbox(mountain_shapes)
  
  bbox_buf <- terra::ext(
    bbox["xmin"] - 2,
    bbox["xmax"] + 2,
    bbox["ymin"] - 2,
    bbox["ymax"] + 2
  )
  
  dem_crop <- terra::crop(dem_n, bbox_buf)

  world_crop <- sf::st_crop(world, bbox_buf)
  
  slope  <- terra::terrain(dem_crop, v = "slope", unit = "radians")
  aspect <- terra::terrain(dem_crop, v = "aspect", unit = "radians")
  hillshade <- terra::shade(slope, aspect, angle = 10, direction = 270)
  
  p <- ggplot() +
    
    geom_spatraster(data = hillshade) +
    scale_fill_gradientn(
      colours = grey.colors(100, start = 1, end = 0.1),
      guide = "none", na.value = "transparent"
    ) +
    
    new_scale_fill() +
    
    geom_spatraster(data = dem_crop, alpha = 0.4) +
    scale_fill_gradientn(
      colours = grey.colors(100, start = 1, end = 0.1),
      na.value = "transparent"
    ) +
    
    # country borders
    geom_sf(data = world_crop, fill = NA, color = "white", linewidth = 0.1) +
    
    # mountain
    geom_sf(data = mountain_shapes, fill = "red", alpha = 0.7, color = "black", linewidth = 0.1) +
    
    # country labels (cleaner subset)
    geom_sf_text(
      data = world_crop,
      aes(label = name),
      size = 3, fontface = "bold",
      color = "grey20"
    ) +
    
    coord_sf(
      xlim = c(bbox["xmin"]-2, bbox["xmax"]+2),
      ylim = c(bbox["ymin"]-2, bbox["ymax"]+2),
      expand = TRUE
    ) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          legend.position = "none",
          plot.subtitle = element_text(face = "bold.italic"),
          panel.grid = element_line(linewidth = 0.1)) +
    labs(subtitle = mountain_shapes$Level_03)
  
  ggsave(
    filename = paste0("map_", mountain_shapes$Level_03, ".png"),
    plot = p,
    path = file.path(source_path, "GMBA_project/maps"),
    dpi = 300,
    width = 8,
    height = 6
  )
}
