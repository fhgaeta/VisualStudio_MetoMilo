#plot single area - Hardanger 14.1 Hardbunnskorallskog
#Finished, only octocoral available in Hardanger

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(leaflet)
library(terra)

#Define folder
Sys.setlocale("LC_ALL", "en_US.UTF-8")
#user <- Sys.getenv("USERNAME")

basefolder_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/"
sub_teams <- "C:/Users/FEG/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/"
sub_dir <- paste0("C:/Users/FEG/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/Focus areas/")
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output"

# read data - Only hornkoraller tilgjengelig i Hardanger
file_Hornkorall <- paste0(basefolder_od, 
               "/GIS Data/Ecological components data/17. Horn corals/17.1 Hornkoraller_FGDB.gdb")

  # Load the saved .rds files
  shp_5109_02_03 <- readRDS(paste0(folder_output_od, "shp_1108_1000.rds"))
  shp_water <- readRDS(paste0(folder_output_od, "shp_water.rds"))
  rw_shp <- readRDS(paste0(folder_output_od, "rw_shp.rds"))

# load the 200m study area mask as a shapefile in raster format
rast_100_hard <- terra::rast(paste0(basefolder_od,"Focus areas/grid_mask/Raster/5109_02_03.tif"))

# read octocoral data
db_layers <- sf::st_layers(file_Hornkorall)$name
shp_octocoral_all <- purrr::map(
  db_layers, sf::st_read, dsn = file_Hornkorall, quiet = TRUE
) %>% bind_rows()

# get the projection from the geonorge layer - we will use this as the basis for our work
crs_proj <- sf::st_crs(shp_water)

#Load study area polygons
Study_areas <- read_sf(paste0(sub_dir, "Focus areas_boundary.shp"))

#The polygons are defined in lat/long coordinates. Now we want to transform them to UTM33 projected coordinates.
Study_areas_proj <- Study_areas %>%
  sf::st_transform(crs=crs_proj)
shp_octocoral_proj <- shp_octocoral_all %>%
  sf::st_transform(crs=crs_proj)
shp_water_proj <- shp_water %>%
  sf::st_transform(crs=crs_proj)

# Subarea of study area
shp_area_2 <- Study_areas_proj %>%
  filter(vannregion=="5109")

# intersection of datasett and area polygons will give us points within the study areas
shp_octoral_inters <- sf::st_intersection(
  shp_octocoral_proj, 
  shp_area_2)

# select only "art" in our category: "hardbunnskorallskog".
# make the dataset names a factor. this is a minor detail.
# it will allow us to keep the same colours for different species
# across regions / study areas
octocoral_species_NO <- unique(shp_octoral_inters$art) %>% sort()
shp_octoral_inters$art <- factor(shp_octoral_inters$art,
                                         levels=octocoral_species_NO)

#plot the results
ggplot() +
  geom_sf(data = shp_5109_02_03, fill = "#69d9f5", colour = NA, alpha = 0.2) +
  geom_sf(data = shp_octoral_inters, aes(colour = art)) +
  coord_sf() +
  scale_colour_discrete(drop = F, name = "art") +
  guides(colour = guide_legend(ncol = 1, bycol = T)) +
  theme_minimal() +
  labs(subtitle = "Hardbunnskorallskog - Hardanger")


# get the region
plot_title <- paste0("Vannregion: ", shp_area_2$vannregion[1])

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/14. Benthos/"

#plot the results for octocoral
p1 <- ggplot() +
  geom_sf(data = shp_5109_02_03, fill = "#69d9f5", colour = NA, alpha = 0.2) +
  geom_sf(data = shp_area_2, fill = NA, colour = "grey", alpha = 0.2, linewidth = 0.4) +
  geom_sf(data = shp_octoral_inters, aes(colour = art)) +
  coord_sf() +
  scale_colour_discrete(drop = F, name = "art") +
  guides(colour = guide_legend(ncol = 1, bycol = T)) +
  theme_minimal() +
  labs(subtitle = "Hardbunnskorallskog - Hardanger")

# Save the vectorial plot for octocoral
ggsave(p1, filename = paste0(folder_output_od, "14.1 Hardbunnskorallskog_Hardanger_vectorial_", shp_area_2$vannregion[1], ".png"),
             dpi = 300, height = 20, width = 20, units = "cm", bg = "white")

# Transform the data to raster format

# List of species to rasterize
species_list <- shp_octoral_inters %>%
  filter(art %in% c("Paragorgia arborea",
                   "Primnoa resedaeformis", 
                    "Paramuricea placomus")) %>%
  distinct(art) %>%
  pull(art) %>%
  as.character()

# Loop through each species
for (species_to_rasterize in species_list) {
 cat(paste0(species_to_rasterize, "\n")) 
  
  # Filter the shapefile for the current species "testspec"
 shp_octoral_inters_selection <- shp_octoral_inters %>%
    filter(art %in% c("Paragorgia arborea", "Primnoa resedaeformis", "Paramuricea placomus"))

  # Rasterize the filtered data
  rasterized_species <- terra::rasterize(
    shp_octoral_inters_selection, rast_100_hard, field = "art", fun = "count"
  )

  df <- terra::xyFromCell(rasterized_species, 1:ncell(rasterized_species)) %>% as.data.frame()
  df$val <- terra::values(rasterized_species)
  df <- df %>%
    filter(!is.na(val))

  

  # Convert the rasterized data to polygons for visualization
  rasterized_species_shp <- terra::as.polygons(rasterized_species, aggregate = FALSE) %>%
    sf::st_as_sf()

  # Create a ggplot object
  plot <- ggplot() +
    geom_sf(data = rasterized_species_shp, colour = NA, alpha = 0.8) +
    ggtitle(paste("Rasterized", species_to_rasterize)) +
    theme_minimal() +
    geom_sf(data = shp_water, colour = NA, fill = "lightblue", alpha = 0.5) +
    coord_sf(xlim = st_bbox(shp_area_2)[c("xmin", "xmax")], ylim = st_bbox(shp_area_2)[c("ymin", "ymax")], datum = 25833) +
    scale_fill_discrete(name = "Hardbunnskorallskog art") +
    geom_point(data = df, aes(x = x, y = y), color = "red")

  # Define the folder path
  folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output_final/"

  # Save the plot
  ggsave(
    plot,
    filename = paste0(
      folder_output_od, 
      "14.1 Hardbunnskorallskog_Hardanger_raster_200_", 
      species_to_rasterize, 
      ".png"
    ),
    dpi = 300, height = 20, width = 20, units = "cm", bg = "white"
  )
}
