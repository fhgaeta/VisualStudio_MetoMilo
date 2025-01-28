#plot single area - Salten 14.2 Bløttbunnskorallskog
#både 19 Sårbare marine biotoper - bløtbunnskorallskog og 17 hornkoraller eksisterer
#valgte å plotte i raster kun 17 hornkoraller fordi de overlapper med 19, men 17 er på artsnivå.

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

# read data 
file_Hornkorall <- paste0(basefolder_od, 
               "/GIS Data/Ecological components data/17. Horn corals/17.1 Hornkoraller_FGDB.gdb")
# Define the path to the gdb file
file_path_gdb_smb <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/19. Sårbare marine biotoper/19.2 SarbareMarineBunndyrObs_FGDB/SarbareMarineBunndyrObs_FGDB.gdb"

  # Load the saved .rds files
  shp_1108 <- readRDS(paste0(folder_output_od, "shp_1108_1000.rds"))
  shp_water <- readRDS(paste0(folder_output_od, "shp_water.rds"))
  rw_shp <- readRDS(paste0(folder_output_od, "rw_shp.rds"))

# load the 200m study area mask as a shapefile in raster format
rast_200_salt <- terra::rast(paste0(basefolder_od,"Focus areas/grid_mask/Raster/Nord-Salten_1108_08.tif"))

# read octocoral data
db_layers <- sf::st_layers(file_Hornkorall)$name
shp_octocoral_all <- purrr::map(
  db_layers, sf::st_read, dsn = file_Hornkorall, quiet = TRUE
) %>% bind_rows()

#read gdb file DN-19 Salten - PRESENT IN SALTEN 
gdb_smh_obs_salten <- read_sf(file_path_gdb_smb)

#read gdb file Coral garden - hardbottom - NOT PRESENT IN SALTEN
gdb_coral_garden <- read_sf(file_path_gdb_cor_gard)

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
gdb_smh_obs_salten_proj <- gdb_smh_obs_salten %>%
  sf::st_transform(crs = crs_proj)
gdb_coral_garden_proj <- gdb_coral_garden %>%
  sf::st_transform(crs = crs_proj)

# Subarea of study area
shp_area_3 <- Study_areas_proj %>%
  filter(vannregion=="1108")

# intersection of datasett and area polygons will give us points within the study areas
shp_octoral_inters <- sf::st_intersection(
  shp_octocoral_proj, 
  shp_area_3)

# select only "art" in our category: "hardbunnskorallskog".
# make the dataset names a factor. this is a minor detail.
# it will allow us to keep the same colours for different species
# across regions / study areas
octocoral_species_NO <- unique(shp_octoral_inters$art) %>% sort()
shp_octoral_inters$art <- factor(shp_octoral_inters$art,
                                         levels=octocoral_species_NO)

#plot the results
ggplot() +
  geom_sf(data = shp_1108, fill = "#69d9f5", colour = NA, alpha = 0.2) +
  geom_sf(data = shp_octoral_inters, aes(colour = art)) +
  coord_sf() +
  scale_colour_discrete(drop = F, name = "art") +
  guides(colour = guide_legend(ncol = 1, bycol = T)) +
  theme_minimal() +
  labs(subtitle = "14.2 Bløttbunnskorallskog - Salten")

# intersection of datasett and area polygons will give us points within the study areas
gdb_smh_obs_salten_intersect <- sf::st_intersection(
  gdb_smh_obs_salten_proj,
  shp_area_3
)
# NO DATA FOR SMB IN SALTEN AS EXPECTED

# intersection of datasett and area polygons will give us points within the study areas
gdb_coral_garden_proj_intersect <- sf::st_intersection(
  gdb_coral_garden_proj,
  shp_area_3
)
#NO DATA FOR CORAL GARDEN IN SALTEN AS EXPECTED

# make the dataset names a factor. this is a minor detail.
# it will allow us to keep the same colours for different species
coralgarden_species_NO <- unique(gdb_coral_garden_proj_intersect$art) %>% sort()
gdb_coral_garden_proj_intersect$art <- factor(gdb_coral_garden_proj_intersects$art,
                                         levels=coralgarden_species_NO )

# get the region
plot_title <- paste0("Vannregion: ", shp_area_3$vannregion[1])

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/14. Benthos/"

#plot the results for octocoral
p1 <- ggplot() +
  geom_sf(data = shp_1108, fill = "#69d9f5", colour = NA, alpha = 0.2) +
  geom_sf(data = shp_area_3, fill = NA, colour = "grey", alpha = 0.2, linewidth = 0.4) +
  geom_sf(data = shp_octoral_inters, aes(colour = art)) +
  coord_sf() +
  scale_colour_discrete(drop = F, name = "art") +
  guides(colour = guide_legend(ncol = 1, bycol = T)) +
  theme_minimal() +
  labs(subtitle = "14.2 Bløttbunnskorallskog - Salten")

# Save the vectorial plot for octocoral
ggsave(p1, filename = paste0(folder_output_od, "14.2 Bløttbunnskorallskog_Salten_vectorial_", shp_area_3$vannregion[1], ".png"),
             dpi = 300, height = 20, width = 20, units = "cm", bg = "white")

# Plot the results for coral garden
ggplot() +
    geom_sf(data = shp_1108, fill = "#69d9f5", colour = NA, alpha = 0.2) +
    geom_sf(data = shp_area_3, fill = NA, colour = "grey", alpha = 0.2, linewidth = 0.4) +
    geom_sf(data = gdb_coral_garden_proj_intersect, aes(colour = "sarbarhabitat")) +
    coord_sf() +
    scale_colour_discrete(drop = F, name = "art") +
    guides(colour = guide_legend(ncol = 1, bycol = T)) +
    theme_minimal() +
    labs(subtitle = paste0("Coral Garden - ", plot_title))

# Save the vectorial plot for coral garden
ggsave(p2, filename = paste0(folder_output_od, "14.2 CoralGarden_Salten_vectorial_", shp_area_3$vannregion[1], ".png"),
             dpi = 300, height = 20, width = 20, units = "cm", bg = "white")

# Plot the results for SMH observations
ggplot() +
    geom_sf(data = shp_1108, fill = "#69d9f5", colour = NA, alpha = 0.2) +
    geom_sf(data = shp_area_3, fill = NA, colour = "grey", alpha = 0.2, linewidth = 0.4) +
    geom_sf(data = gdb_smh_obs_salten_intersect, aes(colour = "Naturtype")) +
    coord_sf() +
    scale_colour_discrete(drop = F, name = "art") +
    guides(colour = guide_legend(ncol = 1, bycol = T)) +
    theme_minimal() +
    labs(subtitle = paste0("SMH Observations - ", plot_title))

# Save the vectorial plot for SMH observations
ggsave(p3, filename = paste0(folder_output_od, "14.2 SMHObservations_Salten_vectorial_", shp_area_3$vannregion[1], ".png"),
             dpi = 300, height = 20, width = 20, units = "cm", bg = "white")


# Transform the bird data to raster format

# List of species to rasterize
species_list <- shp_octoral_inters %>%
  filter(art %in% c("Isidella lofotensis",
                   "Radicipes gracilis")) %>%
  distinct(art) %>%
  pull(art) %>%
  as.character()

# Loop through each species
for (species_to_rasterize in species_list) {
 cat(paste0(species_to_rasterize, "\n")) 
  
  # Filter the shapefile for the current species "testspec"
 shp_octoral_inters_selection <- shp_octoral_inters %>%
    filter(art %in% c("Isidella lofotensis",
                   "Radicipes gracilis"))

  # Rasterize the filtered data
  rasterized_species <- terra::rasterize(
    shp_octoral_inters_selection, rast_200_salt, field = "art", fun = "count"
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
    coord_sf(xlim = st_bbox(shp_area_3)[c("xmin", "xmax")], ylim = st_bbox(shp_area_3)[c("ymin", "ymax")], datum = 25833) +
    scale_fill_discrete(name = "14.2 Bløttbunnskorallskog art") +
    geom_point(data = df, aes(x = x, y = y), color = "red")

  # Define the folder path
  folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output_final/"

  # Save the plot
  ggsave(
    plot,
    filename = paste0(
      folder_output_od, 
      "14.2 Bløttbunnskorallskog_Salten_raster_200_", 
      species_to_rasterize, 
      ".png"
    ),
    dpi = 300, height = 20, width = 20, units = "cm", bg = "white"
  )
}
