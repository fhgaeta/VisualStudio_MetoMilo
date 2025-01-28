#plot single area 3 - Jæren. 14.1 Hardbunnskorallskog

# Missing only input from Astrid regarding the species to be included in the analysis.

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

  # Load the saved .rds files
  shp_5104 <- readRDS(paste0(folder_output_od, "shp_5104_1000.rds"))
  shp_water <- readRDS(paste0(folder_output_od, "shp_water.rds"))
  rw_shp <- readRDS(paste0(folder_output_od, "rw_shp.rds"))

# load the 100m study area mask as a shapefile in raster format
rast_100_jeren <- terra::rast(paste0(basefolder_od,"Focus areas/grid_mask/Raster/Jæren_5104_01.tif"))

# read data
file_path_gdb_cor_gard <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/21. Marine grunnkart - Sårbare habitater/SarbareMarineHabitater_FGDB.gdb"
#read gdb file Coral garden - hardbottom - ONLY ONE PRESENT IN jeren
gdb_coral_garden <- read_sf(file_path_gdb_cor_gard)

# get the projection from the geonorge layer - we will use this as the basis for our work
crs_proj <- sf::st_crs(shp_water)

#Load study area polygons
Study_areas <- read_sf(paste0(sub_dir, "Focus areas_boundary.shp"))

#The polygons are defined in lat/long coordinates. Now we want to transform them to UTM33 projected coordinates.
Study_areas_proj <- Study_areas %>%
  sf::st_transform(crs=crs_proj)
shp_smb_proj <- gdb_coral_garden %>%
  sf::st_transform(crs=crs_proj)
shp_water_proj <- shp_water %>%
  sf::st_transform(crs=crs_proj)

  # Subarea of study area
shp_area_1 <- Study_areas_proj %>%
  filter(vannregion=="5104")


# intersection of datasett and area polygons will give us points within the study areas
shp_smb_proj_inters <- sf::st_intersection(
  shp_smb_proj, 
  shp_area_1)
# make the bird names a factor. this is a minor detail.
# it will allow us to keep the same colours for different species
# across regions / study areas
coralgarden_species_NO <- unique(shp_smb_proj_inters$sarbarthabitat) %>% sort()
shp_smb_proj_inters$sarbarthabitat <- factor(shp_smb_proj_inters$sarbarthabitat,
                                         levels=coralgarden_species_NO)

# get the region
plot_title <- paste0("Vannregion: ", shp_area_1$vannregion[1])

#plot the results
p1 <- ggplot() +
  geom_sf(data=shp_5104, fill="#69d9f5", colour=NA, alpha=0.2) +
  geom_sf(data=shp_area_1, fill=NA, colour="grey", alpha=0.2, linewidth=0.4) +
  geom_sf(data=shp_smb_proj_inters, aes(colour=sarbarthabitat)) +
  coord_sf() +
  scale_colour_discrete(drop=F, name="sarbarthabitat") +
  guides(colour = guide_legend(ncol = 1, bycol = T)) +
  theme_minimal() +
  labs(subtitle=plot_title)

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/14. Benthos/"

# Save the vectorial plot
ggsave(p1, filename=paste0(folder_output_od, "14.1 Hardbunnskorallskog_Jeren_vectorial", shp_area_1$vannregion[1], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")

# Transform the data to raster format

# List of species to rasterize
species_list <- shp_smb_proj_inters %>%
  filter(sarbarthabitat %in% c("Korallskog",
                   "Korallskog/Reirskjell")) %>%
  distinct(sarbarthabitat) %>%
  pull(sarbarthabitat) %>%
  as.character()

# Loop through each species
for (species_to_rasterize in species_list) {
  cat(paste0(species_to_rasterize, "\n")) 
  
  # Filter the shapefile for the current species
  shp_smb_proj_inters_filtered <- shp_smb_proj_inters %>%
    filter(sarbarthabitat %in% c("Korallskog",
                   "Korallskog/Reirskjell"))

  # Rasterize the filtered data
  rasterized_species <- terra::rasterize(
    shp_smb_proj_inters_filtered, rast_100_jeren, field = "sarbarthabitat", fun = "count"
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
    coord_sf(xlim = st_bbox(shp_area_1)[c("xmin", "xmax")], ylim = st_bbox(shp_area_1)[c("ymin", "ymax")], datum = 25833) +
    scale_fill_discrete(name = "14.1 Hardbunnskorallskog art ") +
    geom_point(data = df, aes(x = x, y = y), color = "red")

  # Define the folder path
  folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/"

  # Save the plot
  ggsave(
    plot,
    filename = paste0(
      folder_output_od,
      "Output_final", "14.1 Hardbunnskorallskog_Jeren_raster_100_",
      species_to_rasterize,
      ".png"
    ),
    dpi = 300, height = 20, width = 20, units = "cm", bg = "white"
  )
}
