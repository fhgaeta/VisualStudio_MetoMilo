#plot single area - Hardanger 14.3 Korallrev
#working on it

#16. Naturtyper. DN-Handbok 19
#18 Extremely valuable and sensitive areas 
#19 Sårbare marine biotoper
#20. Coral reefs
#21 Marine grunnkart -Sårbare habitater 
#22. Species/habitats & areas of conservation


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

# Define the path to the GDB file  DN-19 Hard
file_path_gdb <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/16. Naturtyper. DN-Håndbok 19/16.1 Vestlandet DN nat.19/Naturtyper_hb19_EgnDef_4326_FILEGDB.gdb"

#read shp file DN-19 Hard
gdb_dn19_hard <- read_sf(file_path_gdb)

# Define the path to the SHP file- 18 Extremely valuable and sensitive areas 
file_path_gdb_ext_val <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/18. Extremely valuable and sensitive areas/svo_miljoverdier_korallrev/svo_miljoverdier_korallrev.shp"

#read shp file 18 Extremely valuable and sensitive areas
gdb_ext_val <- read_sf(file_path_gdb_ext_val) %>%
  sf::st_set_crs(crs_proj)

# Define the path to 19 Sårbare marine biotoper
file_path_gdb_sar_mar_bio <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/19. Sårbare marine biotoper/19.2 SarbareMarineBunndyrObs_FGDB/SarbareMarineBunndyrObs_FGDB.gdb"
#read shp file 19 Sårbare marine biotoper
gdb_sar_mar_bio <- read_sf(file_path_gdb_sar_mar_bio)

# Define the path to 20. Coral reefs
file_path_gdb_cor_reef <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/20. Korallrev/20.1 Korallrev_FGDB.gdb"
#read shp file 20. Coral reefs
gdb_cor_reef <- read_sf(file_path_gdb_cor_reef)

#define the path to 21 Marine grunnkart -Sårbare habitater
file_path_gdb_sar_hab <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/21. Marine grunnkart - Sårbare habitater/SarbareMarineHabitater_FGDB.gdb"
#read shp file 21 Marine grunnkart -Sårbare habitater
gdb_sar_hab <- read_sf(file_path_gdb_sar_hab)
#define path to 22. Species/habitats & areas of conservation
file_path_gdb_spe_hab <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/22. Species_habitats and areas of conservation/22.6_KorallrevForbudsomrader_FGDB.gdb"
#read shp file 22. Species/habitats & areas of conservation
gdb_spe_hab <- read_sf(file_path_gdb_spe_hab)

  # Load the saved .rds files
  shp_5109_02_03 <- readRDS(paste0(folder_output_od, "shp_5109_1000.rds"))
  shp_water <- readRDS(paste0(folder_output_od, "shp_water.rds"))
  rw_shp <- readRDS(paste0(folder_output_od, "rw_shp.rds"))

# load the 200m study area mask as a shapefile in raster format
rast_100_hard <- terra::rast(paste0(basefolder_od,"Focus areas/grid_mask/Raster/5109_02_03.tif"))

# get the projection from the geonorge layer - we will use this as the basis for our work
crs_proj <- sf::st_crs(shp_water)

#Load study area polygons
Study_areas <- read_sf(paste0(sub_dir, "Focus areas_boundary.shp"))


#The polygons are defined in lat/long coordinates. Now we want to transform them to UTM33 projected coordinates.
Study_areas_proj <- Study_areas %>%
  sf::st_transform(crs=crs_proj)
shp_cor_reef_proj <- gdb_cor_reef %>%
  sf::st_transform(crs=crs_proj)
shp_dn19_proj <- gdb_dn19_hard %>%
  sf::st_transform(crs=crs_proj)
shp_ext_val_proj <- gdb_ext_val %>%
  sf::st_transform(crs = crs_proj)
shp_sar_mar_bio_proj <- gdb_sar_mar_bio %>%
  sf::st_transform(crs=crs_proj)
shp_sar_hab_proj <- gdb_sar_hab %>%
  sf::st_transform(crs=crs_proj)
shp_spe_hab_proj <- gdb_spe_hab %>%
  sf::st_transform(crs=crs_proj)

shp_water_proj <- shp_water %>%
  sf::st_transform(crs=crs_proj)

# Subarea of study area
shp_area_2 <- Study_areas_proj %>%
  filter(vannregion=="5109")

# intersection of datasett and area polygons will give us points within the study areas
shp_cor_reef_inters <- sf::st_intersection(
  shp_cor_reef_proj, 
  shp_area_2)
shp_dn19_inters <- sf::st_intersection(
  shp_dn19_proj, 
  shp_area_2)
shp_ext_val_inters <- sf::st_intersection(
  shp_ext_val_proj, 
  shp_area_2)
shp_sar_mar_bio_inters <- sf::st_intersection(
  shp_sar_mar_bio_proj, 
  shp_area_2)
shp_sar_hab_inters <- sf::st_intersection(
  shp_sar_hab_proj, 
  shp_area_2)
shp_spe_hab_inters <- sf::st_intersection(
  shp_spe_hab_proj, 
  shp_area_2)

# select only "art" in our category: "hardbunnskorallskog".
# make the dataset names a factor. this is a minor detail.
# it will allow us to keep the same colours for different species
# across regions / study areas
#octocoral_species_NO <- unique(shp_octoral_inters$art) %>% sort()
#shp_octoral_inters$art <- factor(shp_octoral_inters$art,
#                                         levels=octocoral_species_NO)

#plot the results
p1 <-ggplot() +
  geom_sf(data = shp_5109_02_03, fill = "#69d9f5", colour = NA, alpha = 0.2) +
  geom_sf(data = shp_cor_reef_inters, aes(colour = naturtypenavn )) +
  geom_sf(data = shp_area_2, fill = NA, colour = "grey", alpha = 0.2, linewidth = 0.4) +
 geom_sf(data= shp_dn19_inters, aes(colour = naturtype)) +
 # geom_sf(data = shp_ext_val_inters, aes(colour = naturtype)) +
  geom_sf(data = shp_sar_mar_bio_inters, aes(colour = indikatorvme)) +
 # geom_sf(data = shp_sar_hab_inters, aes(colour = habitattype)) +
  geom_sf(data = shp_spe_hab_inters, aes(colour = objtype)) +
  coord_sf() +
  scale_colour_discrete(drop = F, name = "art") +
  guides(colour = guide_legend(ncol = 1, byrow = TRUE, override.aes = list(size = 4))) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.text = element_text(size = 8)) +
  labs(subtitle = "14.3 Korallrev - Hardanger")


# get the region
plot_title <- paste0("Vannregion: ", shp_area_2$vannregion[1])

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/14. Benthos/"

# Save the vectorial plot for the current study area
ggsave(p1, filename = paste0(folder_output_od, "14.3 Korallrev_Hardanger_vectorial_", shp_area_2$vannregion[1], ".png"),
             dpi = 300, height = 20, width = 20, units = "cm", bg = "white")

# Transform the data to raster format

# List of species to rasterize
species_list <- shp_dn19_inters %>%
  filter(naturtype %in% c("korallforekomster")) %>%
  distinct(naturtype) %>%
  pull(naturtype) %>%
  as.character()

# Loop through each species
for (species_to_rasterize in species_list) {
 cat(paste0(species_to_rasterize, "\n")) 
  
  # Filter the shapefile for the current species "testspec"
 shp_dn19_inters_selection <- shp_dn19_inters %>%
    filter(naturtype %in% c("korallforekomster"))

  # Rasterize the filtered data
  rasterized_species <- terra::rasterize(
    shp_dn19_inters_selection, rast_100_hard, field = "naturtype", fun = "count"
  )
# Convert the rasterized data to polygons for visualization
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
    ggtitle(paste("14.3 Korallrev_Hardanger_naturtype", species_to_rasterize)) +
    theme_minimal() +
    geom_sf(data = shp_water, colour = NA, fill = "lightblue", alpha = 0.5) +
    coord_sf(xlim = st_bbox(shp_area_2)[c("xmin", "xmax")], ylim = st_bbox(shp_area_2)[c("ymin", "ymax")], datum = 25833) +
    scale_fill_discrete(name = "14.3 Korallrev_Hardanger_naturtype_Steinkoraller") +
    geom_point(data = df, aes(x = x, y = y), color = "red")

  # Define the folder path
  folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output_final/"

  # Save the raster
  ggsave(
    plot,
    filename = paste0(
      folder_output_od, 
      "14.3 Korallrev_Hardanger_naturtype_Steinkoraller_raster_100_", 
      species_to_rasterize, 
      ".png"
    ),
    dpi = 300, height = 20, width = 20, units = "cm", bg = "white"
  )
}

# 18. Extremely valuable and sensitive areas - NO AREAS
# 20 korallrev
# List of species to rasterize
species_list <- shp_cor_reef_inters %>%
  filter(naturtypenavn %in% c("korallforekomster")) %>%
  distinct(naturtypenavn) %>%
  pull(naturtypenavn) %>%
  as.character()

# Loop through each species
for (species_to_rasterize in species_list) {
 cat(paste0(species_to_rasterize, "\n")) 
  
  # Filter the shapefile for the current species "testspec"
 shp_cor_reef_inters_selection <- shp_cor_reef_inters %>%
    filter(naturtypenavn %in% c("korallforekomster"))

  # Rasterize the filtered data
  rasterized_species <- terra::rasterize(
    shp_cor_reef_inters_selection, rast_100_hard, field = "naturtypenavn", fun = "count"
  )
# Convert the rasterized data to polygons for visualization
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
    ggtitle(paste("14.3 Korallrev_Hardanger_naturtypenavn", species_to_rasterize)) +
    theme_minimal() +
    geom_sf(data = shp_water, colour = NA, fill = "lightblue", alpha = 0.5) +
    coord_sf(xlim = st_bbox(shp_area_2)[c("xmin", "xmax")], ylim = st_bbox(shp_area_2)[c("ymin", "ymax")], datum = 25833) +
    scale_fill_discrete(name = "14.3 Korallrev_Hardanger_naturtypenavn") +
    geom_point(data = df, aes(x = x, y = y), color = "red")

  # Define the folder path
  folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output_final/"

  # Save the raster
  ggsave(
    plot,
    filename = paste0(
      folder_output_od, 
      "14.3 Korallrev_Hardanger_naturtype_naturtypenavn_raster_100_", 
      species_to_rasterize, 
      ".png"
    ),
    dpi = 300, height = 20, width = 20, units = "cm", bg = "white"
  )
}

# 21 Marine grunnkart -Sårbare habitater - NO AREAS
# 22. Species/habitats & areas of conservation - Overlapping areas with 20. Coral reefs
