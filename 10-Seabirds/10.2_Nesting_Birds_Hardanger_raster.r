#plot single area 3 - Hardanger

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
  shp_5109_02_03 <- readRDS(paste0(folder_output_od, "shp_5109_1000.rds"))
  shp_water <- readRDS(paste0(folder_output_od, "shp_water.rds"))
  rw_shp <- readRDS(paste0(folder_output_od, "rw_shp.rds"))

# load the 100m study area mask as a shapefile in raster format
rast_100_hard <- terra::rast(paste0(basefolder_od,"Focus areas/grid_mask/Raster/5109_02_03.tif"))

# read bird data 

file <- paste0(sub_teams,"GIS Data/Ecological components data/10. Seabirds/10.2_FGDB.gdb")

db_layers <- sf::st_layers(file) 
db_layers <- db_layers$name
shp_birds_all <- purrr::map(db_layers, sf::st_read, dsn=file, quiet=T) %>%
  bind_rows()

# get the projection from the geonorge layer - we will use this as the basis for our work
crs_proj <- sf::st_crs(shp_water)

shp_birds <- shp_birds_all %>%
  sf::st_transform(crs=crs_proj)

  #Load study area polygons
Study_areas <- read_sf(paste0(sub_dir, "Focus areas_boundary.shp"))

# get the projection from the geonorge layer - we will use this as the basis for our work
crs_proj <- sf::st_crs(shp_water)

#The polygons are defined in lat/long coordinates. Now we want to transform them to UTM33 projected coordinates.
Study_areas_proj <- Study_areas %>%
  sf::st_transform(crs=crs_proj)
shp_birds_proj <- shp_birds %>%
  sf::st_transform(crs=crs_proj)
shp_water_proj <- shp_water %>%
  sf::st_transform(crs=crs_proj)

# intersection of birds and area polygons will 
# givr us points within the study areas

shp_birds <- shp_birds %>%
  sf::st_intersection(Study_areas_proj)

# make the bird names a factor. this is a minor detail.
# it will allow us to keep the same colours for different species
# across regions / study areas
bird_species_EN <- unique(shp_birds$speciesenglishname) %>% sort()
bird_species_NO <- unique(shp_birds$speciesnorwegianname) %>% sort()
shp_birds$speciesenglishname <- factor(shp_birds$speciesenglishname,
                                       levels=bird_species_EN)
shp_birds$speciesnorwegianname <- factor(shp_birds$speciesnorwegianname,
                                         levels=bird_species_NO)

# select subset of data for a single study area

shp_area_2 <- Study_areas_proj %>%
  filter(vannregion=="5109")

shp_birds_1 <- shp_birds_proj %>%
  filter(vannregion=="5109")

# select only specific columns for the bird data
shp_birds_1 <- shp_birds_1 %>%
  select(speciesenglishname, speciesnorwegianname)

# get the region
plot_title <- paste0("Vannregion: ", shp_area_2$vannregion[1])

#plot the results
p1 <- ggplot() +
  geom_sf(data=shp_5109_02_03, fill="lightblue", colour=NA, alpha=0.8) +
  geom_sf(data=shp_area_2, fill=NA, colour="grey", alpha=0.2, linewidth=0.4) +
  geom_sf(data=shp_birds_1, aes(colour=speciesnorwegianname)) +
  coord_sf() +
  scale_colour_discrete(drop=FALSE, name="art") +
  guides(colour = guide_legend(ncol = 1, bycol = TRUE)) +
  theme_minimal() +
  labs(subtitle= plot_title)

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/10.Seabirds/"

# Save the vectorial plot
ggsave(p1, filename=paste0(folder_output_od, "Seabirds_Hardang_vectorial", shp_area_2$vannregion[1], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")

#Transform the bird data to raster format

# List of species to rasterize
species_list <- shp_birds_1 %>%
  distinct(speciesnorwegianname) %>%
  pull(speciesnorwegianname) %>%
  as.character()

# Loop through each species
for (species_to_rasterize in species_list) {
  cat(paste0(species_to_rasterize, "\n")) 
  
  # Filter the shapefile for the current species
  shp_birds_1_filtered <- shp_birds_1 %>%
    filter(speciesnorwegianname == species_to_rasterize)

#res <- sf::st_intersects(shp_birds_1_filtered, shp_100_jeren)

?sf::st_intersects
  # Rasterize the filtered data
  rasterized_species <- terra::rasterize(
    shp_birds_1_filtered, rast_100_hard, field = "speciesnorwegianname", fun = "count"
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
    scale_fill_discrete(name = "Bird Specie") +
    geom_point(data = df, aes(x = x, y = y), color = "red")

  # Define the folder path
  folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output_final/"

  # Save the plot
  ggsave(
    plot,
    filename = paste0(
      folder_output_od, 
      "10.2.Seabirds_Hardanger_raster_100_", 
      species_to_rasterize, 
      ".png"
    ),
    dpi = 300, height = 20, width = 20, units = "cm", bg = "white"
  )
}
