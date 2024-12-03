#plot NiN classification (marine grunnkart) - Jæren

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(leaflet)
library(terra)
install.packages("rgdal")
library(rgdal)


Sys.setlocale("LC_ALL", "en_US.UTF-8")


#Define folders

user <- Sys.getenv("USERNAME")

basefolder_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/"
sub_dir <- paste0("C:/Users/", user, 
          "/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/",
          "AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/Focus areas/")
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output"

# Load study area polygons
study_areas <- read_sf(paste0(sub_dir, "Focus areas_boundary.shp"))

  # Load the saved .rds files
  shp_5104 <- readRDS(paste0(folder_output_od, "shp_5104_1000.rds"))
  shp_water <- readRDS(paste0(folder_output_od, "shp_water.rds"))
  rw_shp_jer <- readRDS(paste0(folder_output_od, "rw_shp_jer.rds"))

plot(rw_shp_jer)

# read NiN data 

# Read and transform the FGDB object into a shapefile # nolint
nin_jer_fgdb <- paste0(basefolder_od, "GIS Data/Ecological components data/14. Benthic biotopes/14.1 SaltvannssjobPredikert_FGDB.gdb")

# List all layers in the FGDB
db_layers <- sf::st_layers(nin_jer_fgdb)$name

# Read all layers and combine them into a single shapefile
shp_nin_all <- purrr::map(db_layers, sf::st_read, dsn=nin_jer_fgdb, quiet=TRUE) %>%
bind_rows()

#load study area polygons
study_areas <- read_sf(paste0(sub_dir, "Focus areas_boundary.shp"))

# Define the CRS
crs_proj <- 25833

# Transform the CRS of the shapefile
shp_nin <- shp_nin_all %>%
  sf::st_transform(crs = crs_proj)
study_areas_proj <- study_areas %>%
  sf::st_transform(crs = crs_proj)
shp_water_proj <- shp_water %>%
    sf::st_transform(crs = crs_proj)


# Save the shapefile
sf::st_write(shp_nin, paste0(basefolder_od, "GIS Data/Ecological components data/14. Benthic biotopes/nin_jer_shapefile.shp"))

# intersection of Nin and area polygons will 
# givr us points within the study areas
# Select only the attribute titled "vannregion" and the row "1108"

shp_nin_inters <- shp_nin %>%
  sf::st_intersection(shp_5104)

# get the region
plot_title <- paste0("Vannregion: 5104")

#plot the results
p <- ggplot() +
    geom_sf(data=shp_5104, fill="lightblue", colour=NA, alpha=0.8) +
    geom_sf(data=rw_shp_jer, fill=NA, colour="grey", alpha=0.2, linewidth=0.4) +
    geom_sf(data=shp_nin_inters, aes(colour=ninbeskrivelse)) + 
    coord_sf() +
    scale_colour_discrete(drop = FALSE, name = "Biotop") + 
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(ncol = 1, bycol = TRUE)) +
    labs(subtitle=plot_title)
  
p

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/14. Benthic biotopes/"

# Save the vectorial plot
ggsave(p, filename=paste0(folder_output_od, "Nin_classification_Jeren_vectorial", shp_5104$vannregion[1], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")

#Transform the Nin data to raster format

# Define the extent (Box)

# Select only the attribute titled "vannregion" and the row "5104"
selected_region <- study_areas_proj %>%
  filter(vannregion == "5104")

extent <- sf::st_bbox(selected_region)

res <- 1000 # resolution m 
x0 <- res*floor(extent$xmin/res) 
y0 <- res*floor(extent$ymin/res) 
x1 <- res*ceiling(extent$xmax/res) 
y1 <- res*ceiling(extent$ymax/res) 

r <- terra::rast(
  xmin = x0, 
  xmax = x1, 
  ymin = y0, 
  ymax = y1, 
  crs = paste0("EPSG:", crs_proj), 
  resolution = res, 
  vals = 1
)

# Test plot (only for raster)
r

#We can use the rasterize function from the raster package to transform the Nin data to raster format.
#The function will count the number of bird observations in each raster cell.

# Rasterize the vector data using the terra package
rasterized_nin <- terra::rasterize(
  shp_nin_inters, r, field = "ninbeskrivelse", fun = "mean"
)

# Convert the rasterized data to polygons for plotting
rasterized_nin_shp <- terra::as.polygons(rasterized_nin, aggregate = FALSE) %>%
    sf::st_as_sf()

# Plot the rasterized Nin data
p2 <- ggplot() +
    geom_sf(data = rasterized_nin_shp, aes(fill = as.factor(ninbeskrivelse)), color = NA) +
    geom_sf(data = rw_shp_jer, fill = NA, color = "grey", alpha = 0.2, linewidth = 0.4) +
    geom_sf(data = shp_5104, fill = NA, color = "red") +
    geom_sf(data = shp_water_proj, fill = "lightblue", color = NA, alpha = 0.5) +
    coord_sf(xlim = c(x0, x1), ylim = c(y0, y1), crs = crs_proj) +
    scale_fill_discrete(name = "Biotop") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 13, bycol = TRUE)) +
    labs(title = "Rasterized NiN Classification Areas - Jæren")

p2

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/14. Benthic biotopes/"

# Save the vectorial plot
ggsave(p2, filename=paste0(folder_output_od, "NiN_Classification_Jeren_raster_1000", shp_5104$vannregion[3], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")



