#12.1 spawnings grounds
library(sf)
library(dplyr)
library(ggplot2)
library(terra)

# now we assume we already made our masked raster 
# after getting the masked raster (e.g.) for 1108 Nord-Salten, we saved it as a tif file:
# 
# rw <- terra::mask(rw_unmasked, shp_1108, touches=F)
# terra::writeRaster(rw, filename="study_area_rasters/grid_raster_1108.tif")

#Define folder

Sys.setlocale("LC_ALL", "en_US.UTF-8")
user <- Sys.getenv("USERNAME")

basefolder <- paste0("C:/Users/",user,"/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/GIS Data/") # nolint
sub_dir <- paste0("C:/Users/",user,"/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/Focus areas/") # nolint


# read study area polygons


file_areas <- read_sf(paste0(sub_dir, "Focus areas_boundary.shp")) # nolint

ggplot(file_areas)+ # nolint
  geom_sf(fill= "#52b8a0", color="white")+ # nolint
  theme_void()

# read and plot spawnings area cod

shp_spw_cod <- st_read(paste0(basefolder, "/Ecological components data/12. Fisk/12.1 Spawning grounds/kysttorsk/kysttorsk.shp")) # nolint

print(shp_spw_cod)
summary(shp_spw_cod)

shp_spw_cod <- st_make_valid(shp_spw_cod)


ggplot(shp_spw_cod) +
  geom_sf(fill= "#69b3a2", color="white") +
  theme_void()
