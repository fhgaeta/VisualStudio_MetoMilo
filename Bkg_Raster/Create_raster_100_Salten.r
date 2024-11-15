
library(sf)
library(dplyr)
library(ggplot2)
library(terra)

Sys.setlocale("LC_ALL", "en_US.UTF-8")


# Specify the path to the file for study area polygons (this file includes all study areas)
basefolder_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/"
basefolder_teams <- "C:/Users/FEG/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/"

file_areas <- paste0(basefolder_od, "Focus areas/Focus areas_boundary.shp")

# Read in the study area polygons
shp <- sf::read_sf(file_areas, quiet = TRUE)

ggplot() +
  geom_sf(data = shp) + 
  theme_minimal() 

# Background map
# Load background map polygons for sea (from geonorge)
# Specify the geodatabase file
file_db <- paste0(basefolder_od, "GIS Data/Background map/Basisdata_0000_Norge_25833_N1000Kartdata_FGDB.gdb")

# Read the Arealdekke layer and filter to select only "Havflate"
shp_water <- sf::st_read(dsn = file_db, layer = "N1000_Arealdekke_omrade", quiet = TRUE) %>%
  filter(objtype == "Havflate")

# Plot the water layer
ggplot() +
  geom_sf(data = shp_water) + 
  theme_minimal()

# get the projection from the geonorge layer - we will use this as the basis for our work
crs_proj <- sf::st_crs(shp_water)

#The polygons are defined in lat/long coordinates. Now we want to transform them to UTM33 projected coordinates.
shp_proj <- shp %>%
  sf::st_transform(crs=crs_proj)

#Split the polygons for the different ares
shp_1108  <-  shp_proj %>%
  filter(vannområd=="1108-08")

shp_5104  <-  shp_proj %>%
  filter(vannområd=="5104-01")

shp_5109_02  <-  shp_proj %>%
  filter(vannområd=="5109-02")

shp_5109_03  <-  shp_proj %>%
  filter(vannområd=="5109-03")

#Plot the individual study areas
plot_area <- function(shp, shp_bckgd){
  
  # we are using a background shape file that covers
  # the whole of NO so by default the plot will
  # cover the whole country
  # we need to limit the extent of the plot
  # to the area we are interested in
  xlims <- sf::st_bbox(shp)[c(1,3)]
  ylims <- sf::st_bbox(shp)[c(2,4)]
  
  plot_title <- paste0(shp$navn[1]," [", shp$vannområd[1], "]")
  
  p <- ggplot() +
    geom_sf(data=shp_bckgd, colour=NA, fill="lightblue", alpha=0.5) + 
    geom_sf(data=shp, colour="red", fill=NA) + 
    theme_minimal() + 
    labs(subtitle=plot_title) +
    coord_sf(xlim=xlims, ylim=ylims, datum=25833)
  
  return(p)
}

plot_area(shp_1108, shp_water)
plot_area(shp_5104, shp_water)
plot_area(shp_5109_02, shp_water)
plot_area(shp_5109_03, shp_water)

#Create raster grid
#Create rasters to cover each study area. We can take Salten1108 as an example.

#First decide on the resolution of our raster
res <- 100 # resolution m 

#Find the extent of the area of interest (we actually did this previously - we could have been more effective and reused the results)
extent <- sf::st_bbox(shp_1108)

#The corner points of the extent define the extent of the raster. We need to round the coordinates to the nearest whole values of raster resolution.
x0 <- res*floor(extent$xmin/res) 
y0 <- res*floor(extent$ymin/res) 
x1 <- res*ceiling(extent$xmax/res) 
y1 <- res*ceiling(extent$ymax/res)

#Create a raster using the defined extent and resolution
r <- terra::rast(nlyrs=1, xmin=x0, xmax=x1, ymin=y0, ymax=y1, crs=terra::crs(shp_proj), resolution=res, vals=1)

#We cannot plot raster directly in ggplot but we can convert the raster cells to polygons (squares) to visualize them:
grid_shp <- terra::as.polygons(r, aggregate=F) %>%
  sf::st_as_sf()

p <- ggplot() +
  geom_sf(data=shp_water, colour=NA, fill="lightblue", alpha=0.5) + 
  geom_sf(data=grid_shp, colour="grey", fill=NA, alpha=0.1) + 
  geom_sf(data=shp_1108, colour="red", fill=NA) + 
  theme_minimal() + 
  coord_sf(xlim=c(x0,x1), ylim=c(y0,y1), datum=25833)

#We can find out which cells in our raster are water by masking with the water polygons (Havflate)
rw <- terra::crop(r, shp_water, mask=T, touches=F)

# mask the raster using the vannområde polygon 
# this is slightly different to crop
# it doesn't reduce the extent of the raster but
# cells outside the mask polygon are set to NA
rw <- terra::mask(rw, shp_1108, touches=F)


rw_shp <- terra::as.polygons(rw, aggregate=F) %>%
  sf::st_as_sf()

# rw <- sf::st_intersection(grid_shp, shp_water)

p2 <- ggplot() +
  geom_sf(data=shp_water, colour=NA, fill="lightblue", alpha=0.5) + 
  geom_sf(data=rw_shp, colour="grey", fill="blue", alpha=0.1) + 
  geom_sf(data=shp_1108, colour="red", fill=NA) + 
  theme_minimal() + 
  coord_sf(xlim=c(x0,x1), ylim=c(y0,y1), datum=25833)

#The masked water raster has the same number of cells as the original raster. The masked-out cells are given an NA value.

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output"

# Save the plot to the specified folder
ggsave(p2, file=paste0(folder_output_od, "/plot_salten_raster100.png"), width=10, height=10, units="cm", dpi=300)

 # Save the results as .rds files
  saveRDS(shp_1108, file = paste0(folder_output_od, "shp_1108_100.rds"))
  saveRDS(shp_water, file = paste0(folder_output_od, "shp_water.rds"))
  saveRDS(rw_shp, file = paste0(folder_output_od, "rw_shp.rds"))


#Now we can use the unmasked raster r as a template for rasterising vector data and use the masked raster rw to mask the resulting rasters so that only water cells are “active”.


#Conversion of vector data to raster
#Now that we have defined our raster grid, we can convert vector data to the grid so that it can be used in our analyses.