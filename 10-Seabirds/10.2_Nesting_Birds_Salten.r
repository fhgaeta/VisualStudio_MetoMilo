#plot single area 3 - Nord Salten

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(leaflet)
library(terra)
install.packages("rgdal")
library(rgdal)


#Define folder

Sys.setlocale("LC_ALL", "en_US.UTF-8")
#user <- Sys.getenv("USERNAME")

basefolder_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/"
basefolder_teams <- "C:/Users/FEG/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/"


# Read in the study area polygons
file_areas <- paste0(basefolder_od, "Focus areas/Focus areas_boundary.shp")
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


#Create raster grid
#Create rasters to cover each study area. We can take Salten1108 as an example.

#First decide on the resolution of our raster
res <- 1000 # resolution m 

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

ggplot() +
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

# read bird data 

file <- paste0(basefolder_teams,"GIS Data/Ecological components data/10. Seabirds/10.2_FGDB.gdb")

db_layers <- sf::st_layers(file) 
db_layers <- db_layers$name

shp_birds_all <- purrr::map(db_layers, sf::st_read, dsn=file, quiet=T) %>%
  bind_rows()

shp_birds <- shp_birds_all %>%
  sf::st_transform(crs=crs_proj)

# intersection of birds and area polygons will 
# givr us points within the study areas

shp_birds <- shp_birds %>%
  sf::st_intersection(shp)

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

shp_area_3 <- shp %>%
  filter(vannregion=="1108")

shp_birds_1 <- shp_birds %>%
  filter(vannregion=="1108")

# select only specific columns for the bird data
shp_birds_1 <- shp_birds_1 %>%
  select(speciesenglishname, speciesnorwegianname)

# get the region
plot_title <- paste0("Vannregion: ", shp_area_3$vannregion[1])

#plot the results
p <- ggplot() +
  geom_sf(data=shp_1108, fill="lightblue", colour=NA, alpha=0.8) +
  geom_sf(data=shp_area_3, fill=NA, colour="grey", alpha=0.2, linewidth=0.4) +
  geom_sf(data=shp_birds_1, aes(colour=speciesnorwegianname)) +
  coord_sf() +
  scale_colour_discrete(drop=F, name="art") +
  guides(colour = guide_legend(ncol = 1, bycol = T)) +
  theme_minimal() +
  labs(subtitle=plot_title)

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/10.Seabirds/"

# Save the vectorial plot
ggsave(p, filename=paste0(folder_output_od, "Seabirds_Salten_vectorial", shp_area_3$vannregion[1], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")

#Transform the bird data to raster format
#We can use the rasterize function from the raster package to transform the bird data to raster format.
#The function will count the number of bird observations in each raster cell.
# Rasterize the vector data using the unmasked raster as a template
rasterized_birds <- terra::rasterize(shp_birds_1, r, field="speciesnorwegianname", fun="count")

# Convert the rasterized data to polygons for visualization
rasterized_birds_shp <- terra::as.polygons(rasterized_birds, aggregate=F) %>%
  sf::st_as_sf()

# Ensure the speciesnorwegianname field is treated as a factor
rasterized_birds_shp$speciesnorwegianname <- as.factor(rasterized_birds_shp$speciesnorwegianname)

# Plot the rasterized bird data
p3 <- ggplot() +
  geom_sf(data=shp_water, colour=NA, fill="lightblue", alpha=0.5) + 
  geom_sf(data=rasterized_birds_shp, aes(fill=speciesnorwegianname), colour=NA, alpha=0.8) + 
  geom_sf(data=shp_1108, colour="red", fill=NA) + 
  theme_minimal() + 
  coord_sf(xlim=c(x0,x1), ylim=c(y0,y1), datum=25833) +
  scale_fill_discrete(name="Bird Species")

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/10.Seabirds/"

# Save the vectorial plot
ggsave(p3, filename=paste0(folder_output_od, "Seabirds_Salten_raster_1000", shp_area_3$vannregion[1], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")



