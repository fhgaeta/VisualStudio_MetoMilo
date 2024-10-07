#visualize-> Fiskeridir_Fjorder med sjeldan_sørvest

#Define folder

user<-Sys.getenv ("USERNAME")

basefolder <- paste0("C:/Users/",user,"/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/GIS Data/")
sub_dir <- paste0("C:/Users/",user,"/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/Focus areas/")

# read study area polygons
library(sf)
library(ggplot2)

#file_areas <- read_sf("C:/Users/",user,"/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/Focus areas/Focus areas_boundary.shp")
file_areas <- read_sf("Focus areas_boundary.shp")

#file.exists('Focus areas_boundary.shp')
#dir.exists(file.path(C:/Users/FEG/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/Focus areas))

ggplot(file_areas)+
  geom_sf(fill= "#69b3a2", color="white")+
  theme_void()

#shp <- sf::read_sf(file_areas)

# read and plot fjords area low oksygen

shp_sv_low <- read_sf("C:/Users/",user,"/NIVA/METOMI~1/AP1KAR~1/DATACO~1/GISDAT~1/ECOLOG~1/113E71~1.PLA/119A01~1.3AN/FISKER~1/Fiskeridir_Fjorder med sjeldan.shp")

ggplot(shp_sv_low)+
  geom_sf(fill= "#69b3a2", color="white")+
  theme_void()

# EPSG:32632
# WGS 84 / UTM zone 32N

crs_proj <- 32632

shp_proj <- shp %>%
  sf::st_transform(crs=crs_proj)

shp_low_proj <- shp_sv_low %>%
  sf::st_transform(crs=crs_proj)

#Define the study area
vannreg_select <- "1108"

shp_area_select <- shp_proj %>%
  filter(vannregion==vannreg_select) %>%
  dplyr::select(identifika)

#Define the extent (Box)
extent <- sf::st_bbox(shp_area_select)

res <- 1000 # resolution m 
x0 <- res*floor(extent$xmin/res) 
y0 <- res*floor(extent$ymin/res) 
x1 <- res*ceiling(extent$xmax/res) 
y1 <- res*ceiling(extent$ymax/res) 


r <- terra::rast(nlyrs=1, xmin=x0, xmax=x1, ymin=y0, ymax=y1, crs=paste0("EPSG:",crs_proj), resolution=res, vals=1)
filledContour (r, add=FALSE)
#test plot (only for raster)
plot (r)

#plot shp

library(ggplot2)
ggplot(shp_sv_low) +
  geom_sf(fill = "#69b3a2", color = "white") +
  theme_void()


plot(shp_area_select, add=TRUE) 

#Transform in raster
?terra::rasterize # see help for this function

shp_sv_low <- terra::rasterize(shp_low_proj, r, fun="count")

r_birds2 <- terra::rasterize(shp_birds_proj, r, field="speciesnorwegianname", fun="mean")
r_birds2 <- terra::rasterize(shp_birds_proj, r, field=1, fun="mean")

plot(r_birds1) 
plot(shp_area_select, add=TRUE) 




#plot(shp_area_select)
plot(r_birds1, add=TRUE) 

terra::writeRaster(r_birds1, filename="test_raster.tif")


quit()
