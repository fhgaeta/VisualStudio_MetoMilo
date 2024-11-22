#visualize-> Gytefelt lyr, hvitting, hyse, sei i Hardanger

# read study area polygons
library(sf)
library(dplyr)
library(ggplot2)
library(terra)

#Define folder

user<-Sys.getenv ("USERNAME")

basefolder <- paste0("C:/Users/",user,"/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/GIS Data/")
sub_dir <- paste0("C:/Users/",user,"/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/Focus areas/")
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output"

  # Load the saved .rds files
  shp_1108 <- readRDS(paste0(folder_output_od, "shp_1108_1000.rds"))
  shp_water <- readRDS(paste0(folder_output_od, "shp_water.rds"))
  rw_shp_sal<- readRDS(paste0(folder_output_od, "rw_shp_Salten.rds"))

  plot(rw_shp_sal)

# read and plot spawnings grounds for torsk
# Load and read the shapefile as sf
shp_beite_salt <- read_sf("C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/12. Fisk/12.2 Nursery & feeding areas/12.2 Fiskeridir_Oppvekst beiteområd/Fiskeridir_Oppvekst beiteområd.shp")

# Plot the shapefile
ggplot() +
  geom_sf(data = shp_beite_salt, fill = "#69b3a2", color = "white") +
  labs(title = "Shapefile of feeding areas") +
  theme_void()  

#Load study area polygons
Study_areas <- read_sf(paste0(sub_dir, "Focus areas_boundary.shp"))

# get the projection from the geonorge layer - we will use this as the basis for our work
crs_proj <- sf::st_crs(shp_water)

#The polygons are defined in lat/long coordinates. Now we want to transform them to UTM33 projected coordinates.
Study_areas_proj <- Study_areas %>%
  sf::st_transform(crs=crs_proj)
shp_water_proj <- shp_water %>%
  sf::st_transform(crs=crs_proj)
shp_salt_beite_proj <- shp_beite_salt %>%
  sf::st_transform(crs=crs_proj)

# Select only the attribute titled "vannregion" and the row "1108"
selected_region <- Study_areas_proj %>%
  filter(vannregion == "1108")

  # Intersect the low oxygen fjords with the selected study area
  shp_salt_beite_intersect <- sf::st_intersection(shp_salt_beite_proj, selected_region)

  # Plot the intersected shapefile
  ggplot() +
    geom_sf(data = shp_salt_beite_intersect, fill = "#69b3a2", color = "white") +
    labs(title = "Shapefile within the Study Area") +
    theme_void()

# get the region
plot_title <- paste0("Vannregion: ", selected_region$vannregion[1])

#plot the results
p <- ggplot() +
  geom_sf(data=selected_region, fill=NA, colour="grey", alpha=0.2, linewidth=0.4) +
  geom_sf(data=shp_1108, fill="lightblue", colour=NA, alpha=0.8) +
  geom_sf(data = shp_salt_beite_intersect, fill = "#69b3a2", color = "white") +
theme_minimal() 

print(p)

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/12. fisk/12.2 feeding areas/"

# Save the vectorial plot
ggsave(p, filename=paste0(folder_output_od, "Feeding area_salt_vectorial", selected_region$vannregion[2], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")

#Define the extent (Box)
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
  crs = paste0("EPSG:", crs_proj$epsg), 
  resolution = res, 
  vals = 1
)

filled.contour(r, add = FALSE)

#test plot (only for raster)
plot (r)


# Rasterize the intersected shapefile
r_salt_beite <- terra::rasterize(shp_salt_beite_intersect, r, field=1, fun="mean")  

# Convert the rasterized data to polygons for visualization
#rasterized_salt_low_shp <- as.data.frame(r_salt_low, xy = TRUE)
rasterized_salt_beite_shp <- terra::as.polygons(r_salt_beite, aggregate = TRUE)

# Ensure the object is an sf object
rasterized_salt_beite_shp <- st_as_sf(rasterized_salt_beite_shp)


# Check if the geometry column is present
print(names(rasterized_salt_beite_shp))


  # Create the plot with color differentiation for different fish spawning categories
  p2 <- ggplot() +
    geom_sf(data = rasterized_salt_beite_shp, fill = "#1cd85b", color = "white", alpha = 0.8) +
    geom_sf(data = shp_water, colour = NA, fill = "lightblue", alpha = 0.5) + 
    geom_sf(data = rw_shp_sal, colour = "grey", fill = NA, alpha = 0.1) +
    geom_sf(data = shp_1108, colour = "red", fill = NA) + 
    coord_sf(xlim = c(x0, x1), ylim = c(y0, y1), crs = crs_proj) +
    labs(title = "Rasterized feeding and nursery areas salten", fill = "Value") +
    theme_minimal()

# Display the plot
print(p2)


# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/12. Fisk/12.2 feeding areas/"

# Save the raster plot
ggsave(p2, filename=paste0(folder_output_od, "Feeding and nursery area_salten_1000_raster", selected_region$vannregion[2], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")
