#visualize-> Fiskeridir_Fjorder med sjeldan_sørvest_Jeren

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
  rw_shp<- readRDS(paste0(folder_output_od, "rw_shp.rds"))

# read and plot fjords area low oksygen

shp_salt_low <- read_sf(paste0("C:/Users/", user, "/NIVA/METOMI~1/AP1KAR~1/DATACO~1/",
                             "GISDAT~1/ECOLOG~1/113E71~1.PLA/119A01~1.3AN/FISKER~2/",
                             "Fiskeridir_Fjorder Salten.shp"))

ggplot(shp_salt_low) +
  geom_sf(fill = "#69b3a2", color = "white") +
  theme_void()

#Load study area polygons
Study_areas <- read_sf(paste0(sub_dir, "Focus areas_boundary.shp"))

# get the projection from the geonorge layer - we will use this as the basis for our work
crs_proj <- sf::st_crs(shp_water)

#The polygons are defined in lat/long coordinates. Now we want to transform them to UTM33 projected coordinates.
Study_areas_proj <- Study_areas %>%
  sf::st_transform(crs=crs_proj)
shp_salt_low_proj <- shp_salt_low %>%
  sf::st_transform(crs=crs_proj)
shp_water_proj <- shp_water %>%
  sf::st_transform(crs=crs_proj)


# Select only the attribute titled "vannregion" and the row "1108"
selected_region <- Study_areas_proj %>%
  filter(vannregion == "1108")

  # Intersect the low oxygen fjords with the selected study area
  shp_salt_low_intersect <- sf::st_intersection(shp_salt_low_proj, selected_region)

  # Plot the intersected shapefile
  ggplot(shp_salt_low_intersect) +
    geom_sf(fill = "#69b3a2", color = "white") +
    geom_sf (data= selected_region, fill=NA, color="red") +
    theme_void() +
    labs(title = "Shapefile within the Study Area")

# get the region
plot_title <- paste0("Vannregion: ", selected_region$vannregion[1])

#plot the results
p <- ggplot() +
  geom_sf(data=shp_1108, fill="lightblue", colour=NA, alpha=0.8) +
  geom_sf(data=selected_region, fill=NA, colour="grey", alpha=0.2, linewidth=0.4) +
  geom_sf(data=shp_salt_low_intersect) +
  coord_sf() +
  scale_colour_discrete(drop=FALSE, name="art") +
  guides(colour = guide_legend(ncol = 1, bycol = TRUE)) +
  theme_minimal() +
  labs(subtitle= plot_title)

p

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/11. Plankton/"

# Save the vectorial plot
ggsave(p, filename=paste0(folder_output_od, "Low_oksy_Salten_vectorial", selected_region$vannregion[1], ".png"),
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
r_salt_low <- terra::rasterize(shp_salt_low_intersect, r, field=1, fun="mean")

# Convert the rasterized data to polygons for visualization
#rasterized_salt_low_shp <- as.data.frame(r_salt_low, xy = TRUE)
rasterized_salt_low_shp <- terra::as.polygons(r_salt_low, aggregate = TRUE)

# Ensure the object is an sf object
rasterized_salt_low_shp <- st_as_sf(rasterized_salt_low_shp)

# Check if the geometry column is present
print(names(rasterized_salt_low_shp))

ggplot(rasterized_salt_low_shp) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Rasterized Low Oxygen Fjords", fill = "Value")

# Ensure the geometry column is correctly recognized
if (!"geometry" %in% names(rasterized_salt_low_shp)) {
  rasterized_salt_low_shp <- st_as_sf(rasterized_salt_low_shp)
}

# Create the plot
p2 <- ggplot() +
  geom_sf(data = rasterized_salt_low_shp, aes(geometry = geometry), colour = NA, alpha = 0.8, fill = "yellow") +  # Add the rasterized shapefile
  geom_sf(data = shp_water, colour = NA, fill = "lightblue", alpha = 0.5) + 
  geom_sf(data = rw_shp, colour = "grey", fill = NA, alpha = 0.1) +
  geom_sf(data = shp_1108, colour = "red", fill = NA) + 
  theme_minimal() + 
  coord_sf(xlim = c(x0, x1), ylim = c(y0, y1), datum = 25833) +
  scale_fill_discrete(name = "Fjords with low oxygen") +
  labs(title = "Merged Plot of Rasterized Low Oxygen Fjords")

# Display the plot
print(p2)



# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/11. Plankton/"

# Save the raster plot
ggsave(p2, filename=paste0(folder_output_od, "Low_oksygen_Salten_raster_1000", selected_region$vannregion[1], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")

