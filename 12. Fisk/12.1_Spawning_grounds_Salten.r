#visualize-> Gytefelt  Salten

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
  rw_shp_salt <- readRDS(paste0(folder_output_od, "rw_shp_Salten.rds"))

  plot(rw_shp_salt)

# read and plot spawnings grounds for torsk

base_path <- paste0("C:/Users/", user, "/NIVA/METOMI~1/AP1KAR~1/DATACO~1/GISDAT~1/ECOLOG~1/12. Fisk/12.1 Spawning grounds/kysttorsk/")
shp_salt_torsk <- read_sf(paste0(base_path, "kysttorsk.shp"))
shp_lyr_salten <- read_sf(paste0(base_path, "../Gytefelt_lyr/Gytefelt_lyr.shp"))
shp_salt_hvitting <- read_sf(paste0(base_path, "../Gytefelt_hvitting/Gytefelt_hvitting.shp"))
shp_salt_hyse <- read_sf(paste0(base_path, "../Gytefelt_hyse/Gytefelt_hyse.shp"))
shp_salt_sei <- read_sf(paste0(base_path, "../Gytefelt_sei/Gytefelt_sei.shp"))
#shp_salt_torsk2 <- read_sf(paste0(base_path, "../Fiskd_Gyteom_torsk_jer/Fiskeridir_Gyteområder torsk.shp"))


# Plot only if necessary
ggplot() +
  geom_sf(data = shp_lyr_salten, fill = "#69b3a2", color = "white") +
  geom_sf(data = shp_salt_hvitting, fill = "green", color = "white") +
  geom_sf(data = shp_salt_hyse, fill = "#ac5564", color = "white") +
  geom_sf(data = shp_salt_sei, fill = "blue", color = "white") +
  #geom_sf(data = shp_salt_torsk, fill = "red", color = "white") +
  labs(title = "Shapefile of Spawning grounds for torsk, hyse, lyr, sei, hvitting") +
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
shp_salten_lyr_proj <- shp_lyr_salten %>% 
  sf::st_transform(crs=crs_proj)
shp_salten_hvitting_proj <- shp_salt_hvitting %>% 
  sf::st_transform(crs=crs_proj)
shp_salten_hyse_proj <- shp_salt_hyse %>% 
  sf::st_transform(crs=crs_proj)
shp_salten_sei_proj <- shp_salt_sei %>% 
  sf::st_transform(crs=crs_proj)


# Select only the attribute titled "vannregion" and the row "1108"
selected_region <- Study_areas_proj %>%
  filter(vannregion == "1108")

  # Intersect the low oxygen fjords with the selected study area
  shp_salten_lyr_intersect <- sf::st_intersection(shp_salten_lyr_proj, selected_region)
  shp_salten_hvitting_intersect <- sf::st_intersection(shp_salten_hvitting_proj, selected_region)
  shp_salten_hyse_intersect <- sf::st_intersection(shp_salten_hyse_proj, selected_region)
  shp_salten_sei_intersect <- sf::st_intersection(shp_salten_sei_proj, selected_region)
  
  # Ensure geometries are valid
shp_salten_lyr_intersect <- st_make_valid(shp_salten_lyr_intersect)
shp_salten_hvitting_intersect <- st_make_valid(shp_salten_hvitting_intersect)
shp_salten_hyse_intersect <- st_make_valid(shp_salten_hyse_intersect)
shp_salten_sei_intersect <- st_make_valid(shp_salten_sei_intersect)

# get the region
plot_title <- paste0("Vannregion: ", selected_region$vannregion[1])

#plot the results
p <- ggplot() +
  geom_sf(data = shp_salten_lyr_intersect, fill = "#69b3a2", color = "white") +
  geom_sf(data = shp_salten_hvitting_intersect, fill = "green", color = "white") +
  geom_sf(data = shp_salten_hyse_intersect, fill = "#ac5564", color = "white") +
  geom_sf(data = shp_salten_sei_intersect, fill = "blue", color = "white") +
  geom_sf(data=selected_region, fill=NA, colour="grey", alpha=0.2, linewidth=0.4) +
  geom_sf(data=shp_1108, fill="lightblue", colour=NA, alpha=0.8) +
  coord_sf() +
  scale_colour_discrete(drop=FALSE, name="art") +
  guides(colour = guide_legend(ncol = 1, bycol = TRUE)) +
  theme_minimal() +
  labs(subtitle= plot_title)

p

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/12. fisk/12.1 Spawning grounds/"

# Save the vectorial plot
ggsave(p, filename=paste0(folder_output_od, "Spawning_salten_vectorial", selected_region$vannregion[1], ".png"),
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
  vals = NA
)

# Rasterize the shapefiles with different categories
r_lyr <- terra::rasterize(shp_jer_lyr_intersect, r, field = 1, fun = "mean")
r_hyse <- terra::rasterize(shp_salten_hyse_intersect, r, field = 2, fun = "mean")
r_sei <- terra::rasterize(shp_salten_sei_intersect, r, field = 3, fun = "mean")
#r_hvitting <- terra::rasterize(shp_jer_hvitting_intersect, r, field = 4, fun = "mean")

# Convert the rasterized data to polygons for visualization
rasterized_r_lyr <- terra::as.polygons(r_lyr, field = 1, aggregate = TRUE)
rasterized_r_hyse <- terra::as.polygons(r_hyse, field = 2, aggregate = TRUE)
rasterized_r_sei <- terra::as.polygons(r_sei, field = 3, aggregate = TRUE)
#rasterized_r_hvitting <- terra::as.polygons(r_hvitting, aggregate = TRUE)
#rasterized_r_jeren_spawn <- terra::as.polygons(r_jeren_spawn, dissolve = FALSE)

#Ensure the object is an sf object
rasterized_r_lyr <- st_as_sf(rasterized_r_lyr)
rasterized_r_hyse <- st_as_sf(rasterized_r_hyse)
rasterized_r_sei <- st_as_sf(rasterized_r_sei)
#rasterized_r_hvitting <- st_as_sf(rasterized_r_hvitting) 

# Check if the geometry column is present
print(names(rasterized_r_lyr))
print(names(rasterized_r_hyse))
print(names(rasterized_r_sei))
#print(names(rasterized_r_hvitting))

  # Create the plot with color differentiation for different fish spawning categories
  p2 <- ggplot() +
    geom_sf(data = rasterized_r_lyr, fill = "#69b3a2", color = "white", alpha = 0.8) +
    geom_sf(data = rasterized_r_hyse, fill = "#ac5564", color = "white", alpha = 0.8) +
    geom_sf(data = rasterized_r_sei, fill = "blue", color = "white", alpha = 0.8) +
    geom_sf(data = shp_water, colour = NA, fill = "lightblue", alpha = 0.5) + 
    geom_sf(data = rw_shp_salt, colour = "grey", fill = NA, alpha = 0.1) +
    geom_sf(data = shp_1108, colour = "red", fill = NA) + 
    coord_sf(xlim = c(x0, x1), ylim = c(y0, y1), crs = crs_proj) +
    labs(title = "Rasterized spawnings areas", fill = "Value") +
    theme_minimal()


# Display the plot
  print(p2)

# Explicitly call garbage collection
gc()

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/12. Fisk/12.1 Spawning grounds/"

# Save the raster plot
ggsave(p2, filename=paste0(folder_output_od, "Spawnings_areas_Salten_1000", selected_region$vannregion[1], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")