#visualize-> Gytefelt torsk. hyse, lyr, sei, hvitting i Jeren

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
  shp_5104 <- readRDS(paste0(folder_output_od, "shp_5104_1000.rds"))
  shp_water <- readRDS(paste0(folder_output_od, "shp_water.rds"))
  rw_shp_jer<- readRDS(paste0(folder_output_od, "rw_shp.rds"))

  plot(rw_shp_jer)

# read and plot spawnings grounds for torsk

base_path <- paste0("C:/Users/", user, "/NIVA/METOMI~1/AP1KAR~1/DATACO~1/GISDAT~1/ECOLOG~1/12. Fisk/12.1 Spawning grounds/kysttorsk/")
shp_jeren_lyr <- read_sf(paste0(base_path, "../Gytefelt_lyr/Gytefelt_lyr.shp"))
shp_jeren_hvitting <- read_sf(paste0(base_path, "../Gytefelt_hvitting/Gytefelt_hvitting.shp"))
shp_jeren_hyse <- read_sf(paste0(base_path, "../Gytefelt_hyse/Gytefelt_hyse.shp"))
shp_jeren_sei <- read_sf(paste0(base_path, "../Gytefelt_sei/Gytefelt_sei.shp"))
#shp_jeren_kysttorsk <- read_sf(paste0(base_path, "../Fiskeridir_Gyteomraader_torsk/Fiskeridir_Gyteområder torsk.shp")) PROBLEM WITH THE ORIGINAL SOURCE FILE
#shp_jeren_torsk_2 <- read_sf(paste0(base_path, "../Fiskd_gytefelt_torsk/Fiskeridir_Gytefelt torsk MB.shp")) PROBLEM WITH THE ORIGINAL SOURCE FILE


# Plot the shapefile
ggplot() +
  geom_sf(data = shp_jeren_lyr, fill = "#69b3a2", color = "white") +
  geom_sf(data = shp_jeren_hvitting, fill = "green", color = "white") +
  geom_sf(data = shp_jeren_hyse, fill = "#ac5564", color = "white") +
  geom_sf(data = shp_jeren_sei, fill = "blue", color = "white") +
  geom_sf(data = shp_jeren_kysttorsk, fill = "yellow", color = "white") +
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
shp_jeren_lyr_proj <- shp_jeren_lyr %>%
  sf::st_transform(crs=crs_proj)
shp_jeren_hvitting_proj <- shp_jeren_hvitting %>%
  sf::st_transform(crs=crs_proj)
shp_jeren_hyse_proj <- shp_jeren_hyse %>%
  sf::st_transform(crs=crs_proj)
shp_jeren_sei_proj <- shp_jeren_sei %>%
  sf::st_transform(crs=crs_proj)

# Select only the attribute titled "vannregion" and the row "5104"
selected_region <- Study_areas_proj %>%
  filter(vannregion == "5104")

  # Intersect the study object with the selected study area
  shp_jer_lyr_intersect <- sf::st_intersection(shp_jeren_lyr_proj, selected_region)
  shp_jer_hvitting_intersect <- sf::st_intersection(shp_jeren_hvitting_proj, selected_region)
  shp_jer_hyse_intersect <- sf::st_intersection(shp_jeren_hyse_proj, selected_region)
  shp_jer_sei_intersect <- sf::st_intersection(shp_jeren_sei_proj, selected_region)


  # Plot the intersected shapefile
  ggplot() +
  geom_sf(data = shp_jer_lyr_intersect) +
  geom_sf(data = shp_jer_hvitting_intersect) +
  geom_sf(data = shp_jer_lyr_intersect, fill = "#69b3a2", color = "white") +
  geom_sf(data = shp_jer_hvitting_intersect, fill = "green", color = "white") +
  geom_sf(data = shp_jer_hyse_intersect, fill = "#ac5564", color = "white") +
  geom_sf(data = shp_jer_sei_intersect, fill = "blue", color = "white") +
  labs(title = "Shapefile within the Study Area")
  theme_void()
    

# get the region
plot_title <- paste0("Vannregion: ", selected_region$vannregion[3])

#plot the results
p <- ggplot() +
  geom_sf(data=selected_region, fill=NA, colour="grey", alpha=0.2, linewidth=0.4) +
  geom_sf(data=shp_5104, fill="lightblue", colour=NA, alpha=0.8) +
  geom_sf(data = shp_jer_lyr_intersect, fill = "#69b3a2", color = "white") +
  geom_sf(data = shp_jer_hvitting_intersect, fill = "green", color = "white") +
  geom_sf(data = shp_jer_hyse_intersect, fill = "#ac5564", color = "white") +
  geom_sf(data = shp_jer_sei_intersect, fill = "blue", color = "white") +
  coord_sf() +
  scale_colour_discrete(drop=FALSE, name="art") +
  guides(colour = guide_legend(ncol = 1, bycol = TRUE)) +
  theme_minimal() +
  labs(subtitle= plot_title)

p

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/12. fisk/12.1 Spawning grounds/"

# Save the vectorial plot
ggsave(p, filename=paste0(folder_output_od, "Spawning_jeren_vectorial", selected_region$vannregion[3], ".png"),
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
r_jeren_low <- terra::rasterize(shp_jer_lyr_intersect, r, field=1, fun="mean")

# Convert the rasterized data to polygons for visualization
#rasterized_salt_low_shp <- as.data.frame(r_salt_low, xy = TRUE)
rasterized_jer_lyr_shp <- terra::as.polygons(r_jeren_low, aggregate = TRUE)

# Ensure the object is an sf object
rasterized_jer_lyr_shp <- st_as_sf(rasterized_jer_lyr_shp)

# Check if the geometry column is present
print(names(rasterized_jer_lyr_shp))

ggplot(rasterized_jer_lyr_shp) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Rasterized Low Oxygen Fjords", fill = "Value")

# Ensure the geometry column is correctly recognized
if (!"geometry" %in% names(rasterized_jer_lyr_shp)) {
  rasterized_jer_lyr_shp <- st_as_sf(rasterized_jer_lyr_shp)
}

# Create the plot
p2 <- ggplot() +
  geom_sf(data = rasterized_jer_lyr_shp, aes(geometry = geometry), colour = NA, alpha = 0.8, fill = "yellow") +  # Add the rasterized shapefile
  geom_sf(data = shp_water, colour = NA, fill = "lightblue", alpha = 0.5) + 
  geom_sf(data = rw_shp_jer, colour = "grey", fill = NA, alpha = 0.1) +
  geom_sf(data = shp_1108, colour = "red", fill = NA) + 
  theme_minimal() + 
  coord_sf(xlim = c(x0, x1), ylim = c(y0, y1), datum = 25833) +
  scale_fill_discrete(name = "Fjords with spawnings area lyr") +
  labs(title = "Merged Plot of Rasterized spawnings area lyr")

# Display the plot
print(p2)



# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/12. Fisk/12.1 Spawning grounds/"

# Save the raster plot
ggsave(p2, filename=paste0(folder_output_od, "Spawnings_area_lyr_jeren_1000", selected_region$vannregion[3], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")
