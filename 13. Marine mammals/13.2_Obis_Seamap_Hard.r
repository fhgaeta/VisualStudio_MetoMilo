# Load necessary libraries
library(ggplot2)
library(readr)
library(dplyr)
library(sf)

# Define the path to the CSV file
file_path_shp <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/13. Marine mammals/13.2 obis_seamap_custom/shp_features_jer_hard.shp"

# Define folder
user <- Sys.getenv("USERNAME")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

basefolder <- paste0("C:/Users/", user, 
       "/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/",
       "AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/GIS Data/")
sub_dir <- paste0("C:/Users/", user, 
      "/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/",
      "AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/Focus areas/")
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output"

# Load the saved .rds files
shp_5109 <- readRDS(paste0(folder_output_od, "shp_5109_1000.rds"))
shp_water <- readRDS(paste0(folder_output_od, "shp_water.rds"))
rw_shp_hard <- readRDS(paste0(folder_output_od, "rw_shp_Hard.rds"))

plot(rw_shp_hard)

# Read shp file Obis Hard
shp_obis_hard <- read_sf(file_path_shp)

# Plot the shapefile Obis Hard
ggplot () +
  geom_sf(data = shp_obis_hard, fill = "transparent", color = "black") +
  labs(title = "Obis Hard") +
  theme_void()

# Set locale to Norwegian
Sys.setlocale("LC_ALL", "no_NO.UTF-8")

# Load study area polygons
study_areas <- read_sf(paste0(sub_dir, "Focus areas_boundary.shp"))
# Get the projection from the geonorge layer - we will use this as the basis for our work
crs_proj <- sf::st_crs(shp_water)

# Transform polygons to UTM33 projected coordinates
# The polygons are defined in lat/long coordinates. Now we want to transform them to UTM33 projected coordinates.
study_areas_proj <- study_areas %>%
  sf::st_transform(crs = crs_proj)
shp_water_proj <- shp_water %>%
  sf::st_transform(crs = crs_proj)
shp_obis_hard_proj <- shp_obis_hard %>%
  sf::st_transform(crs = crs_proj)

# Select only the attribute titled "vannregion" and the row "1108"
selected_region <- study_areas_proj %>%
  filter(vannregion == "5109")

# Intersect the shapefile with the selected region (1108)
shp_obis_hard_intersect <- sf::st_intersection(
  shp_obis_hard_proj,
  selected_region
)

# Plot the intersected shapefile categorized by species with different colors
p <- ggplot() +
  geom_sf(
    data = shp_obis_hard_intersect, 
    aes(color = species),
    fill = "transparent"
  ) +
  geom_sf(data = selected_region, fill = "transparent", color = "black") +
  geom_sf(data = rw_shp_hard, fill = "transparent", color = "black") +
  scale_fill_viridis_d() +
  labs(title = "Shapefile within the Study Area categorized by Species", fill = "Species") +
  theme(legend.position = "bottom")

# Display the plot
print(p)

# Save the categorized plot
ggsave(p, filename=paste0(folder_output_od, "/13. Marine mammals/Marine mammals observations Hard by Species ", selected_region$vannregion[1], ".png"),
dpi=300, height=20, width=20, units="cm", bg="white")

# Define the extent (Box)
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

# Test plot (only for raster)
plot (r)

# Rasterize the intersected shapefile
r_hard_marine_mammals <- terra::rasterize(
  shp_obis_hard_intersect,
  r,
  field = "num_record",
  fun = mean
)

# Convert the rasterized data to polygons for visualization
rasterized_hard_mammals_shp <- terra::as.polygons(r_hard_marine_mammals, aggregate = TRUE, values = TRUE)

# Ensure the object is an sf object
rasterized_hard_mammals_shp <- sf::st_as_sf(rasterized_hard_mammals_shp)

# Check if the geometry column is present
print(names(rasterized_hard_mammals_shp))

# Create the plot with color differentiation for different fish spawning categories
p2 <- ggplot() +
  geom_sf(
  data = rasterized_hard_mammals_shp, 
  aes(fill = mean),
  color = "black", 
  alpha = 0.8
  ) +
  geom_sf(data = rw_shp_hard, colour = "grey", fill = NA, alpha = 0.1) +
  geom_sf(data = shp_5109, colour = "red", fill = NA) + 
  geom_sf(data = shp_water, colour = NA, fill = "lightblue", alpha = 0.5) + 
  coord_sf(xlim = c(x0, x1), ylim = c(y0, y1), crs = crs_proj) +
  labs(title = "Rasterized marine mammals observations areas Hard", fill = "Mean") +
  theme_minimal()

# Display the plot
print(p2)

# Save the raster plot
ggsave(p2, filename=paste0(folder_output_od, "/13. Marine mammals/Rasterized marine mammals observations Hard", selected_region$vannregion[1], ".png"),
   dpi=300, height=20, width=20, units="cm", bg="white")
