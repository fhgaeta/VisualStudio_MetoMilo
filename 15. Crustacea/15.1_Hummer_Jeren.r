# Load necessary libraries
library(ggplot2)
library(readr)
library(dplyr)
library(sf)

# Set locale to Norwegian
Sys.setlocale("LC_ALL", "no_NO.UTF-8")

#Define folder
user <- Sys.getenv("USERNAME")

# Define the path to the CSV file
file_path_shp <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/15. Crustacea/15.1 Hummer/Hummer.shp"

basefolder <- paste0("C:/Users/", user, 
  "/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/",
  "AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/GIS Data/")
sub_dir <- paste0("C:/Users/", user, 
      "/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/",
      "AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/Focus areas/")
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output"

# Load the saved .rds files
shp_5104 <- readRDS(paste0(folder_output_od, "shp_5104_1000.rds"))
shp_water <- readRDS(paste0(folder_output_od, "shp_water.rds"))
rw_shp_jer <- readRDS(paste0(folder_output_od, "rw_shp_jer.rds"))

plot(rw_shp_jer)

#read shp file Obis jer
Hummer_shp_jer <- read_sf(file_path_shp)

#plot the shapefile Obis jer
ggplot () +
  geom_sf(data = Hummer_shp_jer, fill = "transparent", color = "black") +
  labs(title = "Hummer utbredelse jer") +
  theme_void()

#Load study area polygons
study_areas <- read_sf(paste0(sub_dir, "Focus areas_boundary.shp"))
# Get the projection from the geonorge layer - we will use this as the basis for our work
crs_proj <- sf::st_crs(shp_water)

# Transform polygons to UTM33 projected coordinates
# The polygons are defined in lat/long coordinates. Now we want to transform them to UTM33 projected coordinates.
study_areas_proj <- study_areas %>%
  sf::st_transform(crs = crs_proj)
shp_water_proj <- shp_water %>%
  sf::st_transform(crs = crs_proj)
Hummer_shp_jer_proj <- Hummer_shp_jer %>%
  sf::st_transform(crs = crs_proj)
  

# Select only the attribute titled "vannregion" and the row "5204"
selected_region <- study_areas_proj %>%
  filter(vannregion == "5104")

# Intersect the shapefile with the selected region (5204)
Hummer_shp_jer_intersect <- sf::st_intersection(
  Hummer_shp_jer_proj,
  selected_region
)

# Plot the intersected shapefile categorized by species with different colors
p <- ggplot() +
  geom_sf(data = Hummer_shp_jer_intersect, fill = "transparent", color = "black") +
  geom_sf(data = selected_region, fill = "transparent", color = "black") +
  geom_sf(data = rw_shp_jer, fill = "transparent", color = "black") +
  scale_fill_viridis_d() +
  labs(
  title = "Distribution lobster in jer", 
  fill = "Species"
  ) +
  theme_void()

# Display the plot
print(p)

# Save the categorized plot with legend
p <- p + theme(legend.position = "down", legend.title = element_text(size = 10), legend.text = element_text(size = 10))

ggsave(p, filename=paste0(folder_output_od, "/15. Crustacea/hummer_jer", selected_region$vannregion[3], ".png"),
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

#test plot (only for raster)
plot (r)

# Rasterize the intersected shapefile
# Check if the data is numeric before rasterizing
if ("wms_code" %in% colnames(Hummer_shp_jer_intersect) && 
  is.numeric(Hummer_shp_jer_intersect$wms_code)) {
  r_jer_hummer <- terra::rasterize(
  Hummer_shp_jer_intersect,
  r
  )
  # Check if aggregation is needed
  if (any(terra::freq(r_jer_hummer)$count > 1)) {
  # Convert the rasterized data to polygons with aggregation
  rasterized_jer_hummer_shp <- terra::as.polygons(
  r_jer_hummer,
  aggregate = TRUE,
  values = TRUE
  )
  } else {
  # Convert the rasterized data to polygons without aggregation
  rasterized_jer_hummer_shp <- terra::as.polygons(
  r_jer_hummer, 
  aggregate = FALSE, 
  values = TRUE
  )
  }
  rasterized_jer_hummer_shp <- sf::st_as_sf(rasterized_jer_hummer_shp)
} else {
  rasterized_jer_hummer_shp <- sf::st_as_sf(terra::as.polygons(r_jer_hummer))
}


# Check if the "geometry" column is present
print("geometry" %in% names(rasterized_jer_hummer_shp))

  # Create the plot with color differentiation for different fish 
  # spawning categories
  p2 <- ggplot() +
  geom_sf(
  data = rasterized_jer_hummer_shp,
  color = "black",
  alpha = 0.8
  ) +
  geom_sf(
  data = rw_shp_jer,
  colour = "grey",
  fill = NA,
  alpha = 0.1
  ) +
   geom_sf(
  data = selected_region,
  colour = NA,
  fill = "#e0e7e9",
  alpha = 0.5
  ) +
   geom_sf(
  data = shp_5104,
  colour = "red",
  fill = NA
  ) +
  coord_sf(
  xlim = c(x0, x1),
  ylim = c(y0, y1),
  crs = crs_proj
  ) +
  labs(
  title = "Rasterized marine hummer distributions areas jer",
  fill = "Mean"
  ) +
  theme_minimal()

# Display the plot
print(p2)

# Save the raster plot
ggsave(p2, filename=paste0(folder_output_od, "/15. Crustacea/Rasterized lobster distribution jer", selected_region$vannregion[3], ".png"),
   dpi=300, height=20, width=20, units="cm", bg="white")
