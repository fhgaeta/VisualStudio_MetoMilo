# Load necessary libraries
library(ggplot2)
library(readr)
library(dplyr)
library(sf)

# Define the path to the CSV file
file_path_gdb <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/16. Naturtyper. DN-Håndbok 19/16.1 Nord Salten DN Nat.19/Naturtyper_hb19_EgnDef_4326_FILEGDB.gdb"


#Define folder
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
  shp_1108 <- readRDS(paste0(folder_output_od, "shp_1108_1000.rds"))
  shp_water <- readRDS(paste0(folder_output_od, "shp_water.rds"))
  rw_shp_sal <- readRDS(paste0(folder_output_od, "rw_shp_Salten.rds"))

  plot(rw_shp_sal)

#read shp file DN-19 Salten
gdb_dn19_salten <- read_sf(file_path_gdb)

#db_layers <- sf::st_layers(gdb_dn19_salten) 
#db_layers <- db_layers$name

#plot the shapefile DN-19 Salten
ggplot () +
  geom_sf(data = gdb_dn19_salten, fill = "transparent", color = "black") +
  labs(title = "DN-19 Salten") +
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
gdb_dn19_salten_proj <- gdb_dn19_salten %>%
  sf::st_transform(crs = crs_proj)

# Select only the attribute titled "vannregion" and the row "1108"
selected_region <- study_areas_proj %>%
    filter(vannregion == "1108")

# Intersect the shapefile with the selected region (1108)
gdb_dn19_salten_intersect <- sf::st_intersection(
  gdb_dn19_salten_proj,
  selected_region
)

# Plot the intersected shapefile categorized by species with different colors
p <- ggplot() +
  geom_sf(data = gdb_dn19_salten_intersect, aes(fill = naturtype), color = "black") +
  geom_sf(data = selected_region, fill = "transparent", color = "black") +
  #geom_sf(data = rw_shp_sal, fill = "transparent", color = "black") +
  scale_fill_viridis_d() +
  labs(title = "Shapefile within the Study Area categorized by Naturtype", fill = "Naturtype") +
  theme_void()

# Display the plot
print(p)

# Save the categorized plot
ggsave(p, filename=paste0(folder_output_od, "/16. DN_Handbok_19/16.1 DN_Handbok_19/DN_Haandbok_19_Salten", selected_region$vannregion[1], ".png"),
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

# Create a numeric mapping for the "naturtype" attribute
gdb_dn19_salten_intersect$naturtype_num <- as.numeric(
  as.factor(gdb_dn19_salten_intersect$naturtype))

# Rasterize the vector data using the unmasked raster as a template
r_salt_dn19 <- terra::rasterize(
  gdb_dn19_salten_intersect,
  r,
  field = "naturtype_num",
  fun = "mean"
)

# Convert the rasterized data to polygons for visualization
r_salt_dn19_shp <- terra::as.polygons(
  r_salt_dn19, aggregate = TRUE, values = TRUE, field = "naturtype_num")

#| Ensure the object is an sf object
r_salt_dn19_shp <- sf::st_as_sf(r_salt_dn19_shp)

# Check if the geometry column is present
print(names(r_salt_dn19_shp))

  # Create the plot with color differentiation for different naturtypes
  p2 <- ggplot() +
    geom_sf(
      data = r_salt_dn19_shp,
      aes(fill = as.factor(naturtype_num)),
      color = NA,
      alpha = 0.8
    ) +
    geom_sf(data = rw_shp_sal, colour = "grey", fill = NA, alpha = 0.1) +
    geom_sf(data = shp_1108, colour = "red", fill = NA) +
    geom_sf(data = shp_water, colour = NA, fill = "lightblue", alpha = 0.5) +
    coord_sf(xlim = c(x0, x1), ylim = c(y0, y1), crs = crs_proj) +
    labs(
      title = "Rasterized large kelp forest Salten",
      fill = "Naturtype Number"
    ) +
    theme_minimal()

# Display the plot
print(p2)

# Save the raster plot
ggsave(p2, filename=paste0(folder_output_od, "/16. DN_Handbok_19/16.1 DN_Handbok_19/Rasterized DN_Handbok_19_16.1_Salten", selected_region$vannregion[1], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")
