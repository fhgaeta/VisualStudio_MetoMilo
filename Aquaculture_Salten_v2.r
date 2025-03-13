library(tidyr)
library(dplyr)
library(sf)
install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)
install.packages('tidyterra', repos='https://rspatial.r-universe.dev')
library(tidyterra)
library(ggplot2)
install.packages('patchwork', repos='https://rspatial.r-universe.dev')
library(patchwork)
library(whitebox)

### Read the raster for study area.

# Read the raster for the study area
user <- Sys.getenv("USERNAME")
local_folder <- paste0("C:/Users/FEG/Downloads/TEST_Metomilo_Local/")
basefolder_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/"
file_r <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/Nord-Salten_1108_08.tif"))

# Define point data
file_path_gdb <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Industries data sources/1.Aquaculture/OneDrive_2_3-7-2025/Nord-Salten_1108_08_akva_lok_sea.shp"
shp <- sf::st_read(file_path_gdb)
shp <- shp %>% sf::st_transform(crs=terra::crs(file_r))

# Add an id_source column to the shapefile data
shp$id_source <- 1:nrow(shp)

# Add a value 
shp <- shp %>%
  mutate(source_value=1)

# Extract coordinates from the shapefile
shp_coords <- shp %>% 
  sf::st_coordinates() %>% 
  as.data.frame()

# Add the coordinates to the shapefile data frame
shp <- shp %>% 
  mutate(x_source = shp_coords$X, y_source = shp_coords$Y)

# Plot the shapefile
ggplot() +
  geom_sf(data=shp) +
  theme_minimal()

# Define effect distance
effect_dist <- 1000 # meters

# Calculate distances
rpts_all <- file_r %>% 
  terra::xyFromCell(1:terra::ncell(file_r)) %>% 
  as.data.frame()

# Add the values of the grid cells
rvalues <- terra::values(file_r, na.rm=F, mat=F)
rpts_all$val <- rvalues

# Add an id column - we will use this later to add calculated pressure values back to the raster
rpts_all$id_rast <- 1:length(rvalues)

# Filter the data to exclude NA cells (not water)
rpts <- rpts_all %>% 
  filter(!is.na(val))

# Add an id to identify the rows in the filtered raster cells
rpts$id_filtered <- 1:nrow(rpts)

# Convert to spatial data
rpts <- rpts %>% 
  sf::st_as_sf(coords = c("x", "y"), crs=sf::st_crs(file_r), remove=F)

# Check which points in rpts are within the effect distance from the source points
df_dist <- sf::st_is_within_distance(rpts, shp, dist=effect_dist, sparse=F) %>% 
  as.data.frame()
# The result is a matrix with TRUE/FALSE values. We need to convert this to a data frame
nc <- ncol(df_dist)
# Add an id column to the data frame
df_dist$id_filtered <- 1:nrow(df_dist)
# Convert the matrix to a long format
df_dist <- df_dist %>% 
  pivot_longer(cols=all_of(1:nc), names_to = "id_source", names_prefix = "V", values_to = "value", names_transform = as.integer)
# Filter to keep only the rows where value is TRUE
df_dist <- df_dist %>% 
  filter(value==TRUE) %>% 
  select(-value)

# Now we have all grid cells in r which are within a distance of effect_dist from a source point
# We can now calculate the distance from each grid cell to each source point
rpts <- rpts %>% 
  sf::st_drop_geometry()
# Add an id column to the data frame
rpts <- rpts %>% 
  left_join(df_dist, by="id_filtered")
# Add the coordinates of the source points to the data frame
rpts <- rpts %>% 
  left_join(shp %>% sf::st_drop_geometry(), by="id_source")

# Calculate the distance as RMS of distances in x and y directions
rpts <- rpts %>% 
  mutate(dist=sqrt((x_source - x)^2 + (y_source - y)^2))

## Decay methods

#Now we calculate a factor `f` which gives the intensity at the
#calculated distance. This factor varies from *1.0* at the source point
#to *0.0* where the distance is greater than the effect distance
#`effect_dist`.

#We can use different functions to estimate the *decay*. For example an
#*inverse distance weighted* function (IDW) ${1 - \frac 1 {distance} }$ or 
#*inverse distance squared weighted* ( ${({1 - \frac 1 {distance})}^2 }$).
# Calculate the pressure given by carbon load

shp_car <- shp %>%
  mutate(pressure_carbon = (6.3254 * kapasitet_) + 8680.4)

shp_f <- shp %>%
  mutate(pressure_fosfor = (0.01 * kapasitet_))

shp_nitrogen <- shp_f %>%
  mutate(pressure_nitrogen = (0.10 * kapasitet_))

# Ensure id_source is present in rpts before joining
if (!"id_source" %in% colnames(rpts)) {
  stop("id_source column is missing in rpts")
}

# Add the pressure values to the rpts data frame
rpts <- rpts %>%
  left_join(shp_car %>% select(id_source, pressure_carbon), by = "id_source") %>%
  left_join(shp_nitrogen %>% select(id_source, pressure_nitrogen), by = "id_source") %>%
  left_join(shp_f %>% select(id_source, pressure_fosfor), by = "id_source")

### Calculate factors for the decay functions
# Apply the decay function (IDW^2)
rpts <- rpts %>%
  mutate(decay = (1 - (dist / (effect_dist + 1)))^2) %>%
  mutate(decay = ifelse(decay < 0, 0, decay)) 

# Sum the values for each grid cell
rpts <- rpts %>%
  group_by(id_rast) %>%
  summarise(value = sum(val, na.rm=T), .groups="drop")

# Add the values back to the raster
rpts_all <- rpts_all %>%
  left_join(rpts, by="id_rast")
rp <- file_r
terra::values(rp) <- rpts_all$value

# Plot the results
raster_df <- as.data.frame(rp, xy = TRUE, na.rm = FALSE)
names(raster_df)[3] <- "value"

# Summarize the value column
summary(raster_df$value)
# Plot the raster
str(raster_df)

p2 <- ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = value)) +
  scale_fill_distiller(palette = "Spectral", na.value = "transparent", guide = "none") +
  coord_sf(datum = sf::st_crs(file_r)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.25)
  )

print(p2)


### Saving results as csv

# Create a dataframe with all values from the raster including NAs
df <- rp %>%
  terra::values(na.rm = FALSE) %>%
  as.data.frame()
names(df) <- "value"

# Create a dataframe with all x, y coordinates from the raster 
# and join it to the values
coords <- rp %>%
  terra::xyFromCell(1:ncell(rp)) %>%
  as.data.frame()
df <- bind_cols(coords, df)

# Filter to keep only x, y, value where value is not NA
df <- df %>%
  filter(!is.na(value))

# Print the structure of the df object to debug
str(df)

# Call function to save the data to a CSV file
source("C:/Users/FEG/OneDrive - NIVA/VisualStudio script/MetoMILO/VisualStudio_MetoMilo/Ecological components/function_rasterise_metomilo.R") # Load the function

# Define output CSV file name
local_folder <- paste0("C:/Users/FEG/Downloads/TEST_Metomilo_Local/")
file_out <- paste0(local_folder, "aquaculture_effect_distance_Salten.csv")

# Rasterize and save CSV
df <- rasterise_mm(df, return_df = TRUE, filecsv = file_out)
cat(paste0(nrow(df), "\n"))
