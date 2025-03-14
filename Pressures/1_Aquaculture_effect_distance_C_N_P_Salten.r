library(tidyr)
library(dplyr)
library(sf)
install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)
install.packages('tidyterra', repos='https://rspatial.r-universe.dev')
library(tidyterra)
library(ggplot2)
install.packages('patchwork', repos='https://rspatial.r-universe.dev')

### Read the raster for study area.

# Read the raster for the study area
user <- Sys.getenv("USERNAME")
local_folder <- paste0("C:/Users/FEG/Downloads/TEST_Metomilo_Local/")
basefolder_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/"
file_r <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/Nord-Salten_1108_08.tif"))

### Define example point data

# Read the shapefile with the point data from Salten
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

#add a column to the shp file with the total carbon pressure based on the function: pressure_carbon = (6.3254 * kapasitet_) + 8680.4)
shp <- shp %>% 
  mutate(carbon_pressure = (6.3254 * kapasitet_) + 8680.4)

#add a column with the nitrogen pressure
shp <- shp %>% 
  mutate(nitrogen_pressure = (0.1 * kapasitet_) )

#add a column with the phosphorus pressure
shp <- shp %>% 
  mutate(phosphorus_pressure = (0.01 * kapasitet_) )

# Plot the shapefile
ggplot() +
  geom_sf(data=shp) +
  theme_minimal()

### Define effect distance

effect_dist <- 1000 # meters

### Calculate distances

# Get the coordinates of the centre points of the raster grid cells
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

### Decay methods

#Now we calculate a factor `f` which gives the intensity at the
#calculated distance. This factor varies from *1.0* at the source point
#to *0.0* where the distance is greater than the effect distance
#`effect_dist`.

#We can use different functions to estimate the *decay*. For example an
#*inverse distance weighted* function (IDW) ${1 - \frac 1 {distance} }$ or 
#*inverse distance squared weighted* ( ${({1 - \frac 1 {distance})}^2 }$).

x <- seq(0,effect_dist*1.05,1)

f1 <- 1-(x/(effect_dist+1))
f1 <- ifelse(f1<0,0,f1)
f2 <- f1^2
# Plot the decay functions
df_decay <- data.frame(x, f1, f2) %>%
  pivot_longer(cols=c(f1,f2), names_to = "method", values_to = "f")

ggplot(df_decay) +
  geom_line(aes(x=x, y=f, colour=method)) +
  geom_vline(xintercept=effect_dist, linetype=2, colour="#999999") +
  theme_minimal() +
  scale_color_manual(values=c("red","blue"),
    labels=c("IDW", expression({IDW}^2)),
    name="") +
  labs(x="Distance [m]", y="f") +
  theme(legend.position = c(0.8,0.9))

### Calculate factors



# Here we will use IDW: ${1 - \frac 1 {distance} }$. Unless you have information about a decay function which applies to a specific pressure source, then just use this function. 
#do the decay function for each source for carbon,nitrogen and phosporus pressure/site 

rpts <- rpts %>%
  mutate(f = (1 - (dist / (effect_dist + 1)))^2) %>%
  mutate(carbon_decay = carbon_pressure * f) %>%
  mutate(nitrogen_decay = nitrogen_pressure * f) %>%
  mutate(phosphorus_decay = phosphorus_pressure * f)


rpts <- rpts %>% 
  mutate(carbon_decay = ifelse(is.na(carbon_decay),0,carbon_decay)) %>%
  mutate(nitrogen_decay = ifelse(is.na(nitrogen_decay),0,nitrogen_decay)) %>% 
  mutate(phosphorus_decay = ifelse(is.na(phosphorus_decay),0,phosphorus_decay))

# We now have one row for each combination of raster cell and point source 
# (where they are within the distance limit)
# It is possible that a grid cell is within the effect range of more than one
# pressure source. The contributions from different sources should be summed

# Sum the values for each grid cell
rpts <- rpts %>%
  group_by(id_rast) %>%
  summarise(
    carbon_decay = sum(carbon_decay, na.rm = TRUE),
    nitrogen_decay = sum(nitrogen_decay, na.rm = TRUE),
    phosphorus_decay = sum(phosphorus_decay, na.rm = TRUE),
    .groups = "drop"
  )

# Add the values back to the raster
rpts_all <- rpts_all %>% 
  left_join(rpts, by="id_rast")

# Create a new raster with the calculated values for carbon, nitrogen and phosphorus
rp_carbon <- file_r
rp_nitrogen <- file_r
rp_phosphorus <- file_r

terra::values(rp_carbon) <- rpts_all$carbon_decay
terra::values(rp_nitrogen) <- rpts_all$nitrogen_decay
terra::values(rp_phosphorus) <- rpts_all$phosphorus_decay



### Plot the results 

# Convert the raster data to a data frame
raster_df_carbon <- as.data.frame(rp_carbon, xy = TRUE, na.rm = FALSE)
raster_df_nitrogen <- as.data.frame(rp_nitrogen, xy = TRUE, na.rm = FALSE)
raster_df_phosphorus <- as.data.frame(rp_phosphorus, xy = TRUE, na.rm = FALSE)

names(raster_df_carbon)[3] <- "carbon_decay"  # Ensure the third column is named 'carbon_decay'
names(raster_df_nitrogen)[3] <- "nitrogen_decay"  # Ensure the third column is named 'nitrogen_decay'
names(raster_df_phosphorus)[3] <- "phosphorus_decay"  # Ensure the third column is named 'phosphorus_decay'

# Convert the data frame to a tibble
raster_df_carbon <- as_tibble(raster_df_carbon)
raster_df_nitrogen <- as_tibble(raster_df_nitrogen)
raster_df_phosphorus <- as_tibble(raster_df_phosphorus)

# Plot the raster data
p_carbon <- ggplot() +
  geom_raster(data = raster_df_carbon, aes(x = x, y = y, fill = carbon_decay)) +
  scale_fill_distiller(palette = "Spectral", na.value = "transparent") +
  coord_sf(datum = sf::st_crs(file_r)) +
  theme_void() +
  labs(fill = "Carbon decay") +
  theme(legend.position = "bottom")

p_nitrogen <- ggplot() +
  geom_raster(data = raster_df_nitrogen, 
              aes(x = x, y = y, fill = nitrogen_decay)) +
  scale_fill_distiller(palette = "Spectral", na.value = "transparent") +
  coord_sf(datum = sf::st_crs(file_r)) +
  theme_void() +
  labs(fill = "Nitrogen decay") +
  theme(legend.position = "bottom")

p_phosphorus <- ggplot() +
  geom_raster(data = raster_df_phosphorus, 
              aes(x = x, y = y, fill = phosphorus_decay)) +
  scale_fill_distiller(palette = "Spectral", na.value = "transparent") +
  coord_sf(datum = sf::st_crs(file_r)) +
  theme_void() +
  labs(fill = "Phosphorus decay") +
  theme(legend.position = "bottom")


print(p_carbon)
print(p_nitrogen)
print(p_phosphorus)

### Saving results as csv

# create a dataframe with all values from the raster including NAs
raster_df_carbon <- rp_carbon %>%
  terra::values(na.rm = FALSE) %>%
  as.data.frame()
names(raster_df_carbon) <- "carbon_decay"

raster_df_nitrogen <- rp_nitrogen %>%
  terra::values(na.rm=F) %>%
  as.data.frame()
names(raster_df_nitrogen) <- "nitrogen_decay"

raster_df_phosphorus <- rp_phosphorus %>%
  terra::values(na.rm=F) %>%
  as.data.frame()
names(raster_df_phosphorus) <- "phosphorus_decay"
  
# create a dataframe with all x,y coordinate from the raster 
# and join it to the values
raster_df_carbon <- rp_carbon %>%
  terra::xyFromCell(1:ncell(rp_carbon)) %>%
  as.data.frame() %>%
  bind_cols(raster_df_carbon)

raster_df_nitrogen <- rp_nitrogen %>%
  terra::xyFromCell(1:ncell(rp_nitrogen)) %>%
  as.data.frame() %>%
  bind_cols(raster_df_nitrogen)

raster_df_phosphorus <- rp_phosphorus %>%
  terra::xyFromCell(1:ncell(rp_phosphorus)) %>%
  as.data.frame() %>%
  bind_cols(raster_df_phosphorus)

  
# filter to keep only x,y,value where value is not NA
raster_df_carbon <- raster_df_carbon %>%
  filter(!is.na(carbon_decay))

raster_df_nitrogen <- raster_df_nitrogen %>%
  filter(!is.na(nitrogen_decay))

raster_df_phosphorus <- raster_df_phosphorus %>%
  filter(!is.na(phosphorus_decay))

filename <- "effect_distances_carbon_Salten.csv"

# save the x,y,value data as csv 
write.table(raster_df_carbon, file = filename, sep = ",", quote = FALSE,
            fileEncoding = "UTF-8", row.names = FALSE)

write.table(raster_df_nitrogen, file = "effect_distances_nitrogen_Salten.csv", sep = ",", quote = FALSE,
            fileEncoding = "UTF-8", row.names = FALSE)

write.table(raster_df_phosphorus, file = "effect_distances_phosphorus_Salten.csv", sep = ",", quote = FALSE,
            fileEncoding = "UTF-8", row.names = FALSE)
            