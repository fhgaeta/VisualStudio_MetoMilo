# Load necessary libraries
library(dplyr)
library(sf)
library(terra)

source("function_rasterise_metomilo.R")

# Set locale
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Get user name
user <- Sys.getenv("USERNAME")
local_folder <- paste0("C:/Users/FEG/Downloads/TEST_Metomilo_Local/")
folder_base <- "C:/Users/FEG/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/"
folder_area <- paste0(folder_base, "Focus areas/grid_v3/raster/")
folder_output_csv <- paste0(folder_base, "../Analyses/input_data/ecosystem_components/")
basefolder_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/"

# Define the path to the shapefiles
file_path_gdb_jeren <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/EUSeaMap_Europe_2023/EUSeaMap_2023_Jeren/EUSeaMap_2023_Jeren.shp"
file_path_gdb_sar_hab <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/21. Marine grunnkart - Sårbare habitater/SaltvannssjobunntyperPredikert_FGDB.gdb"

# Load the 100m study area raster
r_area <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/Jæren_5104_01.tif"))

# Load EUSeaMap_2023
db_layers <- sf::st_layers(file_path_gdb_jeren)$name
shp_sea_map_23 <- purrr::map(
  db_layers, sf::st_read, dsn = file_path_gdb_jeren, quiet = TRUE
) %>% 
  bind_rows() %>% 
  sf::st_make_valid()

# Load 21 Marine grunnkart - Saltvannssjobunntyper Predikert
db_layers <- sf::st_layers(file_path_gdb_sar_hab)$name
shp_sar_hab <- purrr::map(
  db_layers, sf::st_read, dsn = file_path_gdb_sar_hab, quiet = TRUE
) %>% 
  bind_rows()

# List all categories present in the column "All2019DL2"
categories <- shp_sea_map_23 %>%
  distinct(All2019DL2) %>%
  pull(All2019DL2) %>%
  as.character()

cat("Categories in 'All2019DL2':\n")
cat(paste(categories, collapse = "\n"))

# List all categories present in the column "ninbeskrivelse"
categories2 <- shp_sar_hab %>% 
  distinct(ninbeskrivelse) %>% 
  pull(ninbeskrivelse) %>% 
  as.character()

cat("Categories in 'ninbeskrivelse':\n")
cat(paste(categories2, collapse = "\n"))

# List of species to rasterize according to Ecomar / Metomilo: Infralittoral sand and muddy sand
# categories_to_merge <- list(
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk litt og temmelig finmaterialrik bunn med temmelig liten til intermediær erosjonsmotstand."),
#   list("MB1: Infralittoral rock", "Eufotisk fast saltvannsbunn."),
#   list("MB4: Infralittoral mixed sediment", "Eufotisk marin sedimentbunn - brakk eufotisk uten finmateriale og finmaterialfattig bunn med liten erosjonsmotstand."),
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk og temmelig finmaterialrik bunn med liten erosjonsmotstand."),
#   list("Circalittoral seabed", "Afotisk marin sedimentbunn."),
#   list("Circalittoral rock", "Afotisk fast saltvannsbunn."),
#   list("Afotisk fast saltvannsbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.", "Marin undervannseng.", "Afotisk marin sedimentbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i atlantisk vann.", "Afotisk marin sedimentbunn - finmaterialdominert afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.")
# )

# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

cat1 <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c("MB5: Infralittoral sand")) %>%
  distinct(All2019DL2) %>%
  pull(All2019DL2) %>%
  as.character()

cat2 <- shp_sar_hab %>%
  filter(ninbeskrivelse %in% c("Eufotisk marin sedimentbunn - brakk eufotisk litt og temmelig finmaterialrik bunn med temmelig liten til intermediær erosjonsmotstand.")) %>%
  distinct(ninbeskrivelse) %>%
  pull(ninbeskrivelse) %>%
  as.character()

cat3 <- shp_sar_hab %>%
  filter(ninbeskrivelse %in% c("Eufotisk marin sedimentbunn - brakk eufotisk uten finmateriale og finmaterialfattig bunn med liten erosjonsmotstand.")) %>%
  distinct(ninbeskrivelse) %>%
  pull(ninbeskrivelse) %>%
  as.character()

# Select current habitats
habitat <- paste0(cat1, cat2, cat3)
cat(paste0(habitat, ": "))

# Filter shape for selected species
shp1 <- shp_sea_map_23 %>% filter(All2019DL2 == cat1)
shp2 <- shp_sar_hab %>% filter(ninbeskrivelse == cat2)
shp3 <- shp_sar_hab %>% filter(ninbeskrivelse == cat3)

# Merge the spatial positions
merged <- st_union(shp1, shp2, shp3)

# Ensure the merged object has a geometry column
st_geometry(merged) <- "geometry"

# Add a new column to the merged object
merged$common_column <- paste0(merged$ninbeskrivelse, "_", merged$All2019DL2)

# Define output CSV file name
file_out <- paste0(local_folder, "14.10_Infralittoral sand and muddy sand_Jæren.csv")


# Use the new column as the variable in the rasterise_mm function
df <- rasterise_mm(
  r_area,
  merged,
  variable = "common_column",
  return_df = TRUE,
  filecsv = file_out
)

cat(paste0(nrow(df), "\n"))

# Save the data to a CSV file
write.csv(df, file_out)

# List of species to rasterize according to Ecomar / Metomilo:  Infralittoral coarse sediments
# categories_to_merge <- list(
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk litt og temmelig finmaterialrik bunn med temmelig liten til intermediær erosjonsmotstand."),
#   list("MB1: Infralittoral rock", "Eufotisk fast saltvannsbunn."),
#   list("MB4: Infralittoral mixed sediment", "Eufotisk marin sedimentbunn - brakk eufotisk uten finmateriale og finmaterialfattig bunn med liten erosjonsmotstand."),
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk og temmelig finmaterialrik bunn med liten erosjonsmotstand."),
#   list("Circalittoral seabed", "Afotisk marin sedimentbunn."),
#   list("Circalittoral rock", "Afotisk fast saltvannsbunn."),
#   list("Afotisk fast saltvannsbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.", "Marin undervannseng.", "Afotisk marin sedimentbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i atlantisk vann.", "Afotisk marin sedimentbunn - finmaterialdominert afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.")
# )

# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

cat2 <- shp_sar_hab %>%
  filter(ninbeskrivelse %in% c("Eufotisk marin sedimentbunn - brakk eufotisk uten finmateriale og finmaterialfattig bunn med liten erosjonsmotstand.")) %>%
  distinct(ninbeskrivelse) %>%
  pull(ninbeskrivelse) %>%
  as.character()


# Select current habitats
habitat <- paste0(cat1, cat2, cat3)
cat(paste0(habitat, ": "))

# Filter shape for selected species
shp2 <- shp_sar_hab %>% filter(ninbeskrivelse == cat2)

# Ensure the merged object has a geometry column
st_geometry(shp2) <- "geometry"


# Define output CSV file name
file_out <- paste0(local_folder, "14.10_ Infralittoral coarse sediments_Jæren.csv")


# Use the new column as the variable in the rasterise_mm function
df <- rasterise_mm(
  r_area,
  merged,
  variable = "ninbeskrivelse",
  return_df = TRUE,
  filecsv = file_out
)

cat(paste0(nrow(df), "\n"))

# Save the data to a CSV file
write.csv(df, file_out)

# List of species to rasterize according to Ecomar / Metomilo: Infralittoral rocks and biogenic reefs
# categories_to_merge <- list(
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk litt og temmelig finmaterialrik bunn med temmelig liten til intermediær erosjonsmotstand."),
#   list("MB1: Infralittoral rock", "Eufotisk fast saltvannsbunn."),
#   list("MB4: Infralittoral mixed sediment", "Eufotisk marin sedimentbunn - brakk eufotisk uten finmateriale og finmaterialfattig bunn med liten erosjonsmotstand."),
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk og temmelig finmaterialrik bunn med liten erosjonsmotstand."),
#   list("Circalittoral seabed", "Afotisk marin sedimentbunn."),
#   list("Circalittoral rock", "Afotisk fast saltvannsbunn."),
#   list("Afotisk fast saltvannsbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.", "Marin undervannseng.", "Afotisk marin sedimentbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i atlantisk vann.", "Afotisk marin sedimentbunn - finmaterialdominert afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.")
# )

# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

cat1 <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c("MB1: Infralittoral rock")) %>%
  distinct(All2019DL2) %>%
  pull(All2019DL2) %>%
  as.character()

cat2 <- shp_sar_hab %>%
  filter(ninbeskrivelse %in% c("Eufotisk fast saltvannsbunn.")) %>%
  distinct(ninbeskrivelse) %>%
  pull(ninbeskrivelse) %>%
  as.character()

# Select current habitats
habitat <- paste0(cat1, cat2)
cat(paste0(habitat, ": "))

# Filter shape for selected species
shp1 <- shp_sea_map_23 %>% filter(All2019DL2 == cat1)
shp2 <- shp_sar_hab %>% filter(ninbeskrivelse == cat2)


# Merge the spatial positions
merged <- st_union(shp1, shp2)

# Ensure the merged object has a geometry column
st_geometry(merged) <- "geometry"

# Add a new column to the merged object
merged$common_column <- paste0(merged$ninbeskrivelse, "_", merged$All2019DL2)

# Define output CSV file name
file_out <- paste0(local_folder, "14.10_Infralittoral rocks and biogenic reefs_Jæren.csv")

# Use the new column as the variable in the rasterise_mm function
df <- rasterise_mm(
  r_area,
  merged,
  variable = "common_column",
  return_df = TRUE,
  filecsv = file_out
)

cat(paste0(nrow(df), "\n"))

# Save the data to a CSV file
write.csv(df, file_out)

# List of species to rasterize according to Ecomar / Metomilo: Infralittoral mixed sediments 
# categories_to_merge <- list(
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk litt og temmelig finmaterialrik bunn med temmelig liten til intermediær erosjonsmotstand."),
#   list("MB1: Infralittoral rock", "Eufotisk fast saltvannsbunn."),
#   list("MB4: Infralittoral mixed sediment", "Eufotisk marin sedimentbunn - brakk eufotisk uten finmateriale og finmaterialfattig bunn med liten erosjonsmotstand."),
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk og temmelig finmaterialrik bunn med liten erosjonsmotstand."),
#   list("Circalittoral seabed", "Afotisk marin sedimentbunn."),
#   list("Circalittoral rock", "Afotisk fast saltvannsbunn."),
#   list("Afotisk fast saltvannsbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.", "Marin undervannseng.", "Afotisk marin sedimentbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i atlantisk vann.", "Afotisk marin sedimentbunn - finmaterialdominert afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.")
# )

# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

cat1 <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c("MB4: Infralittoral mixed sediment")) %>%
  distinct(All2019DL2) %>%
  pull(All2019DL2) %>%
  as.character()

# Select current habitats
habitat <- paste0(cat1)
cat(paste0(habitat, ": "))

# Filter shape for selected species
shp1 <- shp_sea_map_23 %>% filter(All2019DL2 == cat1)

# Ensure the merged object has a geometry column
st_geometry(shp1) <- "geometry"


# Define output CSV file name
file_out <- paste0(local_folder, "14.10_Infralittoral mixed sediments _Jæren.csv")


# Use the new column as the variable in the rasterise_mm function
df <- rasterise_mm(
  r_area,
  merged,
  variable = "All2019DL2",
  return_df = TRUE,
  filecsv = file_out
)

cat(paste0(nrow(df), "\n"))

# Save the data to a CSV file
write.csv(df, file_out)

# List of species to rasterize according to Ecomar / Metomilo: Circalittoral seabed (soft & hardbottom, includes offshore circalittoral seabed)
# categories_to_merge <- list(
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk litt og temmelig finmaterialrik bunn med temmelig liten til intermediær erosjonsmotstand."),
#   list("MB1: Infralittoral rock", "Eufotisk fast saltvannsbunn."),
#   list("MB4: Infralittoral mixed sediment", "Eufotisk marin sedimentbunn - brakk eufotisk uten finmateriale og finmaterialfattig bunn med liten erosjonsmotstand."),
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk og temmelig finmaterialrik bunn med liten erosjonsmotstand."),
#   list("Circalittoral seabed", "Afotisk marin sedimentbunn."),
#   list("Circalittoral rock", "Afotisk fast saltvannsbunn."),
#   list("Afotisk fast saltvannsbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.", "Marin undervannseng.", "Afotisk marin sedimentbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i atlantisk vann.", "Afotisk marin sedimentbunn - finmaterialdominert afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.")
# )

# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

cat1 <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c("Circalittoral seabed")) %>%
  distinct(All2019DL2) %>%
  pull(All2019DL2) %>%
  as.character()

cat2 <- shp_sar_hab %>%
  filter(ninbeskrivelse %in% c( "Afotisk marin sedimentbunn.")) %>%
  distinct(ninbeskrivelse) %>%
  pull(ninbeskrivelse) %>%
  as.character()

# Select current habitats
habitat <- paste0(cat1, cat2)
cat(paste0(habitat, ": "))

# Filter shape for selected species
shp1 <- shp_sea_map_23 %>% filter(All2019DL2 == cat1)
shp2 <- shp_sar_hab %>% filter(ninbeskrivelse == cat2)


# Merge the spatial positions
merged <- st_union(shp1, shp2)

# Ensure the merged object has a geometry column
st_geometry(merged) <- "geometry"

# Add a new column to the merged object
merged$common_column <- paste0(merged$ninbeskrivelse, "_", merged$All2019DL2)

# Define output CSV file name
file_out <- paste0(local_folder, "14.10_Circalittoral seabed_Jæren.csv")


# Use the new column as the variable in the rasterise_mm function
df <- rasterise_mm(
  r_area,
  merged,
  variable = "common_column",
  return_df = TRUE,
  filecsv = file_out
)

cat(paste0(nrow(df), "\n"))

# Save the data to a CSV file
write.csv(df, file_out)

# List of species to rasterize according to Ecomar / Metomilo: Circalittoral mud (includes Offshore circalittoral mud)
# categories_to_merge <- list(
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk litt og temmelig finmaterialrik bunn med temmelig liten til intermediær erosjonsmotstand."),
#   list("MB1: Infralittoral rock", "Eufotisk fast saltvannsbunn."),
#   list("MB4: Infralittoral mixed sediment", "Eufotisk marin sedimentbunn - brakk eufotisk uten finmateriale og finmaterialfattig bunn med liten erosjonsmotstand."),
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk og temmelig finmaterialrik bunn med liten erosjonsmotstand."),
#   list("Circalittoral seabed", "Afotisk marin sedimentbunn."),
#   list("Circalittoral rock", "Afotisk fast saltvannsbunn."),
#   list("Afotisk fast saltvannsbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.", "Marin undervannseng.", "Afotisk marin sedimentbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i atlantisk vann.", "Afotisk marin sedimentbunn - finmaterialdominert afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.")
# )

# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

cat3 <- shp_sar_hab %>%
  filter(ninbeskrivelse %in% c("Afotisk marin sedimentbunn - finmaterialdominert afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.")) %>%
  distinct(ninbeskrivelse) %>%
  pull(ninbeskrivelse) %>%
  as.character()

shp3 <- shp_sar_hab %>% filter(ninbeskrivelse == cat3)

# Ensure the merged object has a geometry column
st_geometry(shp3) <- "geometry"

# Define output CSV file name
file_out <- paste0(local_folder, "14.10_Circalittoral mud_Jæren.csv")


# Use the new column as the variable in the rasterise_mm function
df <- rasterise_mm(
  r_area,
  merged,
  variable = "ninbeskrivelse",
  return_df = TRUE,
  filecsv = file_out
)

cat(paste0(nrow(df), "\n"))

# Save the data to a CSV file
write.csv(df, file_out)

# List of species to rasterize according to Ecomar / Metomilo:  Circalittoral coarse sediments (includes Offshore circalittoral coarse sediments)
# categories_to_merge <- list(
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk litt og temmelig finmaterialrik bunn med temmelig liten til intermediær erosjonsmotstand."),
#   list("MB1: Infralittoral rock", "Eufotisk fast saltvannsbunn."),
#   list("MB4: Infralittoral mixed sediment", "Eufotisk marin sedimentbunn - brakk eufotisk uten finmateriale og finmaterialfattig bunn med liten erosjonsmotstand."),
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk og temmelig finmaterialrik bunn med liten erosjonsmotstand."),
#   list("Circalittoral seabed", "Afotisk marin sedimentbunn."),
#   list("Circalittoral rock", "Afotisk fast saltvannsbunn."),
#   list("Afotisk fast saltvannsbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.", "Marin undervannseng.", "Afotisk marin sedimentbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i atlantisk vann.", "Afotisk marin sedimentbunn - finmaterialdominert afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.")
# )

# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}


cat2 <- shp_sar_hab %>%
  filter(ninbeskrivelse %in% c("Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i atlantisk vann.")) %>%
  distinct(ninbeskrivelse) %>%
  pull(ninbeskrivelse) %>%
  as.character()


# Select current habitats
habitat <- paste0(cat2)
cat(paste0(habitat, ": "))

# Filter shape for selected species

shp2 <- shp_sar_hab %>% filter(ninbeskrivelse == cat2)


# Ensure the merged object has a geometry column
st_geometry(shp2) <- "geometry"

# Define output CSV file name
file_out <- paste0(local_folder, "14.10_ Circalittoral coarse sediments (includes Offshore circalittoral coarse sediments)_Jæren.csv")


# Use the new column as the variable in the rasterise_mm function
df <- rasterise_mm(
  r_area,
  merged,
  variable = "ninbeskrivelse",
  return_df = TRUE,
  filecsv = file_out
)

cat(paste0(nrow(df), "\n"))

# Save the data to a CSV file
write.csv(df, file_out)

# List of species to rasterize according to Ecomar / Metomilo: Circalittoral rocks and biogenic reefs
# categories_to_merge <- list(
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk litt og temmelig finmaterialrik bunn med temmelig liten til intermediær erosjonsmotstand."),
#   list("MB1: Infralittoral rock", "Eufotisk fast saltvannsbunn."),
#   list("MB4: Infralittoral mixed sediment", "Eufotisk marin sedimentbunn - brakk eufotisk uten finmateriale og finmaterialfattig bunn med liten erosjonsmotstand."),
#   list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk og temmelig finmaterialrik bunn med liten erosjonsmotstand."),
#   list("Circalittoral seabed", "Afotisk marin sedimentbunn."),
#   list("Circalittoral rock", "Afotisk fast saltvannsbunn."),
#   list("Afotisk fast saltvannsbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.", "Marin undervannseng.", "Afotisk marin sedimentbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i atlantisk vann.", "Afotisk marin sedimentbunn - finmaterialdominert afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.")
# )

# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

cat1 <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c("Circalittoral rock")) %>%
  distinct(All2019DL2) %>%
  pull(All2019DL2) %>%
  as.character()

cat2 <- shp_sar_hab %>%
  filter(ninbeskrivelse %in% c("Afotisk fast saltvannsbunn.")) %>%
  distinct(ninbeskrivelse) %>%
  pull(ninbeskrivelse) %>%
  as.character()

# Select current habitats
habitat <- paste0(cat1, cat2, cat3)
cat(paste0(habitat, ": "))

# Filter shape for selected species
shp1 <- shp_sea_map_23 %>% filter(All2019DL2 == cat1)
shp2 <- shp_sar_hab %>% filter(ninbeskrivelse == cat2)

# Merge the spatial positions
merged <- st_union(shp1, shp2)

# Ensure the merged object has a geometry column
st_geometry(merged) <- "geometry"

# Add a new column to the merged object
merged$common_column <- paste0(merged$ninbeskrivelse, "_", merged$All2019DL2)

# Define output CSV file name
file_out <- paste0(local_folder, "14.10_Circalittoral rocks and biogenic reefs_Jæren.csv")


# Use the new column as the variable in the rasterise_mm function
df <- rasterise_mm(
  r_area,
  merged,
  variable = "common_column",
  return_df = TRUE,
  filecsv = file_out
)

cat(paste0(nrow(df), "\n"))

# Save the data to a CSV file
write.csv(df, file_out)
