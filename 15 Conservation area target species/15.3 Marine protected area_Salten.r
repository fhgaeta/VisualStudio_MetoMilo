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
file_path_gdb <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/22. Species_habitats and areas of conservation/22.4 marin_vp_oppstart/marin_vp_oppstart.shp"

# Load the 100m study area raster
r_area <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/Nord-Salten_1108_08.tif"))

# Load EUSeaMap_2023
db_layers <- sf::st_layers(file_path_gdb)$name
shp <- purrr::map(
  db_layers, sf::st_read, dsn = file_path_gdb, quiet = TRUE
) %>% 
  bind_rows() %>% 
  sf::st_make_valid()

# List all categories present in the column "kategori"
categories <- shp %>%
  distinct(kategori) %>%
  pull(kategori) %>%
  as.character()

cat("Categories in 'fjord':\n")
cat(paste(categories, collapse = "\n"))

# List of species to rasterize
for (i in seq_along(categories)) {
    # Select current species
    habitat <- categories[i]
    cat(paste0(habitat, ": "))

    # Filter shape for selected species
    shp_filtered <- shp %>% filter(kategori == habitat)

    # Check if shapefile data is empty
    if (nrow(shp_filtered) == 0) {
        cat("No data found for habitat: ", habitat, "\n")
        next
    }

    # Check if shapefile data falls within the raster extent
    if (is.null(terra::intersect(terra::ext(r_area), terra::ext(shp_filtered)))) {
        cat("Shapefile data falls outside the raster extent for habitat: ", habitat, "\n")
        next
    }
    
    # Convert habitat name to valid encoding and replace spaces with underscores
    habitat_clean <- iconv(habitat, from = "UTF-8", to = "ASCII//TRANSLIT")
    habitat_clean <- gsub(" ", "_", habitat_clean)
    
    # Define output CSV file name
    file_out <- paste0(local_folder, "15.3_", habitat_clean, "_Salten.csv")
    file_out <- normalizePath(file_out, mustWork = FALSE)

    # Rasterize and save CSV
    df <- rasterise_mm(r_area, shp_filtered, variable = "kategori", return_df = TRUE, filecsv = file_out)
    cat(paste0(nrow(df), "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out, fileEncoding = "UTF-8")
}
