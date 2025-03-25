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
file_path_gdb <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/EUSeaMap_Europe_2023/EUSeaMap_2023_Hardanger/EUSeaMap_2023_Hardanger.shp"

# study area raster files
# "5109_02_03.tif" - combined Sunnhordaland & Hardanger            
# "5109_02_03_area.tif" - indicates if a grid cell is 02 or 03     
# "Hardanger_5109_03.tif" 
# "Jæren_5104_01.tif"         
# "Nord-Salten_1108_08.tif"   
# "Sunnhordaland_5109_02.tif"
# Load the 100m study area raster
r_area <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/5109_02_03_area.tif"))

# Load EUSeaMap_2023
db_layers <- sf::st_layers(file_path_gdb)$name
shp_sea_map_23 <- purrr::map(
  db_layers, sf::st_read, dsn = file_path_gdb, quiet = TRUE
) %>% 
  bind_rows() %>% 
  sf::st_make_valid()

# List all categories present in the column "All2019DL2"
categories <- shp_sea_map_23 %>%
  distinct(All2019DL2) %>%
  pull(All2019DL2) %>%
  as.character()

cat("Categories in 'All2019DL2':\n")
cat(paste(categories, collapse = "\n"))

# List of species to rasterize according to Ecomar / Metomilo: Infralittoral seabed (soft and hard bottom)
habitat_list <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c("Infralittoral seabed")) %>%
           distinct(All2019DL2) %>%
           pull(All2019DL2) %>%
           as.character()


# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

# List of habitat to rasterize

    # Select current species
    habitat <- habitat_list
    cat(paste0(habitat, ": "))

    # Filter shape for selected species
    shp <- shp_sea_map_23 %>% filter(All2019DL2 == habitat)

    # Check if shapefile data is empty
    if (nrow(shp) == 0) {
        cat("No data found for habitat: ", habitat, "\n")
        next
    }

    # Check if shapefile data falls within the raster extent
    if (is.null(terra::intersect(terra::ext(r_area), terra::ext(shp)))) {
        cat("Shapefile data falls outside the raster extent for habitat: ", habitat, "\n")
        next
    }
    
  # Define output CSV file name
  file_out <- paste0(local_folder, "14.10_", sanitize_filename(habitat), "_Hardanger.csv")
  

    # Rasterize and save CSV
    df <- rasterise_mm(r_area, shp, variable = "All2019DL2", return_df = TRUE, filecsv = file_out)
    cat(paste0(nrow(df), "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)

# List of habitat to rasterize according to Ecomar / Metomilo: Infralittoral mud
habitat_list <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c(
  "MB6: Infralittoral mud")) %>%
           distinct(All2019DL2) %>%
           pull(All2019DL2) %>%
           as.character()


# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

    # Select current species
    habitat <- habitat_list
    cat(paste0(habitat, ": "))

    # Filter shape for selected species
    shp <- shp_sea_map_23 %>% filter(All2019DL2 == habitat)

    # Check if shapefile data is empty
    if (nrow(shp) == 0) {
        cat("No data found for habitat: ", habitat, "\n")
        next
    }

    # Check if shapefile data falls within the raster extent
    if (is.null(terra::intersect(terra::ext(r_area), terra::ext(shp)))) {
        cat("Shapefile data falls outside the raster extent for habitat: ", habitat, "\n")
        next
    }
    
  # Define output CSV file name
  file_out <- paste0(local_folder, "14.10_", sanitize_filename(habitat), "_Hardanger.csv")
  

    # Rasterize and save CSV
    df <- rasterise_mm(r_area, shp, variable = "All2019DL2", return_df = TRUE, filecsv = file_out)
    cat(paste0(nrow(df), "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)


# List of species to rasterize according to Ecomar / Metomilo:  Infralittoral coarse sediments
habitat_list <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c(
  "MB3: Infralittoral coarse sediment"
  )) %>%
           distinct(All2019DL2) %>%
           pull(All2019DL2) %>%
           as.character()


# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

# List of habitat to rasterize

    # Select current species
    habitat <- habitat_list
    cat(paste0(habitat, ": "))

    # Filter shape for selected species
    shp <- shp_sea_map_23 %>% filter(All2019DL2 == habitat)

    # Check if shapefile data is empty
    if (nrow(shp) == 0) {
        cat("No data found for habitat: ", habitat, "\n")
        next
    }

    # Check if shapefile data falls within the raster extent
    if (is.null(terra::intersect(terra::ext(r_area), terra::ext(shp)))) {
        cat("Shapefile data falls outside the raster extent for habitat: ", habitat, "\n")
        next
    }
    
  # Define output CSV file name
  file_out <- paste0(local_folder, "14.10_", sanitize_filename(habitat), "_Hardanger.csv")
  

    # Rasterize and save CSV
    df <- rasterise_mm(r_area, shp, variable = "All2019DL2", return_df = TRUE, filecsv = file_out)
    cat(paste0(nrow(df), "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)

# List of species to rasterize according to Ecomar / Metomilo: Infralittoral rocks and biogenic reefs
habitat_list <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c(
  "MB1: Infralittoral rock"
  )) %>%
           distinct(All2019DL2) %>%
           pull(All2019DL2) %>%
           as.character()


# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

# List of habitat to rasterize

    # Select current species
    habitat <- habitat_list
    cat(paste0(habitat, ": "))

    # Filter shape for selected species
    shp <- shp_sea_map_23 %>% filter(All2019DL2 == habitat)

    # Check if shapefile data is empty
    if (nrow(shp) == 0) {
        cat("No data found for habitat: ", habitat, "\n")
        next
    }

    # Check if shapefile data falls within the raster extent
    if (is.null(terra::intersect(terra::ext(r_area), terra::ext(shp)))) {
        cat("Shapefile data falls outside the raster extent for habitat: ", habitat, "\n")
        next
    }
    
  # Define output CSV file name
  file_out <- paste0(local_folder, "14.10_", sanitize_filename(habitat), "_Hardanger.csv")
  

    # Rasterize and save CSV
    df <- rasterise_mm(r_area, shp, variable = "All2019DL2", return_df = TRUE, filecsv = file_out)
    cat(paste0(nrow(df), "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)


# List of species to rasterize according to Ecomar / Metomilo: Circalittoral seabed (soft & hardbottom, includes offshore circalittoral seabed)
habitat_list <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c(
  "Offshore circalittoral seabed",
  "Circalittoral seabed"
  )) %>%
           distinct(All2019DL2) %>%
           pull(All2019DL2) %>%
           as.character()


# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

# List of habitat to rasterize

    # Select current species
    habitat <- habitat_list
    cat(paste0(habitat, ": "))

    # Filter shape for selected species
    shp <- shp_sea_map_23 %>% filter(All2019DL2 == habitat)

    # Check if shapefile data is empty
    if (nrow(shp) == 0) {
        cat("No data found for habitat: ", habitat, "\n")
        next
    }

    # Check if shapefile data falls within the raster extent
    if (is.null(terra::intersect(terra::ext(r_area), terra::ext(shp)))) {
        cat("Shapefile data falls outside the raster extent for habitat: ", habitat, "\n")
        next
    }
    
  # Define output CSV file name
  file_out <- paste0(local_folder, "14.10_", sanitize_filename(habitat), "_Hardanger.csv")
  

    # Rasterize and save CSV
    df <- rasterise_mm(r_area, shp, variable = "All2019DL2", return_df = TRUE, filecsv = file_out)
    cat(paste0(nrow(df), "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)

# List of species to rasterize according to Ecomar / Metomilo: Circalittoral mud (includes Offshore circalittoral mud)
habitat_list <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c(
  "MD6: Offshore circalittoral mud",
  "MC6: Circalittoral mud",
  )) %>%
           distinct(All2019DL2) %>%
           pull(All2019DL2) %>%
           as.character()


# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

# List of habitat to rasterize

    # Select current species
    habitat <- habitat_list
    cat(paste0(habitat, ": "))

    # Filter shape for selected species
    shp <- shp_sea_map_23 %>% filter(All2019DL2 == habitat)

    # Check if shapefile data is empty
    if (nrow(shp) == 0) {
        cat("No data found for habitat: ", habitat, "\n")
        next
    }

    # Check if shapefile data falls within the raster extent
    if (is.null(terra::intersect(terra::ext(r_area), terra::ext(shp)))) {
        cat("Shapefile data falls outside the raster extent for habitat: ", habitat, "\n")
        next
    }
    
  # Define output CSV file name
  file_out <- paste0(local_folder, "14.10_", sanitize_filename(habitat), "_Hardanger.csv")
  

    # Rasterize and save CSV
    df <- rasterise_mm(r_area, shp, variable = "All2019DL2", return_df = TRUE, filecsv = file_out)
    cat(paste0(nrow(df), "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)

    # List of species to rasterize according to Ecomar / Metomilo:  Circalittoral coarse sediments (includes Offshore circalittoral coarse sediments)
habitat_list <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c(
  "MD3: Offshore circalittoral coarse sediment",
  "MC3: Circalittoral coarse sediment"
  )) %>%
           distinct(All2019DL2) %>%
           pull(All2019DL2) %>%
           as.character()


# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

# List of habitat to rasterize

    # Select current species
    habitat <- habitat_list
    cat(paste0(habitat, ": "))

    # Filter shape for selected species
    shp <- shp_sea_map_23 %>% filter(All2019DL2 == habitat)

    # Check if shapefile data is empty
    if (nrow(shp) == 0) {
        cat("No data found for habitat: ", habitat, "\n")
        next
    }

    # Check if shapefile data falls within the raster extent
    if (is.null(terra::intersect(terra::ext(r_area), terra::ext(shp)))) {
        cat("Shapefile data falls outside the raster extent for habitat: ", habitat, "\n")
        next
    }
    
  # Define output CSV file name
  file_out <- paste0(local_folder, "14.10_", sanitize_filename(habitat), "_Hardanger.csv")
  

    # Rasterize and save CSV
    df <- rasterise_mm(r_area, shp, variable = "All2019DL2", return_df = TRUE, filecsv = file_out)
    cat(paste0(nrow(df), "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)

# List of species to rasterize according to Ecomar / Metomilo: Circalittoral rocks and biogenic reefs
habitat_list <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c(
  "MC1: Circalittoral rock",
  "MD1: Offshore circalittoral rock")) %>%
           distinct(All2019DL2) %>%
           pull(All2019DL2) %>%
           as.character()


# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

# List of habitat to rasterize

    # Select current species
    habitat <- habitat_list
    cat(paste0(habitat, ": "))

    # Filter shape for selected species
    shp <- shp_sea_map_23 %>% filter(All2019DL2 == habitat)

    # Check if shapefile data is empty
    if (nrow(shp) == 0) {
        cat("No data found for habitat: ", habitat, "\n")
        next
    }

    # Check if shapefile data falls within the raster extent
    if (is.null(terra::intersect(terra::ext(r_area), terra::ext(shp)))) {
        cat("Shapefile data falls outside the raster extent for habitat: ", habitat, "\n")
        next
    }
    
  # Define output CSV file name
  file_out <- paste0(local_folder, "14.10_", sanitize_filename(habitat), "_Hardanger.csv")
  

    # Rasterize and save CSV
    df <- rasterise_mm(r_area, shp, variable = "All2019DL2", return_df = TRUE, filecsv = file_out)
    cat(paste0(nrow(df), "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)

# List of species to rasterize according to Ecomar / Metomilo: Upper bathyal sediments
habitat_list <- shp_sea_map_23 %>%
  filter(All2019DL2 %in% c(
  "ME1: Upper bathyal rock",
  "Upper bathyal seabed")) %>%
           distinct(All2019DL2) %>%
           pull(All2019DL2) %>%
           as.character()


# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

# List of habitat to rasterize

    # Select current species
    habitat <- habitat_list
    cat(paste0(habitat, ": "))

    # Filter shape for selected species
    shp <- shp_sea_map_23 %>% filter(All2019DL2 == habitat)

    # Check if shapefile data is empty
    if (nrow(shp) == 0) {
        cat("No data found for habitat: ", habitat, "\n")
        next
    }

    # Check if shapefile data falls within the raster extent
    if (is.null(terra::intersect(terra::ext(r_area), terra::ext(shp)))) {
        cat("Shapefile data falls outside the raster extent for habitat: ", habitat, "\n")
        next
    }
    
  # Define output CSV file name
  file_out <- paste0(local_folder, "14.10_", sanitize_filename(habitat), "_Hardanger.csv")
  

    # Rasterize and save CSV
    df <- rasterise_mm(r_area, shp, variable = "All2019DL2", return_df = TRUE, filecsv = file_out)
    cat(paste0(nrow(df), "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)