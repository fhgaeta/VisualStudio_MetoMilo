#plot single area - Jeren 14.8 Hummerutbredelse

#15. Crustacea

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

# Define the path to the shapefile
file_path_shp <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/15. Crustacea/15.1 Hummer/Hummer.shp"

# study area raster files
# "5109_02_03.tif" - combined Sunnhordaland & Hardanger            
# "5109_02_03_area.tif" - indicates if a grid cell is 02 or 03     
# "Hardanger_5109_03.tif" 
# "Jæren_5104_01.tif"         
# "Nord-Salten_1108_08.tif"   
# "Sunnhordaland_5109_02.tif"

# Load the 100m study area raster
r_area <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/Jæren_5104_01.tif"))

# Load hummer 
db_layers <- sf::st_layers(file_path_shp)$name
shp_hummer <- purrr::map(
  db_layers, sf::st_read, dsn = file_path_shp, quiet = TRUE
) %>% bind_rows()

# List all categories present in the column "naturtype"
categories <- shp_hummer %>%
  distinct(species_no) %>%
  pull(species_no) %>%
  as.character()

cat("Categories in 'species_no':\n")
cat(paste(categories, collapse = "\n"))

# List of species to rasterize
species_list <- shp_hummer %>%
  filter(species_no %in% c("Hummer")) %>%
  distinct(species_no) %>%
  pull(species_no) %>%
  as.character()

for (i in seq_along(species_list)) {
  # Select current species
  species <- species_list[i]
  cat(paste0(species, ": "))

  # Filter shape for selected species
  shp <- shp_hummer %>% filter(species_no == species)
  
  # Check if the filtered data frame is empty
  if (nrow(shp) == 0) {
    cat("No data found for species: ", species, "\n")
    next
  }

  # Define output CSV file name
  file_out <- paste0(local_folder, "14.8_", species, "_Jæren", ".csv")

  # Rasterize and save CSV
  df <- rasterise_mm(
    r_area, shp, variable = "species_no", return_df = TRUE, filecsv = file_out
  )
  cat(paste0(nrow(df), "\n"))

  # Save the data to a CSV file
  write.csv(df, file_out)
}
