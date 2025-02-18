library(dplyr)
library(sf)
library(terra)

source("function_rasterise_metomilo.R")

# Set locale
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Get user name
user <- Sys.getenv("USERNAME")

folder_base <- "C:/Users/FEG/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/"
folder_area <- paste0(folder_base, "Focus areas/grid_v3/raster/")
folder_output_csv <- paste0(folder_base, "../Analyses/input_data/ecosystem_components/")
basefolder_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/"


# read data - Only hornkoraller tilgjengelig i Hardanger
shp_octocoral_all <- paste0(basefolder_od, 
               "/GIS Data/Ecological components data/17. Horn corals/17.1 Hornkoraller_FGDB.gdb")

# Define the path to the gdb file
file_path_gdb_smb <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/19. Sårbare marine biotoper/19.2 SarbareMarineBunndyrObs_FGDB/SarbareMarineBunndyrObs_FGDB.gdb"
#read gdb file DN-19 Salten - PRESENT IN SALTEN 
#valgte å plotte i raster kun 17 hornkoraller fordi de overlapper med 19, men 17 er på artsnivå.

# study area raster files
# "5109_02_03.tif" - combined Sunnhordaland & Hardanger            
# "5109_02_03_area.tif" - indicates if a grid cell is 02 or 03     
# "Hardanger_5109_03.tif" 
# "Jæren_5104_01.tif"         
# "Nord-Salten_1108_08.tif"   
# "Sunnhordaland_5109_02.tif"

# Load the 100m study area raster
r_area <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/Nord-Salten_1108_08.tif"))

#load octocoral data - Only hornkoraller tilgjengelig i Hardanger
db_layers <- sf::st_layers(file_Hornkorall)$name
shp_octocoral_all <- purrr::map(
  db_layers, sf::st_read, dsn = file_Hornkorall, quiet = TRUE
) %>% bind_rows()

# List of species to rasterize
species_list <- shp_octocoral_all %>%
  filter(art %in% c("Isidella lofotensis",
                    "Radicipes gracilis")) %>%
  distinct(art) %>%
  pull(art) %>%
  as.character()

for (i in seq_along(species_list)) {
  # Select current species
  species <- species_list[i]
  cat(paste0(species, ": "))

  # Filter shape for selected species
  shp <- shp_octocoral_all %>% filter(art == species)

  # Define output CSV file name
  file_out <- paste0(folder_output_csv, "14.2 ", species, "_Salten", ".csv")

  # Rasterize and save CSV
  df <- rasterise_mm(r_area, shp, variable = "art", return_df = TRUE, filecsv = file_out)
  cat(paste0(nrow(df), "\n"))

  # Save the data to a CSV file
  write.csv(df, file_out)
}
