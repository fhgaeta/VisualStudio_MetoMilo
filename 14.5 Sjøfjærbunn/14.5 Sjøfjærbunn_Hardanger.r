#plot single area - Hardanger 14.5 Sjøfjærbunn

#19 Sårbare marine biotoper - YES
#21 Marine grunnkart -Sårbare habitater - NO

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
db_path <- paste0("C:/Users/FEG/Downloads/19.2 SarbareMarineBunndyrObs_FGDB.gdb")
# Define the path to 19 Sårbare marine biotoper
file_path_gdb_sar_mar_bio <- paste0("C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/",
                                    "GIS Data/Ecological components data/19. Sårbare marine biotoper/19.2 SarbareMarineBunndyrObs_FGDB.gdb")
#file_path_gdb_sar_hab <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/21. Marine grunnkart - Sårbare habitater/SarbareMarineHabitater_FGDB.gdb"

# study area raster files
# "5109_02_03.tif" - combined Sunnhordaland & Hardanger            
# "5109_02_03_area.tif" - indicates if a grid cell is 02 or 03     
# "Hardanger_5109_03.tif" 
# "Jæren_5104_01.tif"         
# "Nord-Salten_1108_08.tif"   
# "Sunnhordaland_5109_02.tif"

# Load the 100m study area raster
r_area <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/5109_02_03.tif"))

# Load 19 Sårbare marine biotoper - no data in Salten
db_layers <- sf::st_layers(file_path_gdb_sar_mar_bio)$name
shp_19_smb <- purrr::map(
  db_layers, sf::st_read, dsn = file_path_gdb_sar_mar_bio, quiet = TRUE
) %>% bind_rows()

# List all categories present in the column "naturtype"
categories <- shp_19_smb %>%
  distinct(objtype) %>%
  pull(objtype) %>%
  as.character()

cat("Categories in 'naturtype':\n")
cat(paste(categories, collapse = "\n"))

# List of species to rasterize
species_list <- shp_19_smb %>%
  filter(objtype %in% c("Sjøfjærbunn", "Umbellulabestander")) %>%
  distinct(objtype) %>%
  pull(objtype) %>%
  as.character()

for (i in seq_along(species_list)) {
  # Select current species
  species <- species_list[i]
  cat(paste0(species, ": "))

  # Filter shape for selected species
  shp <- shp_19_smb %>% filter(objtype == species)
  
  # Check if the filtered data frame is empty
  if (nrow(shp) == 0) {
    cat("No data found for species: ", species, "\n")
    next
  }

  # Define output CSV file name
  file_out <- paste0(local_folder, "14.5_", species, "_Hardanger", ".csv")

  # Rasterize and save CSV
  df <- rasterise_mm(
    r_area, shp, variable = "objtype", return_df = TRUE, filecsv = file_out
  )
  cat(paste0(nrow(df), "\n"))

  # Save the data to a CSV file
  write.csv(df, file_out)
}
