#plot single area - Hardanger 14.3 Korallrev

#16. Naturtyper. DN-Handbok 19
#18 Extremely valuable and sensitive areas 
#19 Sårbare marine biotoper
#20. Coral reefs
#21 Marine grunnkart -Sårbare habitater 
#22. Species/habitats & areas of conservation

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

# Define the path to the GDB file  DN-19 Hard
file_path_gdb <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/16. Naturtyper. DN-Håndbok 19/16.1 Vestlandet DN nat.19/Naturtyper_hb19_EgnDef_4326_FILEGDB.gdb"

# Define the path to the SHP file- 18 Extremely valuable and sensitive areas 
file_path_gdb_ext_val <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/18. Extremely valuable and sensitive areas/svo_miljoverdier_korallrev/svo_miljoverdier_korallrev.shp"

# Define the path to 19 Sårbare marine biotoper
file_path_gdb_sar_mar_bio <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/19. Sårbare marine biotoper/19.2 SarbareMarineBunndyrObs_FGDB/SarbareMarineBunndyrObs_FGDB.gdb"

# Define the path to 20. Coral reefs
file_path_gdb_cor_reef <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/20. Korallrev/20.1 Korallrev_FGDB.gdb"

#define the path to 21 Marine grunnkart -Sårbare habitater
file_path_gdb_sar_hab <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/21. Marine grunnkart - Sårbare habitater/SarbareMarineHabitater_FGDB.gdb"
#define path to 22. Species/habitats & areas of conservation
file_path_gdb_spe_hab <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/22. Species_habitats and areas of conservation/22.6_KorallrevForbudsomrader_FGDB.gdb"

# study area raster files
# "5109_02_03.tif" - combined Sunnhordaland & Hardanger            
# "5109_02_03_area.tif" - indicates if a grid cell is 02 or 03     
# "Hardanger_5109_03.tif" 
# "Jæren_5104_01.tif"         
# "Nord-Salten_1108_08.tif"   
# "Sunnhordaland_5109_02.tif"

# Load the 100m study area raster
r_area <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/5109_02_03.tif"))

#load DN-19 
db_layers <- sf::st_layers(file_path_gdb)$name
shp_dn_19 <- purrr::map(
  db_layers, sf::st_read, dsn = file_path_gdb, quiet = TRUE
) %>% bind_rows()

# List of species to rasterize
species_list <- shp_dn_19 %>%
  filter(naturtype %in% c("korallforekomster")) %>%
  distinct(naturtype) %>%
  pull(naturtype) %>%
  as.character()

for (i in seq_along(species_list)) {
  # Select current species
  species <- species_list[i]
  cat(paste0(species, ": "))

  # Filter shape for selected species
  shp <- shp_dn_19 %>% filter(naturtype == "korallforekomster")

  # Define output CSV file name
  file_out <- paste0(folder_output_csv, "14.3_", species, "_Hardanger", ".csv")

  # Rasterize and save CSV
  df <- rasterise_mm(r_area, shp, variable = "naturtype", return_df = TRUE, filecsv = file_out)
  cat(paste0(nrow(df), "\n"))

  # Save the data to a CSV file
  write.csv(df, file_out)
}

#load 18 Extremely valuable and sensitive areas
shp_ext_val <- sf::st_read(file_path_gdb_ext_val)$name
shp_18 <- purrr::map(
  shp_ext_val, sf::st_read, dsn = file_path_gdb_ext_val, quiet = TRUE
) %>% bind_rows()

#list of species to rasterize
species_list <- shp_18 %>%
  filter(naturtype %in% c("korallrev")) %>%
  distinct(naturtype) %>%
  pull(naturtype) %>%
  as.character()
