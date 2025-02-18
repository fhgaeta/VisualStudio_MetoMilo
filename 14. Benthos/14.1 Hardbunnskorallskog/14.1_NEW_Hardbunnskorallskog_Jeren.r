library(dplyr)
library(sf)
library(terra)

source("function_rasterise_metomilo.R")


# get user name - the local path for sharepoint folders 
# are identical except for this part
user <- Sys.getenv("USERNAME")

folder_base <- "C:/Users/FEG/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/"
local_folder <- paste0("C:/Users/FEG/Downloads/TEST_Metomilo_Local/")
folder_area <- paste0(folder_base,"Focus areas/grid_v3/raster/")
folder_output_csv <- paste0(folder_base, "../Analyses/input_data/ecosystem_components/")
basefolder_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/"
# read data - Only hornkoraller tilgjengelig i Hardanger
db_path <- paste0(basefolder_od, 
               "/GIS Data/Ecological components data/17. Horn corals/17.1 Hornkoraller_FGDB.gdb")
# read data
file_path_gdb_cor_gard <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/21. Marine grunnkart - Sårbare habitater/SarbareMarineHabitater_FGDB.gdb"
#read gdb file Coral garden - hardbottom - ONLY ONE PRESENT IN jeren
gdb_coral_garden <- read_sf(file_path_gdb_cor_gard)

# study area raster files
# "5109_02_03.tif" - combined Sunnhordaland & Hardanger            
# "5109_02_03_area.tif" - indicates if a grid cell is 02 or 03     
# "Hardanger_5109_03.tif" 
# "Jæren_5104_01.tif"         
# "Nord-Salten_1108_08.tif"   
# "Sunnhordaland_5109_02.tif"

# --------- load the 100m study area raster ----
r_area <- terra::rast(paste0(folder_area, "Jæren_5104_01.tif"))
#r_area <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/Jæren_5104_01.tif"))


# ---------- read data layers ----------------

# read octocoral data
db_layers <- sf::st_layers(db_path)$name
shp_octocoral_all <- purrr::map(
  db_layers, sf::st_read, dsn = db_path, quiet = TRUE
) %>% bind_rows()

# List of species to rasterize
species_list <- shp_octocoral_all %>%
  filter(art %in% c("Paragorgia arborea",
                   "Primnoa resedaeformis", 
                    "Paramuricea placomus")) %>%
  distinct(art) %>%
  pull(art) %>%
  as.character()

# - (2) --------------------------
# if you want to run this for ALL the layers in the DB
# you can just run the whole for{} loop here


for(i in seq_along(species_list)){
  # Select current species
  species <- species_list[i]
  cat(paste0(species, ": "))

  # Filter shape for selected species
  shp <- shp_octocoral_all %>% filter(art == species)

  # Define output CSV file name
  file_out <- paste0(local_folder, "14.1_", species, "_Jeren", ".csv")

  # Rasterize and save CSV
  df <- rasterise_mm(r_area, shp, variable = "art", 
                     return_df = TRUE, filecsv = file_out)
  cat(paste0(nrow(df), "\n"))

  # Save the data to a CSV file
  write.csv(df, file_out)
}

# read coral garden data
gdb_layers <- sf::st_layers(file_path_gdb_cor_gard)$name
shp_coral_garden_all <- purrr::map(
  gdb_layers, sf::st_read, dsn = file_path_gdb_cor_gard, quiet = TRUE
) %>% bind_rows()

# List of species to rasterize
Habitat_list_2 <- gdb_coral_garden %>%
  filter(sarbarthabitat %in% c("Korallskog",
                   "Korallskog/Reirskjell", "Reirskjell")) %>%
  distinct(sarbarthabitat) %>%
  pull(sarbarthabitat) %>%
  as.character()

for(i in seq_along(Habitat_list_2)){
  # Select current species
  Habitat <- Habitat_list_2[i]
  cat(paste0(Habitat, ": "))

  # Filter shape for selected species
  shp <- gdb_coral_garden %>% filter(sarbarthabitat == Habitat)

  # Define output CSV file name
  file_out <- paste0(local_folder,"14.1_", Habitat, "_Jeren", ".csv")

  # Rasterize and save CSV
  df <- rasterise_mm(r_area, shp, variable = "sarbarthabitat", 
                     return_df = TRUE, filecsv = file_out)
  cat(paste0(nrow(df), "\n"))

  # Save the data to a CSV file
  write.csv(df, file_out)
}
