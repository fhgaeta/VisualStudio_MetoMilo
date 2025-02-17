library(dplyr)
library(sf)
library(terra)

source("function_rasterise_metomilo.R")


# get user name - the local path for sharepoint folders 
# are identical except for this part
user <- Sys.getenv("USERNAME")
Sys.setlocale("LC_ALL", "en_US.UTF-8")


folder_base <- paste0("C:/Users/", user, "/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe -",
                      "METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/",
                      "Data collection/")

folder_area <- paste0(folder_base,"Focus areas/grid_v3/raster/")
folder_output_csv <- paste0(folder_base, "../Analyses/input_data/ecosystem_components/")
basefolder_od <- paste0("C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/")
# read data 
db_path <- paste0(basefolder_od, 
               "/GIS Data/Ecological components data/17. Horn corals/17.1 Hornkoraller_FGDB.gdb")
file_path_gdb_smb <- paste0("C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/",
                            "Ecological components data/19. Sårbare marine biotoper/",
                            "19.2 SarbareMarineBunndyrObs_FGDB.gdb")
#read gdb file DN-19 Salten - PRESENT IN SALTEN 
gdb_smh_obs_salten <- read_sf(file_path_gdb_smb)
# study area raster files
# "5109_02_03.tif" - combined Sunnhordaland & Hardanger            
# "5109_02_03_area.tif" - indicates if a grid cell is 02 or 03     
# "Hardanger_5109_03.tif" 
# "Jæren_5104_01.tif"         
# "Nord-Salten_1108_08.tif"   
# "Sunnhordaland_5109_02.tif"

# --------- load the 100m study area raster ----
r_area <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/Nord-Salten_1108_08.tif"))
#r_area <- terra::rast(paste0(folder_area, "Nord-Salten_1108_08.tif"))


# ---------- read data layers ----------------

# read octocoral data
db_layers <- sf::st_layers(db_path)$name
shp_octocoral_all <- purrr::map(
  db_layers, sf::st_read, dsn = db_path, quiet = TRUE
) %>% bind_rows()

# List of species to rasterize
species_list <- shp_octocoral_all %>%
  filter(art %in% c("Paragorgia arborea",
                    "Primnoa resedaeformis")) %>%
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
  file_out <- paste0(folder_output_csv, species, "_Salten", ".csv")

  # Rasterize and save CSV
  df <- rasterise_mm(r_area, shp, variable = "art", 
                     return_df = TRUE, filecsv = file_out)
  cat(paste0(nrow(df), "\n"))

  # Save the data to a CSV file
  write.csv(df, file_out)
}

# read sårbare marine biotoper
db_layers <- sf::st_layers(file_path_gdb_smb)$name
shp_smb_all <- purrr::map(
  db_layers, sf::st_read, dsn = file_path_gdb_smb, quiet = TRUE
) %>% bind_rows()

# List of species to rasterize
species_list <- shp_smb_all %>%
  filter(indicatorvme %in% c("Nephtheidae medow")) %>%
  distinct(indicatorvme) %>%
  pull(indicatorvme) %>%
  as.character()

# - (2) --------------------------
# if you want to run this for ALL the layers in the DB
# you can just run the whole for{} loop here

# NO VALUE FOR Nephtheidae medow
for(i in seq_along(species_list)){
  # Select current species
  species <- species_list[i]
  cat(paste0(species, ": "))

  # Filter shape for selected species
  shp <- shp_smb_all %>% filter(indicatorvme == "Nephtheidae medow")

  # Define output CSV file name
  file_out <- paste0(folder_output_csv, species, "_Salten", ".csv")

  # Rasterize and save CSV
  df <- rasterise_mm(r_area, shp, variable = "indicatorvme", 
                     return_df = TRUE, filecsv = file_out)
  cat(paste0(nrow(df), "\n"))

  # Save the data to a CSV file
  write.csv(df, file_out)
}
