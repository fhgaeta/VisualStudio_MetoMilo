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

# Read data
file_path_gdb_cor_gard <- paste0(
  "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/",
  "Ecological components data/21. Marine grunnkart - Sårbare habitater/",
  "SarbareMarineHabitater_FGDB.gdb"
)

file_path_test <- paste0("C:/Users/FEG/Downloads/SarbareMarineHabitater_FGDB.gdb")

# Convert encoding
file_path_gdb_cor_gard <- iconv(file_path_test, from = "latin1", to = "UTF-8")

# Check if file exists
if (!file.exists(file_path_gdb_cor_gard)) {
  stop("File does not exist: ", file_path_gdb_cor_gard)
}

# Read gdb file Coral garden - hardbottom - ONLY ONE PRESENT IN jeren
gdb_coral_garden <- read_sf(file_path_gdb_cor_gard,)

# Load the 100m study area raster
r_area <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/Jæren_5104_01.tif"))

# Read coral garden data
db_layers <- sf::st_layers(file_path_test)$name
gdb_coral_garden <- purrr::map(
  db_layers, sf::st_read, dsn = file_path_test, quiet = TRUE
) %>% bind_rows()

# List of species to rasterize
species_list <- gdb_coral_garden %>%
  filter(sarbarthabitat %in% c(
                               "Hydrozoa hage",
                               "Cerianthidebunn", "Korallskog", "Korallskog/Reirskjell"
                               )) %>%
  distinct(sarbarthabitat) %>%
  pull(sarbarthabitat) %>%
  as.character()

for (i in seq_along(species_list)) {
  species <- species_list[i]
  cat(paste0(species, ": "))
  
  shp <- gdb_coral_garden %>% filter(sarbarthabitat == species)

  # Define output csv file name
  file_out <- paste0(local_folder, "14.2_", species, "_Jeren", ".csv")
  
  df <- rasterise_mm(
    r_area, shp, variable = "sarbarthabitat", return_df = TRUE, filecsv = file_out
  )
  cat(paste0(nrow(df), "\n"))
  
  write.csv(df, file_out)
}
