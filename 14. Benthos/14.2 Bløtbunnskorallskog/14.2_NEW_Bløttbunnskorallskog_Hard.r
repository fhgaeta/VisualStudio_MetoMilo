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
file_Hornkorall <- paste0(basefolder_od, 
               "/GIS Data/Ecological components data/17. Horn corals/17.1 Hornkoraller_FGDB.gdb")

# Check if file exists
if (!file.exists(file_Hornkorall)) {
  stop("File does not exist: ", File_Hornkorall)
}

# study area raster files
# "5109_02_03.tif" - combined Sunnhordaland & Hardanger            
# "5109_02_03_area.tif" - indicates if a grid cell is 02 or 03     
# "Hardanger_5109_03.tif" 
# "Jæren_5104_01.tif"         
# "Nord-Salten_1108_08.tif"   
# "Sunnhordaland_5109_02.tif"

# Load the 100m study area raster
r_area <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/5109_02_03.tif"))

#load octocoral data - Only hornkoraller tilgjengelig i Hardanger
db_layers <- sf::st_layers(file_Hornkorall)$name
 
layer <- db_layers[1]
 
shp <- sf::st_read(layer, dsn=file_Hornkorall, quiet=T)
 
 
shp %>%
  distinct(art) 

list_select_species <- c("Isidella lofotensis",
                    "Radicipes")
 
shp <- shp %>%
filter(art %in% list_select_species)


for (i in 1:length(list_select_species)) {
  # Select current species
  species <- list_select_species[i]
  cat(paste0(species, ": "))
  # Filter shape for selected species
  shpi <- shp %>% filter(art == species)
  # Define output CSV file name
  file_out <- paste0(folder_output_csv, "14.2 ", species, "_Hardanger", ".csv")
  # Rasterize and save CSV
  df <- rasterise_mm(r_area, shpi, variable = "art", return_df = TRUE, filecsv = file_out)
  cat(paste0(nrow(df), "\n"))
  # Save the data to a CSV file
  write.csv(df, file_out)
}
 