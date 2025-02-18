library(dplyr)
library(sf)
library(terra)

source("function_rasterise_metomilo.R")

# Get user name - the local path for SharePoint folders 
# are identical except for this part
user <- Sys.getenv("USERNAME")
# Define UTF
Sys.setlocale("LC_ALL", "en_US.UTF-8")

folder_base <- "C:/Users/FEG/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/"
local_folder <- paste0("C:/Users/FEG/Downloads/TEST_Metomilo_Local/")
folder_area <- paste0(folder_base, "Focus areas/grid_v3/raster/")
folder_output_csv <- paste0(folder_base, "../Analyses/input_data/ecosystem_components/")

# Study area raster files
# "5109_02_03.tif" - combined Sunnhordaland & Hardanger            
# "5109_02_03_area.tif" - indicates if a grid cell is 02 or 03     
# "Hardanger_5109_03.tif" 
# "Jæren_5104_01.tif"         
# "Nord-Salten_1108_08.tif"   
# "Sunnhordaland_5109_02.tif"

# Load the 100m study area raster
r_area <- terra::rast(paste0(folder_area, "Jæren_5104_01.tif"))

# Load paths for the shapefiles

base_path <- paste0("C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/12. Fisk/12.2 Nursery & feeding areas/12.2 Fiskeridir_Oppvekst beiteområd/Fiskeridir_Oppvekst beiteområd.shp")
# Load and read the shapefile as sf
shp_beite_Hard <- read_sf(base_path)



# Loop through layers in the DB for layer shp_beite_Hard 
for (i in seq_along(shp_beite_Hard$art_norsk1)) {
    # Select current layer
    layer <- as.character(shp_beite_Hard$art_norsk1[i])
    cat(paste0(layer, ": "))

    # Load shape for selected layer
    shp <- sf::st_read(dsn = dirname(base_path), layer = tools::file_path_sans_ext(basename(base_path)), quiet = TRUE)

    # Define output CSV file name
     file_out <- paste0(local_folder, "12.2_", layer, "_Jeren", ".csv")

    # Rasterise and save CSV
    df <- rasterise_mm(
        r_area, shp,
        variable = "art_norsk1",
        return_df = TRUE,
        filecsv = file_out
    )
    n <- nrow(df)
    cat(paste0(n, "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)
}
