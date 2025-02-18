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
r_area <- terra::rast(paste0(folder_area, "Nord-Salten_1108_08.tif"))

# Read data layers
base_path <- paste0("C:/Users/", user, "/NIVA/METOMI~1/AP1KAR~1/DATACO~1/GISDAT~1/ECOLOG~1/12. Fisk/12.1 Spawning grounds/kysttorsk/")
shp_kysttorsk <- read_sf(paste0(base_path, "../kysttorsk/kysttorsk.shp"))
lyr_path <- paste0(base_path, "../Gytefelt_lyr/Gytefelt_lyr.shp")
shp_lyr_Hard <- read_sf(paste0(base_path, "../Gytefelt_lyr/Gytefelt_lyr.shp"))
hvitting_path <- paste0(base_path, "../Gytefelt_hvitting/Gytefelt_hvitting.shp")
shp_hard_hvitting <- read_sf(paste0(base_path, "../Gytefelt_hvitting/Gytefelt_hvitting.shp"))
hyse_path <- paste0(base_path, "../Gytefelt_hyse/Gytefelt_hyse.shp")
shp_hard_hyse <- read_sf(paste0(base_path, "../Gytefelt_hyse/Gytefelt_hyse.shp"))
sei_path <- paste0(base_path, "../Gytefelt_sei/Gytefelt_sei.shp")
shp_hard_sei <- read_sf(paste0(base_path, "../Gytefelt_sei/Gytefelt_sei.shp"))


# Loop through layers in the DB for kysttorsk
for (i in seq_along(shp_kysttorsk$species_no)) {
    # Select current layer
    layer <- as.character(shp_kysttorsk$species_no[i])
    cat(paste0(layer, ": "))

    # Load shape for selected layer
    shp <- sf::st_read(dsn = dirname(base_path), layer = tools::file_path_sans_ext(basename(base_path)), quiet = TRUE)

    # Define output CSV file name
    file_out <- paste0(local_folder, "12.1_", layer, "_Salten", ".csv")

    # Rasterise and save CSV
    df <- rasterise_mm(
        r_area, shp,
        variable = "species_no",
        return_df = TRUE,
        filecsv = file_out
    )
    n <- nrow(df)
    cat(paste0(n, "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)
}

# Loop through layers in the DB for lyr
for (i in seq_along(shp_lyr_Hard$species_no)) {
    # Select current layer
    layer <- as.character(shp_lyr_Hard$species_no[i])
    cat(paste0(layer, ": "))

    # Load shape for selected layer
    shp <- sf::st_read(dsn = dirname(lyr_path), layer = tools::file_path_sans_ext(basename(lyr_path)), quiet = TRUE)

    # Define output CSV file name
    file_out <- paste0(local_folder, "12.1_", layer, "_Salten", ".csv")

    # Rasterise and save CSV
    df <- rasterise_mm(
        r_area, shp,
        variable = "species_no",
        return_df = TRUE,
        filecsv = file_out
    )
    n <- nrow(df)
    cat(paste0(n, "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)
}

# Loop through layers in the DB for hvitting
for (i in seq_along(shp_hard_hvitting$species_no)) 
{
    # Select current layer
    layer <- as.character(shp_hard_hvitting$species_no[i])
    cat(paste0(layer, ": "))

    # Load shape for selected layer
    shp <- sf::st_read(dsn = dirname(hvitting_path), layer = tools::file_path_sans_ext(basename(hvitting_path)), quiet = TRUE)

    # Define output CSV file name
    file_out <- paste0(local_folder, "12.1_", layer, "_Salten", ".csv")

    # Rasterise and save CSV
    df <- rasterise_mm(
        r_area, shp,
        variable = "species_no",
        return_df = TRUE,
        filecsv = file_out
    )
    n <- nrow(df)
    cat(paste0(n, "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)
}

# Loop through layers in the DB for hyse
for (i in seq_along(shp_hard_hyse$species_no)) {
    # Select current layer
    layer <- as.character(shp_hard_hyse$species_no[i])
    cat(paste0(layer, ": "))

    # Load shape for selected layer
    shp <- sf::st_read(dsn = dirname(hyse_path), layer = tools::file_path_sans_ext(basename(hyse_path)), quiet = TRUE)

    # Define output CSV file name
    file_out <- paste0(local_folder, "12.1_", layer, "_Salten", ".csv")

    # Rasterise and save CSV
    df <- rasterise_mm(
        r_area, shp,
        variable = "species_no",
        return_df = TRUE,
        filecsv = file_out
    )
    n <- nrow(df)
    cat(paste0(n, "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)
}

# Loop through layers in the DB for sei
for (i in seq_along(shp_hard_sei$species_no)) {
    # Select current layer
    layer <- as.character(shp_hard_sei$species_no[i])
    cat(paste0(layer, ": "))

    # Load shape for selected layer
    shp <- sf::st_read(dsn = dirname(sei_path), layer = tools::file_path_sans_ext(basename(sei_path)), quiet = TRUE)

    # Define output CSV file name
    file_out <- paste0(local_folder, "12.1_", layer, "_Salten", ".csv")

    # Rasterise and save CSV
    df <- rasterise_mm(
        r_area, shp,
        variable = "species_no",
        return_df = TRUE,
        filecsv = file_out
    )
    n <- nrow(df)
    cat(paste0(n, "\n"))

    # Save the data to a CSV file
    write.csv(df, file_out)
}
