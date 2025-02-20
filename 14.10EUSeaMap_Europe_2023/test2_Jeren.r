# Load necessary libraries
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

# Define the path to the shapefiles
file_path_gdb_jeren <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/EUSeaMap_Europe_2023/EUSeaMap_2023_Jeren/EUSeaMap_2023_Jeren.shp"
file_path_gdb_sar_hab <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/GIS Data/Ecological components data/21. Marine grunnkart - Sårbare habitater/SaltvannssjobunntyperPredikert_FGDB.gdb"

# Load the 100m study area raster
r_area <- terra::rast(paste0(basefolder_od, "/Focus areas/grid_mask/Raster/Jæren_5104_01.tif"))

# Load EUSeaMap_2023
db_layers <- sf::st_layers(file_path_gdb_jeren)$name
shp_sea_map_23 <- purrr::map(
  db_layers, sf::st_read, dsn = file_path_gdb_jeren, quiet = TRUE
) %>% 
  bind_rows() %>% 
  sf::st_make_valid()

# Load 21 Marine grunnkart - Saltvannssjobunntyper Predikert
db_layers <- sf::st_layers(file_path_gdb_sar_hab)$name
shp_sar_hab <- purrr::map(
  db_layers, sf::st_read, dsn = file_path_gdb_sar_hab, quiet = TRUE
) %>% 
  bind_rows()

# List all categories present in the column "All2019DL2"
categories <- shp_sea_map_23 %>%
  distinct(All2019DL2) %>%
  pull(All2019DL2) %>%
  as.character()

cat("Categories in 'All2019DL2':\n")
cat(paste(categories, collapse = "\n"))

# List all categories present in the column "ninbeskrivelse"
categories2 <- shp_sar_hab %>% 
  distinct(ninbeskrivelse) %>% 
  pull(ninbeskrivelse) %>% 
  as.character()

cat("Categories in 'ninbeskrivelse':\n")
cat(paste(categories2, collapse = "\n"))

# Define the categories to merge
categories_to_merge <- list(
  list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk litt og temmelig finmaterialrik bunn med temmelig liten til intermediær erosjonsmotstand."),
  list("MB1: Infralittoral rock", "Eufotisk fast saltvannsbunn."),
  list("MB4: Infralittoral mixed sediment", "Eufotisk marin sedimentbunn - brakk eufotisk uten finmateriale og finmaterialfattig bunn med liten erosjonsmotstand."),
  list("MB5: Infralittoral sand", "Eufotisk marin sedimentbunn - brakk eufotisk og temmelig finmaterialrik bunn med liten erosjonsmotstand."),
  list("Circalittoral seabed", "Afotisk marin sedimentbunn."),
  list("Circalittoral rock", "Afotisk fast saltvannsbunn."),
  list("Afotisk fast saltvannsbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.", "Marin undervannseng.", "Afotisk marin sedimentbunn.", "Afotisk marin sedimentbunn - litt og temmelig finmaterialrik afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i atlantisk vann.", "Afotisk marin sedimentbunn - finmaterialdominert afotisk bunn med temmelig liten til temmelig stor erosjonsmotstand i øvre sublitoral.")
)

# Function to sanitize file names
sanitize_filename <- function(name) {
  gsub("[^[:alnum:]_]", "_", name)
}

# Loop through the categories and merge
for (categories in categories_to_merge) {
  cat1 <- categories[[1]]
  cat2 <- categories[[2]]
  
  infralittoral_rock <- shp_sea_map_23 %>% filter(All2019DL2 == cat1)
  eufotisk_saltvannsbunn <- shp_sar_hab %>% filter(ninbeskrivelse == cat2)
  
  # Merge the spatial positions
  merged <- st_union(infralittoral_rock, eufotisk_saltvannsbunn)
  
  # If there are more than two categories, merge them as well
  if (length(categories) > 2) {
    for (i in 3:length(categories)) {
      additional_category <- shp_sar_hab %>% filter(ninbeskrivelse == categories[[i]])
      merged <- st_union(merged, additional_category)
    }
  }
  
  # Ensure the merged object has a geometry column
  st_geometry(merged) <- "geometry"
  
  # Define output CSV file name
  file_out <- paste0(local_folder, "14.10_", sanitize_filename(cat1), "_", sanitize_filename(cat2), "_Jæren.csv")
  
  # Rasterize and save CSV
  df <- rasterise_mm(r_area, merged, variable = "All2019DL2", return_df = TRUE, filecsv = file_out)
  cat(paste0(nrow(df), "\n"))
  
  # Save the data to a CSV file
  write.csv(df, file_out)
  
  cat("Merged CSV file saved to: ", file_out, "\n")
}
