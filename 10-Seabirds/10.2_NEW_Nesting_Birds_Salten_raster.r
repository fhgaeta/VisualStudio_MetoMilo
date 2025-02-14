library(dplyr)
library(sf)
library(terra)

source("function_rasterise_metomilo.R")


# get user name - the local path for sharepoint folders 
# are identical except for this part
user <- Sys.getenv("USERNAME")

folder_base <- "C:/Users/FEG/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/"

folder_area <- paste0(folder_base,"Focus areas/grid_v3/raster/")
folder_output_csv <- paste0(folder_base, "../Analyses/input_data/ecosystem_components/")
db_path <- paste0(folder_base,"GIS Data/Ecological components data/10. Seabirds/10.2_FGDB.gdb")

# study area raster files
# "5109_02_03.tif" - combined Sunnhordaland & Hardanger            
# "5109_02_03_area.tif" - indicates if a grid cell is 02 or 03     
# "Hardanger_5109_03.tif" 
# "Jæren_5104_01.tif"         
# "Nord-Salten_1108_08.tif"   
# "Sunnhordaland_5109_02.tif"

# --------- load the 100m study area raster ----
r_area <- terra::rast(paste0(folder_area, "Nord-Salten_1108_08.tif"))


# ---------- read data layers ----------------

# get names of layers in DB

db_layers <- sf::st_layers(db_path) 
db_layers <- db_layers$name

# read individual layers...

# - (1) --------------------------
# to process only one layer, then set i to the index
# of the layer you want and just run the 
# code inside the for loop

i <- 2

# select current layer
layer <- db_layers[i]
cat(paste0(layer,"\n"))

# load shape for selected layer
shp <- sf::st_read(layer, dsn=db_path, quiet=T)

df <-  rasterise_mm(r_area, shp, variable = "estimatedvalue")

file_out <- paste0(folder_output_csv, layer, ".csv")

rasterise_mm(r_area, shp, variable = "estimatedvalue", 
             return_df = FALSE, filecsv = file_out)

# save the data to a csv file
# define the path to the output folder

write.csv(df, file_out)


# - (2) --------------------------
# if you want to run this for ALL the layers in the DB
# you can just run the whole for{} loop here


for (i in seq_along(db_layers)) {

  # select current layer
  layer <- db_layers[i]
  cat(paste0(layer, ": "))

  # load shape for selected layer
  shp <- sf::st_read(layer, dsn = db_path, quiet = TRUE)

  # define output csv file name
  file_out <- paste0(folder_output_csv, layer, "_Salten", ".csv")

  # rasterise and save csv
  df <- rasterise_mm(r_area, shp, variable = "estimatedvalue",
                     return_df = TRUE, filecsv = file_out)
  n <- nrow(df)
  cat(paste0(n, "\n"))

  # save the data to a csv file
  write.csv(df, file_out)

}
