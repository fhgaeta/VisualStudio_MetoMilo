#plot single area 3 - Nord Salten

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(leaflet)
library(terra)
install.packages("rgdal")
library(rgdal)


#Define folder

Sys.setlocale("LC_ALL", "en_US.UTF-8")
#user <- Sys.getenv("USERNAME")

basefolder_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/"
basefolder_teams <- "C:/Users/FEG/NIVA/METOMILO - Prosjektgruppe - METOMILO - Prosjektgruppe - METOMILO/AP1 Kartlegge samlet påvirkning av menneskelige aktiviteter/Data collection/"
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output"

  # Load the saved .rds files
  shp_1108 <- readRDS(paste0(folder_output_od, "shp_1108_1000.rds"))
  shp_water <- readRDS(paste0(folder_output_od, "shp_water.rds"))
  rw_shp <- readRDS(paste0(folder_output_od, "rw_shp.rds"))

# read bird data 

file <- paste0(basefolder_teams,"GIS Data/Ecological components data/10. Seabirds/10.2_FGDB.gdb")

db_layers <- sf::st_layers(file) 
db_layers <- db_layers$name

shp_birds_all <- purrr::map(db_layers, sf::st_read, dsn=file, quiet=T) %>%
  bind_rows()

shp_birds <- shp_birds_all %>%
  sf::st_transform(crs=crs_proj)

# intersection of birds and area polygons will 
# givr us points within the study areas

shp_birds <- shp_birds %>%
  sf::st_intersection(shp)

# make the bird names a factor. this is a minor detail.
# it will allow us to keep the same colours for different species
# across regions / study areas
bird_species_EN <- unique(shp_birds$speciesenglishname) %>% sort()
bird_species_NO <- unique(shp_birds$speciesnorwegianname) %>% sort()
shp_birds$speciesenglishname <- factor(shp_birds$speciesenglishname,
                                       levels=bird_species_EN)
shp_birds$speciesnorwegianname <- factor(shp_birds$speciesnorwegianname,
                                         levels=bird_species_NO)



# select subset of data for a single study area

shp_area_3 <- shp %>%
  filter(vannregion=="1108")

shp_birds_1 <- shp_birds %>%
  filter(vannregion=="1108")

# select only specific columns for the bird data
shp_birds_1 <- shp_birds_1 %>%
  select(speciesenglishname, speciesnorwegianname)

# get the region
plot_title <- paste0("Vannregion: ", shp_area_3$vannregion[1])

#plot the results
p <- ggplot() +
  geom_sf(data=shp_1108, fill="lightblue", colour=NA, alpha=0.8) +
  geom_sf(data=shp_area_3, fill=NA, colour="grey", alpha=0.2, linewidth=0.4) +
  geom_sf(data=shp_birds_1, aes(colour=speciesnorwegianname)) +
  coord_sf() +
  scale_colour_discrete(drop=F, name="art") +
  guides(colour = guide_legend(ncol = 1, bycol = T)) +
  theme_minimal() +
  labs(subtitle=plot_title)

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/10.Seabirds/"

# Save the vectorial plot
ggsave(p, filename=paste0(folder_output_od, "Seabirds_Salten_vectorial", shp_area_3$vannregion[1], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")

#Transform the bird data to raster format
#We can use the rasterize function from the raster package to transform the bird data to raster format.
#The function will count the number of bird observations in each raster cell.
# Rasterize the vector data using the unmasked raster as a template
rasterized_birds <- terra::rasterize(shp_birds_1, r, field="speciesnorwegianname", fun="count")

# Convert the rasterized data to polygons for visualization
rasterized_birds_shp <- terra::as.polygons(rasterized_birds, aggregate=F) %>%
  sf::st_as_sf()

# Ensure the speciesnorwegianname field is treated as a factor
rasterized_birds_shp$speciesnorwegianname <- as.factor(rasterized_birds_shp$speciesnorwegianname)

# Plot the rasterized bird data
p3 <- ggplot() +
  geom_sf(data=shp_water, colour=NA, fill="lightblue", alpha=0.5) + 
  geom_sf(data=rasterized_birds_shp, aes(fill=speciesnorwegianname), colour=NA, alpha=0.8) + 
  geom_sf(data=shp_1108, colour="red", fill=NA) + 
  theme_minimal() + 
  coord_sf(xlim=c(x0,x1), ylim=c(y0,y1), datum=25833) +
  scale_fill_discrete(name="Bird Species")

# Define the folder path
folder_output_od <- "C:/Users/FEG/OneDrive - NIVA/METOMILO_OneDrive/Output/10.Seabirds/"

# Save the raster plot
ggsave(p3, filename=paste0(folder_output_od, "Seabirds_Salten_raster_1000", shp_area_3$vannregion[1], ".png"),
       dpi=300, height=20, width=20, units="cm", bg="white")



