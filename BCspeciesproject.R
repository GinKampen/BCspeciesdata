library(readr)
library(dplyr)
library(sf)
library(mapview)
library(stringr)

species_bc <- readr::read_tsv(file = "data/bcsee_export.tsv")

names(species_bc) <- gsub(" ", "", names(species_bc))
ecosections <- dplyr::select(species_bc, ScientificName, Ecosection)

### this gets ecosections for each ScientificName
ecosections <- do.call("rbind", lapply(1:nrow(ecosections), function(x){
  data <- ecosections[x,]
  splits <- strsplit(data$Ecosection, ";")[[1]]
  data <- data.frame(ScientificName = rep(data$ScientificName, length(splits)),
                     Ecosection = splits, stringsAsFactors = FALSE)
}))


ecosection_map <- sf::st_read("data/ERC_ECOSECTIONS_SP/ERC_ECOSEC_polygon.shp")

### get codes for one specific species - changed to include any species as input
species <- "Alnus rubra / Carex obnupta [ Populus trichocarpa ]"
species_ecosections <- ecosections$Ecosection[ecosections$ScientificName == species]


library(rmapshaper)

ecosection_simple <- ms_simplify(ecosection_map)

### get spatial data for one specific species - ecosection distribution - changed to include all species
species_ecosections_spatial <- ecosection_simple[ecosection_simple$ECOSEC_CD %in% species_ecosections,]

### alternatively add a column to ecosection data saying whether one species is present or not - changed for all species
species_present <- ecosection_simple
species_present$Present <- if_else(species_present$ECOSEC_CD %in% species_ecosections, TRUE, FALSE)

### colour by presence//absence
library(ggplot2)

gp <- ggplot() +
  geom_sf(data = species_present, aes(fill = Present), size = 0.1) +
  scale_fill_manual(values = c("white", "red"))

print(gp)

### NEXT STEP - how do we generalise this? 
### make a function where the user can provide a scientific name
### and the function returns spatial data ecosections for taht species 



