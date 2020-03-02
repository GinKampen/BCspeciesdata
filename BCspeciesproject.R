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

### read ecosection spatial data
ecosection_map <- sf::st_read("data/ERC_ECOSECTIONS_SP/ERC_ECOSEC_polygon.shp")

### get codes for one specific species
abies <- ecosections$Ecosection[ecosections$ScientificName == "Abies grandis / Berberis nervosa"]

### get spatial data for one specific species - ecosection distribution
abies_spatial <- ecosection_map[ecosection_map$ECOSEC_CD %in% abies,]

### NEXT STEP - how do we generalise this? 
### make a function where the user can provide a scientific name
### and the function returns spatial data ecosections for taht species 



