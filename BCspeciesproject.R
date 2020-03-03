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

library(rmapshaper)

ecosection_simple <- ms_simplify(ecosection_map)

### get spatial data for one specific species - ecosection distribution
abies_spatial <- ecosection_simple[ecosection_simple$ECOSEC_CD %in% abies,]

### alternatively add a column to ecosection data saing whether one species is present or not
abies_present <- ecosection_simple
abies_present$Present <- if_else(abies_present$ECOSEC_CD %in% abies, TRUE, FALSE)

### colour by presence//absence
library(ggplot2)

gp <- ggplot() +
  geom_sf(data = abies_present, aes(fill = Present), size = 0.1) +
  scale_fill_manual(values = c("white", "red"))

print(gp)
### NEXT STEP - how do we generalise this? 
### make a function where the user can provide a scientific name
### and the function returns spatial data ecosections for taht species 



