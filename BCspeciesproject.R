library(readr)
library(dplyr)
library(sf)
library(mapview)
library(stringr)

species_bc <- readr::read_tsv(file = "data/bcsee_export.tsv")

names(species_bc) <- gsub(" ", "", names(species_bc))
ecosections <- dplyr::select(species_bc, ScientificName, Ecosection)

ecosections <- do.call("rbind", lapply(1:nrow(ecosections), function(x){
  data <- ecosections[x,]
  splits <- strsplit(data$Ecosection, ";")[[1]]
  data <- data.frame(ScientificName = rep(data$ScientificName, length(splits)),
                     Ecosection = splits)
}))


ecosection_map <- sf::st_read("data/ERC_ECOSECTIONS_SP/ERC_ECOSEC_polygon.shp")

codes <- as.character(ecosections$Ecosection[as.character(ecosections$ScientificName) == "Abies grandis / Berberis nervosa"])
ecosection_map <- ecosection_map[as.character(ecosection_map$ECOSEC_CD) %in% codes,]
species_bc$new_column <- "Species with English Names"
if_else(species_bc$`English Name`, == "NA", FALSE)


temp2 <- data.frame(strsplit(temp$Ecosection))

ecosections <- do.call("rbind" , lapply(1:nrow(ecosections), function(x){
  data <- ecosections[x,]
  splits <- strsplit(data$Ecosection,";")[[1]]
  data <- data.frame(ScientificName = rep(data$ScientificName, length(splits)),
                     Ecosection = splits)
}))

ecosections$ScientificName[which(ecosections$Ecosection == "CPR")]
temp <- ecosections$Ecosection[1]
strsplit(temp,";")

species_bc$COSEWIC <- str_replace(species_bc$COSEWIC, "NA", "No Status")




