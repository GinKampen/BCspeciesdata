library(readr)
library(dplyr)
library(sf)
library(mapview)
library(stringr)
library(rmapshaper)
library(ggplot2)
library(tidyverse)

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

### Loading maps used in analysis

ecosection_map <- sf::st_read("data/ERC_ECOSECTIONS_SP/ERC_ECOSEC_polygon.shp")
ecosection_map2 <- bcmaps::ecosections()
bc_boundary <- bcmaps::bc_bound()

ecosection_simple <- ms_simplify(ecosection_map)

### Dropping columns
species_bc$ScientificNameSynonyms <- NULL
species_bc$EnglishNameSynonyms <- NULL
species_bc$GlobalStatusReviewDate <- NULL
species_bc$ProvStatusChangeDate <- NULL
species_bc$ProvStatusReviewDate <- NULL
species_bc$COSEWICComments <- NULL
species_bc$ProvincialFRPA <- NULL
species_bc$GOERT <- NULL
species_bc$MBCA <- NULL
species_bc$SARAComments <- NULL
species_bc$BreedingBird <- NULL
species_bc$MappingStatus <- NULL
species_bc$X46 <- NULL
species_bc$CDCMaps <- NULL

### species distribution of ecosections function 
species_map <- function(species) {
  species_ecosections <- ecosections$Ecosection[ecosections$ScientificName == species]
  species_ecosections_spatial <- ecosection_simple[ecosection_simple$ECOSEC_CD %in% species_ecosections,]
  species_present <- ecosection_simple
  species_present$Present <- if_else(species_present$ECOSEC_CD %in% species_ecosections, TRUE, FALSE)
  gp <- ggplot() +
    geom_sf(data = bc_boundary, size = 0.2) +
    geom_sf(data = species_present, aes(fill = Present), size = 0.05) +
    scale_fill_manual(values = c("transparent", "red"),
                      name = " ",
                      labels = c("Species Absent", "Species Present")) +
    labs(title = "Species Distribution by Ecosection",
         subtitle = species)
                      return(print(gp))
}

### conservation status (red/blue list) function
x <- species_bc$COSEWIC

str_replace(x, "E", "Endangered")
str_replace(x, "T", "Threatened")
str_replace(x, "SC", "Special Concern")
str_replace(x, "XT", "Extirpated")
str_replace(x, "X", "Extinct")
str_replace(x,"NAR", "Not at Risk")
str_replace(x, "NA", "No Status")

species_bc <- gsub("\\()", " ", x)
fixed_dates <- parse_datetime(COSEWIC, "m y")
species_bc_2 <- separate(species_bc, COSEWIC, c("COSEWIC Status", "Implemented Date" ))


conservation_status <- function(species1) {
  species_conservation <- species_bc_2[species_bc$ScientificName == species1,]
  species_conservation_columns <- x[c("BCList", "COSEWIC Status", "Implemented Date")]
  return(print(species_conservation))
}

conservation_status("Anemone occidentalis - Carex nigricans")
species_map("Anemone occidentalis - Carex nigricans")





