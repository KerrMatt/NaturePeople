# Information ----
## Author: M R Kerr (matthew.kerr@bio.au.dk)
## Last edit: 5th September 2024
## SUmmary: Analysing data for rural people, nature dependent people, and novelty interactions with them both.

rm(list = ls()); gc()

# Library load ----

## Housekeeping

library(tidyverse)
library(data.table)
library(readxl)
library(stringr)

## Data
library(geodata)

## Spatial

library(terra)
library(tidyterra)
library(sf)

sf_use_s2(F)

## Parallel

library(foreach)
library(doParallel)

## Plotting

library(scico)
library(biscale)
library(patchwork)
library(waffle)

# Data load ----

## Rural population ----
hyde.rural <- rast("Data/Rural Population/base/rurc_2023AD.asc")

hyde.rural.upper <- rast("Data/Rural Population/upper/rurc_2023AD.asc")
hyde.rural.lower <- rast("Data/Rural Population/lower/rurc_2023AD.asc")

hyde.total <- rast("Data/Rural Population/popc_2023AD.asc")
hyde.ruralp <- hyde.rural[]/hyde.total[]

hyde.rural.proportion <- hyde.rural
hyde.rural.proportion[] <- hyde.ruralp

plot(hyde.rural.proportion)

## Human modification index ----
hmi <- rast("Data/Human Modification/lulc-human-modification-terrestrial-systems_geographic.tif") %>%
  resample(., hyde.rural.proportion)

## Last of the wild
wild <- rast("Data/WIld Areas/wildareas-v3-2009-human-footprint.tif") %>%
  project(., hmi) %>%
  resample(., hyde.rural)

wildgeo <- read_sf("Data/WIld Areas/ltw-global-geo/ltw_v2geo.shp") %>%
  st_union()

## Novelty layers ----
total.novelty <- rast("Data/Novelty Layers/NOVELTY_TOTAL.tif") %>% 
  project(., hyde.rural, method = "average")

climate.novelty <- rast("Data/Novelty Layers/NOVELTY_CLIMATE.tif") %>% 
  project(., hyde.rural, method = "average")
defaunation.novelty <- rast("Data/Novelty Layers/NOVELTY_DEFAUNATION.tif") %>% 
  project(., hyde.rural, method = "average")
disruption.novelty <- rast("Data/Novelty Layers/NOVELTY_DISRUPTION.tif") %>% 
  project(., hyde.rural, method = "average")

## Nature Dependent People (NDP) ----
ndp <- sf::read_sf("Data/NDP/NDPI_subnational_all_dimensions.shp")

ndp.extraction <- extract(total.novelty, ndp) %>%
  group_by(ID) %>%
  summarize(mean_novelty = mean(Total, na.rm = T))

ndp$MeanNovelty <- ndp.extraction$mean_novelty

ndp.extraction <- extract(climate.novelty, ndp) %>%
  group_by(ID) %>%
  summarize(mean_novelty = mean(Climate, na.rm = T))

ndp$ClimateNovelty <- ndp.extraction$mean_novelty

## Country Data ----
# Download for the first time:
# country_list <- data.frame(NDPname = str_to_upper(unique(ndp.table$Country))) %>%
#   merge(., y = data.frame(NDPname = country_codes()[,4], ISO3 = country_codes()[,2]), all.x = T)

subregion.sf <- geodata::gadm(country = country_codes()[,1], level = 0, path = "Data/GADM/gadm/", version = "latest", resolution = 1)
