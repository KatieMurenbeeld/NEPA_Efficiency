library(tidyverse)
library(sf)
library(readr)
library(ggplot2)
library(ggmap)
library(tigris)

## Load the data

fs_subset <- st_read("data/processed/fs_subset.shp")
fws_subset <- st_read("data/processed/fws_subset.shp")
wf_subset <- st_read("data/processed/wf_subset.shp")
cejst_subset <- st_read("data/processed/cejst_subset.shp")
elect_subset <- st_read("data/processed/elect_subset.shp")
landuse_subset <- rast("data/processed/landuse_subset.tif")

## Map 

test_map <- ggplot() +
  geom_sf(data = elect_subset, aes(fill = rrl_pct), size = 0.5) +
  geom_sf(data = fs_subset, fill = "grey", alpha = 0.3) + 
  geom_sf(data = fws_subset, fill = NA, color = "blue", size = 0.5)
ggsave("test_map.png", test_map, width = 12, height = 12, dpi = 300)

