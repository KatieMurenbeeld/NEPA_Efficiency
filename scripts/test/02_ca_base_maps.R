library(tidyverse)
library(terra)
library(sf)
library(readr)
library(ggplot2)
library(ggmap)
library(tigris)

## Load the data

fs_reg1 <- st_read("data/processed/fs_reg1.shp")
fs_subset <- st_read("data/processed/fs_subset.shp")
fws_subset <- st_read("data/processed/fws_subset.shp")
wf_subset <- st_read("data/processed/wf_subset.shp")
cejst_subset <- st_read("data/processed/cejst_subset.shp")
elect_subset <- st_read("data/processed/elect_subset.shp")
landuse_subset <- rast("data/processed/landuse_subset.tif")

## Map 

fws_subset_rg1 <- st_intersection(fws_subset, fs_reg1)
fs_subset_rg1 <- st_intersection(fs_subset, fs_reg1)
cejst_subset_rg1 <- st_intersection(cejst_subset, fs_reg1)
elect_subset_rg1 <- st_intersection(elect_subset, fs_reg1)

test_map <- ggplot() +
  #geom_sf(data = elect_subset_rg1, aes(fill = rrlrbn_), color = NA, alpha = 0.3, size = 0.1) +
  geom_sf(data = cejst_subset_rg1, aes(fill = EALR_PFS), color = NA, alpha = 0.5, size = 0.1) +
  geom_sf(data = fs_subset_rg1, fill = NA, color = "darkgreen", size = 0.1) + 
  geom_sf(data = fws_subset_rg1, fill = NA, color = "orange", size = 0.1)
ggsave("test_map.png", test_map, width = 12, height = 12, dpi = 300)

