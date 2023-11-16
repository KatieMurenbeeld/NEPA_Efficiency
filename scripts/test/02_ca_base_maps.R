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

### Use st_intersection to "clip" the datasets to the FS Region 1 boundary

fws_subset_rg1 <- st_intersection(fws_subset, fs_reg1)
fs_subset_rg1 <- st_intersection(fs_subset, fs_reg1)
cejst_subset_rg1 <- st_intersection(cejst_subset, fs_reg1)
elect_subset_rg1 <- st_intersection(elect_subset, fs_reg1)

### Create test maps with outlines of the Nation Forests in Region 1 and some variables
nf_crit_hab_map <- ggplot() +
  geom_sf(data = fs_subset_rg1, fill = "darkgreen", alpha = 0.5, size = 0.1) + 
  geom_sf(data = fws_subset_rg1, fill = NA, color = "orange", size = 0.2) +
  geom_sf(data = fs_reg1, fill = NA, color = "black", size = 0.5)
ggsave("nf_crit_hab_map.png", nf_crit_hab_map, width = 12, height = 12, dpi = 300)

nf_rural_pct_map <- ggplot() +
  geom_sf(data = elect_subset_rg1, aes(fill = rrl_pct), color = NA, alpha = 0.65, size = 0.1) +
  geom_sf(data = fs_subset_rg1, fill = NA, color = "darkgreen", size = 0.1) +
  geom_sf(data = fs_reg1, fill = NA, color = "black", size = 0.5)
ggsave("nf_rural_pct_map.png", nf_rural_pct_map, width = 12, height = 12, dpi = 300)

nf_ealr_map <- ggplot() +
  geom_sf(data = cejst_subset_rg1, aes(fill = EALR_PFS), color = NA, alpha = 0.65, size = 0.1) +
  geom_sf(data = fs_subset_rg1, fill = NA, color = "darkgreen", size = 0.1) +
  geom_sf(data = fs_reg1, fill = NA, color = "black", size = 0.5)
ggsave("nf_ealr_map.png", nf_ealr_map, width = 12, height = 12, dpi = 300)

