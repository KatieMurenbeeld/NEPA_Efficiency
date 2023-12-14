library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)

# Load the data
fs_reg1 <- st_read("data/processed/fs_reg1.shp")
fs_subset <- st_read("data/processed/fs_subset.shp")
reg1_attri <- rast("data/processed/reg1_attri_crop_3km.tif")
FCM_result_k5 <- readRDS("data/processed/fcm_result_k5.rds")

### The following 2 little code chunks shouldn't be in this script. Need to figure out how to save the cropped NF boundaries as a .shp.
## Reproject the forest service shapes to NAD83
fs_subset.proj <- fs_subset %>% st_transform(., crs=crs(reg1_attri))
fs_reg1.proj <- fs_reg1 %>% st_transform(., crs=crs(reg1_attri))

## Crop the data to the region 1 boundary using st_intersection
fs_subset.proj <- st_intersection(fs_subset.proj, fs_reg1.proj)




