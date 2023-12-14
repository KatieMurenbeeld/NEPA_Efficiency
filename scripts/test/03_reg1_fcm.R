library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)

# Load the data
fs_reg1 <- st_read("data/processed/fs_reg1.shp")
fs_subset <- st_read("data/processed/fs_subset.shp")
reg1_attri <- rast("data/processed/reg1_attri_crop_3km.tif")

### The following 2 little code chunks shouldn't be in this script. Need to figure out how to save the cropped NF boundaries as a .shp.
## Reproject the forest service shapes to NAD83
fs_subset.proj <- fs_subset %>% st_transform(., crs=crs(reg1_attri))
fs_reg1.proj <- fs_reg1 %>% st_transform(., crs=crs(reg1_attri))

## Crop the data to the region 1 boundary using st_intersection
fs_subset.proj <- st_intersection(fs_subset.proj, fs_reg1.proj)

## Process raster for use with geocmeans

### Update the names 
names(reg1_attri) <- c("status", "lcvscore", "rural_cc", "vdep", "nlcd", "whp")

### Remove the status band since they are all NA
reg1_attri <- reg1_attri[[c(2:6)]]

## Convert to a simple list of SpatRaster
dataset <- lapply(names(reg1_attri), function(n){
  aband <- reg1_attri[[n]]
  return(aband)
})

names(dataset) <- names(reg1_attri)

### Test out running a FCM with k = 3 and m = 1.5
FCM_result <- CMeans(dataset, k = 3, m = 1.5, standardize = TRUE)

## Visually review the clusters

Maps.k3 <- mapClusters(object = FCM_result, undecided = 0.45)

test_fcm <- Maps.k3$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set2")

ggsave("test_fcm.png", plot = test_fcm, width = 12, height = 12, dpi = 300)
