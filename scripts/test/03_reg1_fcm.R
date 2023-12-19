## some code came from https://jeremygelb.github.io/geocmeans/articles/web_vignettes/rasters.html

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

#writeRaster(dataset, filename = "data/processed/reg1_rst_stk.tif")

### Test out running a FCM with k = 3 and m = 1.5
FCM_result_k3 <- CMeans(dataset, k = 3, m = 1.5, standardize = TRUE)
map.res.k3 <- rast(FCM_result_k3$rasters)
writeRaster(map.res.k3[["Groups"]], filename = "data/processed/FCM_k3.tif")

## Visually review the clusters

Maps.k3 <- mapClusters(object = FCM_result_k3, undecided = 0.45)

test_fcm <- Maps.k3$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set2")

ggsave("test_fcm.png", plot = test_fcm, width = 12, height = 12, dpi = 300)

### Code below from https://jeremygelb.github.io/geocmeans/articles/web_vignettes/rasters.html
# finding an appropriate k and m values
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, 
                                  k = 2:7, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  verbose = TRUE)

# plotting the silhouette index values
sil.idx <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 8) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
  
ggsave("sil_ind.png", sil.idx, width = 12, height = 12, dpi = 300)

# plotting the explained inertia
ex.inert <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 8) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

ggsave("exp_intert.png", ex.inert, width = 12, height = 12, dpi = 300)

# Based on the results above, run FCM with k = 5 and m = 1.25
FCM_result_k5 <- CMeans(dataset, k = 5, m = 1.25, standardize = TRUE)
map.res.k5 <- rast(FCM_result_k5$rasters)
writeRaster(map.res.k5[["Groups"]], filename = "data/processed/FCM_k5.tif")

# save the result as an rds file
write_rds(FCM_result_k5, "data/processed/fcm_result_k5.rds")

## Visually review the clusters

Maps.k5 <- mapClusters(object = FCM_result_k5, undecided = 0.45)

fcm_k5_map <- Maps.k5$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set2")

ggsave("fcm_k5_map.png", plot = fcm_k5_map, width = 12, height = 12, dpi = 300)
