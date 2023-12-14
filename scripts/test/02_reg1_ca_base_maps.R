library(tidyverse)
library(terra)
library(sf)
library(readr)
library(ggplot2)
library(ggmap)
library(tigris)
library(tmap)
library(patchwork)

# Load the data
fs_reg1 <- st_read("data/processed/fs_reg1.shp")
fs_subset <- st_read("data/processed/fs_subset.shp")
reg1_attri <- rast("data/processed/reg1_archetype_attribute_3km.tif")

## Reproject the forest service shapes to NAD83
fs_subset.proj <- fs_subset %>% st_transform(., crs=crs(reg1_attri))
fs_reg1.proj <- fs_reg1 %>% st_transform(., crs=crs(reg1_attri))

## Crop the data to the region 1 boundary using st_intersection and crop
fs_subset.proj <- st_intersection(fs_subset.proj, fs_reg1.proj)
reg1_attri <- crop(x = reg1_attri, y = vect(fs_reg1.proj), snap = "near", mask = TRUE)

### Save the cropped raster
writeRaster(reg1_attri, "data/processed/reg1_attri_crop_3km.tif")

### Make maps of the region1 stacked raster 

lcv.df <- reg1_attri$LCVScore %>% as.data.frame(xy = TRUE)
rrl.df <- reg1_attri$rrlrbn_ %>% as.data.frame(xy = TRUE)
whp.df <- reg1_attri$WHP_ID %>% as.data.frame(xy = TRUE)
nlcd.df <- reg1_attri$`NLCD Land Cover Class` %>% as.data.frame(xy = TRUE)
vdep.df <- reg1_attri$LABEL %>% as.data.frame(xy = TRUE)

lcv_map <- ggplot() +
  geom_raster(aes(x = lcv.df$x, y = lcv.df$y, fill = lcv.df$LCVScore)) +
  geom_sf(data = fs_subset.proj, fill = NA, color = "black", size = 0.1) +
  theme_bw() +
  theme()

ggsave("lcvscore_test_map.png", plot = lcv_map, width = 12, height = 12, dpi = 300)

rrl_map <- ggplot() +
  geom_raster(aes(x = rrl.df$x, y = rrl.df$y, fill = rrl.df$rrlrbn_)) +
  geom_sf(data = fs_subset.proj, fill = NA, color = "black", size = 0.1) +
  theme_bw() +
  theme()

ggsave("rrl_test_map.png", plot = rrl_map, width = 12, height = 12, dpi = 300)

whp_map <- ggplot() +
  geom_raster(aes(x = whp.df$x, y = whp.df$y, fill = whp.df$WHP_ID)) +
  geom_sf(data = fs_subset.proj, fill = NA, color = "black", size = 0.1) +
  theme_bw() +
  theme()

ggsave("whp_test_map.png", plot = whp_map, width = 12, height = 12, dpi = 300)

nlcd_map <- ggplot() +
  geom_raster(aes(x = nlcd.df$x, y = nlcd.df$y, fill = nlcd.df$`NLCD Land Cover Class`)) +
  geom_sf(data = fs_subset.proj, fill = NA, color = "black", size = 0.1) +
  theme_bw() +
  theme()

ggsave("nlcd_test_map.png", plot = nlcd_map, width = 12, height = 12, dpi = 300)

vdep_map <- ggplot() +
  geom_raster(aes(x = vdep.df$x, y = vdep.df$y, fill = vdep.df$LABEL)) +
  geom_sf(data = fs_subset.proj, fill = NA, color = "black", size = 0.1) +
  theme_bw() +
  theme()

ggsave("vdep_test_map.png", plot = vdep_map, width = 12, height = 12, dpi = 300)


## Create a patchwork figure

patch <- rrl_map + lcv_map

ggsave("patch_test.png", plot = patch, width =12, height = 12, dpi = 300)

patch_reg1 <- (nlcd_map | vdep_map | rrl_map) /
  (whp_map | vdep_map)

ggsave("patch_reg1.png", plot = patch_reg1, width =12, height = 12, dpi = 300)


