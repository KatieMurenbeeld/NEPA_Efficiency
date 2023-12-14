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

### Practice making maps and using patchwork

lcv.df <- reg1_attri$LCVScore %>% as.data.frame(xy = TRUE)
rrl.df <- reg1_attri$rrlrbn_ %>% as.data.frame(xy = TRUE)

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

ggsave("lcvscore_test_map.png", plot = lcv_map, width = 12, height = 12, dpi = 300)







