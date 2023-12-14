library(tidyverse)
library(terra)
library(sf)
library(readr)
library(ggplot2)
library(ggmap)
library(tigris)
library(tmap)
library(patchwork)

## Load the data

fs_reg1 <- st_read("data/processed/fs_reg1.shp")
fs_subset <- st_read("data/processed/fs_subset.shp")
#fws_subset <- st_read("data/processed/fws_subset.shp")
#wf_subset <- st_read("data/processed/wf_subset.shp")
#cejst_subset <- st_read("data/processed/cejst_subset.shp")
#elect_subset <- st_read("data/processed/elect_subset.shp")
#landuse_subset <- rast("data/processed/landuse_subset.tif")
reg1_attri <- rast("data/processed/reg1_archetype_attribute_3km.tif")

## Reproject the forest service shapes to NAD83
fs_subset.proj <- fs_subset %>% st_transform(., crs=crs(reg1_attri))
fs_reg1.proj <- fs_reg1 %>% st_transform(., crs=crs(reg1_attri))

## Map 

### Use st_intersection to "clip" the datasets to the FS Region 1 boundary

#fws_subset_rg1 <- st_intersection(fws_subset, fs_reg1)
#fs_subset_rg1 <- st_intersection(fs_subset, fs_reg1)
#cejst_subset_rg1 <- st_intersection(cejst_subset, fs_reg1)
#elect_subset_rg1 <- st_intersection(elect_subset, fs_reg1)
fs_subset.proj <- st_intersection(fs_subset.proj, fs_reg1.proj)
reg1_attri <- crop(x = reg1_attri, y = vect(fs_reg1.proj), snap = "near", mask = TRUE)


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

#nf_landuse_map <- ggplot(landuse_subset) +
#  geom_sf(data = fs_subset_rg1, fill = NA, color = "darkgreen", size = 0.1) +
#  geom_sf(data = fs_reg1, fill = NA, color = "black", size = 0.5)
#ggsave("nf_landuse_map.png", nf_landuse_map, width = 12, height = 12, dpi = 300)

plot(landuse_subset)
plot(st_geometry(fs_subset_rg1), add = TRUE)

### Practice using patchwork

lcv.df <- reg1_attri$LCVScore %>% as.data.frame(xy = TRUE)
rrl.df <- reg1_attri$rrlrbn_ %>% as.data.frame(xy = TRUE)

lcv <- plot(reg1_attri$LCVScore)
rural <- plot(reg1_attri$rrlrbn_)
whp <- plot(reg1_attri$WHP_ID)
vdep <- plot(reg1_attri$LABEL)
nlcd <- plot(reg1_attri$`NLCD Land Cover Class`)

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

rrl_map + lcv_map



