library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(ggpattern)

# Load the data
fs_reg1 <- st_read("data/processed/fs_reg1.shp")
fs_subset <- st_read("data/processed/fs_subset.shp")
reg1_attri <- rast("data/processed/reg1_attri_crop_3km.tif")
FCM_result_k5 <- readRDS("data/processed/fcm_result_k5.rds") ## not sure if I read this in correctly

### The following 2 little code chunks shouldn't be in this script. Need to figure out how to save the cropped NF boundaries as a .shp.
## Reproject the forest service shapes to NAD83
fs_subset.proj <- fs_subset %>% st_transform(., crs=crs(reg1_attri))
fs_reg1.proj <- fs_reg1 %>% st_transform(., crs=crs(reg1_attri))

## Crop the data to the region 1 boundary using st_intersection
fs_subset.proj <- st_intersection(fs_subset.proj, fs_reg1.proj)

## Create the map of the fuzzy clusters
#Maps.k5 <- mapClusters(object = FCM_result_k5, undecided = 0.45)

#fcm_k5_map <- Maps.k5$ClusterPlot + theme(legend.position = "bottom") + 
#  scale_fill_brewer(palette = "Set2")

## Create maps of the attributes
lcv.df <- reg1_attri$LCVScore %>% as.data.frame(xy = TRUE)
rrl.df <- reg1_attri$rrlrbn_ %>% as.data.frame(xy = TRUE)
whp.df <- reg1_attri$WHP_ID %>% as.data.frame(xy = TRUE)
nlcd.df <- reg1_attri$`NLCD Land Cover Class` %>% as.data.frame(xy = TRUE)
vdep.df <- reg1_attri$LABEL %>% as.data.frame(xy = TRUE)

rrl_map <- ggplot() +
  geom_raster(aes(x = rrl.df$x, y = rrl.df$y, fill = rrl.df$rrlrbn_)) +
  geom_sf(data = fs_subset.proj, fill = "white", color = "black", size = 1.5, alpha = 0.25) +
  scale_fill_viridis("Rural-Urban Continuum", option = "plasma", alpha = 0.5) +
  labs(title = "Region 1: Rural-Urban Continuum", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")

ggsave("rrl_map.png", plot = rrl_map, width = 12, height = 12, dpi = 300)

lcv_map <- ggplot() +
  geom_raster(aes(x = lcv.df$x, y = lcv.df$y, fill = lcv.df$LCVScore)) +
  geom_sf(data = fs_subset.proj, fill = "white", color = "black", size = 1.5, alpha = 0.25) +
  scale_fill_viridis("LCV Score", option = "plasma", alpha = 0.5) +
  labs(title = "Region 1: League of Conservation Voters Score", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")

ggsave("lcv_map.png", plot = lcv_map, width = 12, height = 12, dpi = 300)

nlcd_map <- ggplot() +
  geom_raster(aes(x = nlcd.df$x, y = nlcd.df$y, fill = nlcd.df$`NLCD Land Cover Class`)) +
  geom_sf(data = fs_subset.proj, fill = "white", color = "black", size = 1.5, alpha = 0.05) +
  scale_fill_gradient("Land Cover Class", low = "tan", high = "darkgreen") +
  labs(title = "Region 1: National Land Cover Class", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")

ggsave("nlcd_map.png", plot = nlcd_map, width = 12, height = 12, dpi = 300)

vdep_map <- ggplot() +
  geom_raster(aes(x = vdep.df$x, y = vdep.df$y, fill = vdep.df$LABEL)) +
  geom_sf(data = fs_subset.proj, fill = "white", color = "black", size = 1.5, alpha = 0.05) +
  scale_fill_gradient("Veg. Depart. Index", low = "white", high = "black") +
  labs(title = " Region 1: Vegetation Departure Index", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")

ggsave("vdep_map.png", plot = vdep_map, width = 12, height = 12, dpi = 300)

whp_map <- ggplot() +
  geom_raster(aes(x = whp.df$x, y = whp.df$y, fill = whp.df$WHP_ID)) +
  geom_sf(data = fs_subset.proj, fill = "white", color = "black", size = 1.5, alpha = 0.05) +
  scale_fill_viridis("Wildfire Haz. Potential", option = "inferno", alpha = 0.5) +
  labs(title = "Region 1: Wildfire Hazard Potential", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")

ggsave("whp_map.png", plot = whp_map, width = 12, height = 12, dpi = 300)



