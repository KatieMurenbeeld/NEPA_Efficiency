library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(ggpattern)
library(distributional)
library(ggdist)

# Load the data
fs_nf <- st_read("data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("data/original/S_USA.AdministrativeRegion.shp")
conus_attri <- rast("data/processed/rast_fcm_03_2024-03-29.tif")
map.conus <- rast("data/processed/FCM_03_2024-03-29.tif")

## Reproject the forest service shapes to NAD83
fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=crs(conus_attri))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=crs(conus_attri))
fs_reg.crop <- st_crop(fs_reg.proj, fs_nf.proj)


## Optional: Crop the data to the region 1 boundary using st_intersection
#fs_subset.proj <- st_intersection(fs_subset.proj, fs_reg1.proj)

## Create a map of the clusters with the National Forest boundaries
group.df <- map.conus$Groups %>% as.data.frame(xy = TRUE)

fcm_nf_map <- ggplot() +
  geom_raster(aes(x = group.df$x, y = group.df$y, fill = as.factor(group.df$Groups))) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black", size = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Fuzzy Cluster Map: k=4, m=1.5") +
  theme(legend.position = "bottom")

fcm_nf_map
#ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_nf_map_iter006_", Sys.Date(), ".png"), plot = fcm_nf_map, width = 12, height = 12, dpi = 300)  

### And the Forest Region boundaries

fcm_reg_map <- ggplot() +
  geom_raster(aes(x = group.df$x, y = group.df$y, fill = as.factor(group.df$Groups))) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", size = 150) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Fuzzy Cluster Map: k=4, m=1.5") +
  theme(legend.position = "bottom")

fcm_reg_map
#ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_reg_map_02_", Sys.Date(), ".png"), plot = fcm_reg_map, width = 12, height = 12, dpi = 300)  

## Create maps of the attributes
rrl.df <- conus_attri$RUCC_20 %>% as.data.frame(xy = TRUE)

rrl_map <- ggplot() +
  geom_raster(aes(x = rrl.df$x, y = rrl.df$y, fill = rrl.df$RUCC_20)) +
  geom_sf(data = fs_reg.proj, fill = NA, color = "black", size = 2) +
  #scale_fill_viridis("Rural-Urban Continuum", option = "plasma", alpha = 0.5) +
  labs(title = "Rural-Urban Continuum", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")
rrl_map

#ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/rrl__conus_map_iter005", Sys.Date(), ".png"), plot = rrl_map, width = 12, height = 12, dpi = 300)


## Use patch to create a figure with all of the attribute maps
#patch_test <- rrl_map + 
#patch_test

#ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/patch_test_", Sys.Date(), ".png"), plot = patch_test, width = 25, height = 10)


## Create a stat_pointinterval plot
names(conus_attri) 
#<- c("RUCC_20", "pct_pay", "NAT_AME", "R_NET_M", "WHP", "last")

#
all.vals <- c(map.conus[["Groups"]], conus_attri)

vals <- as.data.frame(values(all.vals, na.rm = TRUE, data.frame = TRUE)) 

# scale the values before pivot_longer
vals$distance_to_crithab_m <- scale(vals$distance_to_crithab_m)
vals$distance_to_wilderness_m <- scale(vals$distance_to_wilderness_m)
vals$last <- scale(vals$last)
vals$WHP <- scale(vals$WHP)
vals$recreation <- scale(vals$recreation)
#vals$last <- scale(vals$last)
#vals$ealr_pfs <- scale(vals$ealr_pfs)
colnames(vals) <- c("group", "RUCC_20", "av_vt_n", "pct_pay", "R_NET_M", "WHP", "last")

vals.df <- as.data.frame(vals) %>%
  pivot_longer(., RUCC_20:last, names_to = "variable", values_to = "val")

# not sure if this part is needed?
vals.df.2 <- vals.df %>% 
  mutate(., cluster = str_remove(variable, " "))

vals.df.sum <- vals.df %>%
  group_by(variable) %>%
  filter(., val > quantile(val, probs = 0.05) & val < quantile(val, probs = 0.95))

## Make the multiple-interval plot
theme_set(theme_ggdist())

multi.int.plot <- ggplot(data = vals.df.sum, aes(x = val, y = variable, color = as.factor(group))) +
  stat_pointinterval(position = "dodge") +
  scale_color_brewer(palette = "Set2") 
multi.int.plot

#ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/conus_multi_point_plot_02_", Sys.Date(), ".png"), plot = multi.int.plot, width = 12, height = 15, dpi = 300)  
