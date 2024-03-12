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
conus_attri <- rast("data/processed/archetype_attribute_3km_03-12-2024-11-57.tif")
map.conus <- rast("data/processed/fcm_conus_2024-03-12.tif")

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
  labs(title = "Fuzzy Cluster Map: k=6, m=1.37") +
  theme(legend.position = "bottom")

ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_nf_map_iter006_", Sys.Date(), ".png"), plot = fcm_nf_map, width = 12, height = 12, dpi = 300)  

fcm_reg_map <- ggplot() +
  geom_raster(aes(x = group.df$x, y = group.df$y, fill = as.factor(group.df$Groups))) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", size = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Fuzzy Cluster Map: k=6, m=1.37") +
  theme(legend.position = "bottom")

ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_reg_map_iter006_", Sys.Date(), ".png"), plot = fcm_reg_map, width = 12, height = 12, dpi = 300)  


### And the Forest Region boundaries

## Create maps of the attributes
lcv.df <- conus_attri$LCVScore %>% as.data.frame(xy = TRUE)
rrl.df <- conus_attri$rrlrbn_ %>% as.data.frame(xy = TRUE)
#whp.df <- reg1_attri$WHP_ID %>% as.data.frame(xy = TRUE)
nlcd.df <- conus_attri$`NLCD Land Cover Class` %>% as.data.frame(xy = TRUE)
nlcd.del.df <- conus_attri$Class_Names %>% as.data.frame(xy = TRUE)
vdep.df <- conus_attri$LABEL %>% as.data.frame(xy = TRUE)
fcv.df <- conus_attri$CLASSNAMES %>% as.data.frame(xy = TRUE)

rrl_map <- ggplot() +
  geom_raster(aes(x = rrl.df$x, y = rrl.df$y, fill = rrl.df$rrlrbn_)) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black", size = 2) +
  scale_fill_viridis("Rural-Urban Continuum", option = "plasma", alpha = 0.5) +
  labs(title = "Rural-Urban Continuum", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")

ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/rrl__conus_map_iter005", Sys.Date(), ".png"), plot = rrl_map, width = 12, height = 12, dpi = 300)

lcv_map <- ggplot() +
  geom_raster(aes(x = lcv.df$x, y = lcv.df$y, fill = lcv.df$LCVScore)) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black", size = 2) +
  scale_fill_viridis("LCV Score", option = "plasma", alpha = 0.5) +
  labs(title = "League of Conservation Voters Score", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")

ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/lcv_conus_map_iter005", Sys.Date(), ".png"), plot = lcv_map, width = 12, height = 12, dpi = 300)

nlcd_map <- ggplot() +
  geom_raster(aes(x = nlcd.df$x, y = nlcd.df$y, fill = nlcd.df$`NLCD Land Cover Class`)) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black", size = 2) +
  scale_fill_gradient("Land Cover Class", low = "darkgreen", high = "tan") +
  labs(title = "National Land Cover Class", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")

ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/nlcd_conus_map_iter005", Sys.Date(), ".png"), plot = nlcd_map, width = 12, height = 12, dpi = 300)

nlcd_del_map <- ggplot() +
  geom_raster(aes(x = nlcd.del.df$x, y = nlcd.del.df$y, fill = nlcd.del.df$`NLCD Land Cover Class`)) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black", size = 2) +
  scale_fill_gradient("Land Cover Change", low = "darkgreen", high = "tan") +
  labs(title = "National Land Cover Change", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")

ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/nlcd_del_conus_map_iter005", Sys.Date(), ".png"), plot = nlcd_del_map, width = 12, height = 12, dpi = 300)


vdep_map <- ggplot() +
  geom_raster(aes(x = vdep.df$x, y = vdep.df$y, fill = vdep.df$LABEL)) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "white", size = 2) +
  scale_fill_gradient("Veg. Depart. Index", low = "white", high = "black") +
  labs(title = "Vegetation Departure Index", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")

ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/vdep_conus_map_iter005", Sys.Date(), ".png"), plot = vdep_map, width = 12, height = 12, dpi = 300)

fvc_map <- ggplot() +
  geom_raster(aes(x = fcv.df$x, y = fcv.df$y, fill = fcv.df$CLASSNAMES)) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black", size = 2) +
  scale_fill_viridis("Fuel Vegetation Cover", option = "inferno", alpha = 0.5) +
  labs(title = "", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")

ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fvc_conus_map_iter005_", Sys.Date(), ".png"), plot = fvc_map, width = 12, height = 12, dpi = 300)


whp_map <- ggplot() +
  geom_raster(aes(x = whp.df$x, y = whp.df$y, fill = whp.df$WHP_ID)) +
  geom_sf(data = fs_subset.proj, fill = NA, color = "white", size = 2) +
  scale_fill_viridis("Wildfire Haz. Potential", option = "inferno", alpha = 0.5) +
  labs(title = "Region 1: Wildfire Hazard Potential", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")

ggsave("whp_map.png", plot = whp_map, width = 12, height = 12, dpi = 300)


## Use patch to create a figure with all of the attribute maps
patch_test <- nlcd_map + vdep_map + rrl_map + lcv_map + fvc_map

ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/patch_test_", Sys.Date(), ".png"), plot = patch_test, width = 25, height = 10)


## Create a stat_pointinterval plot
names(conus_attri) <- c("vdep", "fvc", "nlcd", "nlcd_del", "lcv", "ruralurban_cc", "ealr_pfs")

#
all.vals <- c(map.conus[["Groups"]], conus_attri)

vals <- as.data.frame(values(all.vals, na.rm = TRUE, data.frame = TRUE)) 

# scale the values before pivot_longer
vals$lcv <- scale(vals$lcv)
vals$ruralurban_cc <- scale(vals$ruralurban_cc)
vals$vdep <- scale(vals$vdep)
vals$nlcd <- scale(vals$nlcd)
vals$nlcd_del <- scale(vals$nlcd_del)
vals$fvc <- scale(vals$fvc)
vals$ealr_pfs <- scale(vals$ealr_pfs)
colnames(vals) <- c("group", "vdep", "fvc", "nlcd", "nlcd_del", "lcv", "ruralurban_cc", "ealr_pfs")

vals.df <- as.data.frame(vals) %>%
  pivot_longer(., vdep:ealr_pfs, names_to = "variable", values_to = "val")

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

ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/conus_multi_point_plot_iter006_", Sys.Date(), ".png"), plot = multi.int.plot, width = 12, height = 15, dpi = 300)  


                         
