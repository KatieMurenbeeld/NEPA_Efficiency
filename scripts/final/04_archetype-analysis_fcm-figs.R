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
library(ggsci)

# Load the data
fs_nf <- st_read("data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("data/original/S_USA.AdministrativeRegion.shp")
conus_attri <- rast("data/processed/rast_fcm_05_2024-03-29.tif")
map.conus <- rast("data/processed/FCM_05_2024-03-29.tif")

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
  labs(title = "Fuzzy Cluster Map: k=6, m=1.25") +
  theme(legend.position = "bottom")

fcm_nf_map
#ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_nf_map_iter006_", Sys.Date(), ".png"), plot = fcm_nf_map, width = 12, height = 12, dpi = 300)  

### And the Forest Region boundaries

fcm_reg_map <- ggplot() +
  geom_raster(aes(x = group.df$x, y = group.df$y, fill = as.factor(group.df$Groups))) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", size = 150) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Fuzzy Cluster Map: k=5, m=2.25") +
  theme(legend.position = "bottom")

fcm_reg_map
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_05_reg_map_", Sys.Date(), ".png"), plot = fcm_reg_map, width = 12, height = 12, dpi = 300)  

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
vals$low_nam <- scale(vals$low_nam)
vals$high_nam <- scale(vals$high_nam)
vals$LIF_PFS <- scale(vals$LIF_PFS)
vals$LMI_PFS <- scale(vals$LMI_PFS)
vals$av_vt_n <- scale(vals$av_vt_n)
vals$pct_pay <- scale(vals$pct_pay)

colnames(vals) <- c("group", "dist_crit", "dist_wild", "dist_mill", "low_nam",
                    "high_nam", "lang_iso", "low_med_income", "av_vt_n", "pct_forest_pay")

vals.df <- as.data.frame(vals) %>%
  pivot_longer(., dist_crit:pct_forest_pay, names_to = "variable", values_to = "val")

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

ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_05_multi_point_plot_", Sys.Date(), ".png"), plot = multi.int.plot, width = 12, height = 15, dpi = 300)  

## Make a different plot to look at distribution of attribute values in clusters
vals.df.sum2 <- vals.df %>% 
  group_by(variable, group) %>% 
  median_qi(.width = c(.5, .8, .95))

fcm.hist <- ggplot(data= vals.df.sum2, mapping=aes(
  x = fct_rev(variable), y = val, 
  ymin = .lower, ymax = .upper,
  # size = -.width means smaller probability interval => thicker line
  # this can be omitted, geom_pointinterval includes it automatically
  # if a .width column is in the input data.
  linewidth = 1, color=as.factor(group), alpha=0.5
)) +  
  geom_interval(show.legend = FALSE) +
  scale_color_brewer(palette = "Set2") + 
  scale_alpha_manual(values=c(1,0.7, 0.4), aesthetic="interval_alpha") +
  scale_y_continuous()+
  scale_x_discrete()+
  ylab("Standardized Value") +
  xlab("Variable") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), aspect.ratio = 0.5)+
  facet_wrap(vars(group), scales="free_y", ncol=1, strip.position = "left")
fcm.hist

ggsave(here::here("figures/fcm_05_attri_hist.png"), fcm.hist,
       width = 15, height = 20, dpi = 300)



