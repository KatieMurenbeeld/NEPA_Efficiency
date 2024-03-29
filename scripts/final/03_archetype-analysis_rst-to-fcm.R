## some code came from https://jeremygelb.github.io/geocmeans/articles/web_vignettes/rasters.html

library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)

#---Load the data-----
ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
arch_attri <- rast(here::here("data/processed/arch_attri_03_2024-03-27.tif"))
mill_dist <- rast(here::here("data/processed/mill_dist_open.tif"))
mill_cap_hot <- rast(here::here("data/processed/test_hotspot.tif"))
agc_flux <- rast("~/Analysis/NEPA_Efficiency/data/original/forest_aboveground_carbon_flux_ED_GEDI_ICESat2.tif")
agc_stock <- rast("~/Analysis/NEPA_Efficiency/data/original/forest_aboveground_carbon_stock_ED_GEDI.tif")
# ignore for now, file corrupted?
#gpp_rast <- rast(here::here("data/original/CarbonFlux_2000_2013/Average_GPP_2000_2013.img"))

#---Process the original rasters----
# Resample then crop
agc_stock_proj <- project(agc_stock, 
                          y = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

agc_stock_crop <- crop(agc_stock_proj, ref_rast, mask = TRUE)



# Check alignment 
rast_stack <- c(arch_attri, mill_dist)

#---Process raster for use with geocmeans----

# Update the names if you want 
names(rast_stack)

# Select Variables for FCmeans
rst_fcm <- rast_stack[[c("RUCC_20", "av_vt_n", "pct_pay", "R_NET_M", "WHP", "last")]]
#writeRaster(rst_fcm, paste0("data/processed/rast_fcm_02_", Sys.Date(), ".tif"), overwrite = TRUE)

# Scale the data
rst_fcm_sc <- scale(rst_fcm)

# Quickly investigate the correlation between the attributes
layerCor(rst_fcm, "pearson", na.rm = TRUE)

# Convert to a simple list of SpatRaster
dataset <- lapply(names(rst_fcm_sc), function(n){
  aband <- rst_fcm_sc[[n]]
  return(aband)
})

names(dataset) <- names(rst_fcm_sc)

#---Find appropriate parameter values----
### Code below from https://jeremygelb.github.io/geocmeans/articles/web_vignettes/rasters.html
# finding an appropriate k and m values
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, 
                                  k = 2:6, m = seq(1.2,2.0,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  verbose = TRUE)

# plotting the silhouette index values
sil.idx <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 8) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx

# plotting the explained inertia
ex.inert <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 8) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert

xieb.inert <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = XieBeni.index)) + 
  geom_text(aes(x = m, y = k, label = round(XieBeni.index,2)), size = 8) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
xieb.inert
# Test out other clustering

kmeans <- clusteringRaster(arch_attri_sc, k = 2:5, writeRasterData = FALSE, getSpatRaster = TRUE, 
                 method="kmeans", calcIntCriteria = FALSE, crit = c("Silhouette"), 
                 intCritSampSize = 20000, verbose = TRUE)

# use kmeans()?
v <- na.omit(values(arch_attri_sc))
set.seed(99)
kmn <- kmeans(v, centers=4, iter.max = 500, nstart = 5, algorithm="Lloyd")

kr <- rast(arch_attri_sc, nlyr=1)
i <- attr(v, "na.action")
j <- (1:ncell(arch_attri_sc))[-i]
kr[j] <- kmn$cluster

plot(kr)


# Test out running a FCM 
FCM_result <- CMeans(dataset, k = 5, m = 1.2, standardize = FALSE)
map.res <- rast(FCM_result$rasters)
#writeRaster(map.res[["Groups"]], filename = paste0("data/processed/FCM_02", Sys.Date(), ".tif"))

## Visually review the clusters

Maps <- mapClusters(object = FCM_result, undecided = 0.45)

test_fcm <- Maps$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set2")
test_fcm

#ggsave("test_fcm.png", plot = test_fcm, width = 12, height = 12, dpi = 300)













