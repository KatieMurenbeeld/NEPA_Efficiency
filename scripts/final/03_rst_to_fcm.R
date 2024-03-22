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
arch_attri <- rast(here::here("data/processed/arch_attri_2024-03-22.tif"))

# ignore for now, file corrupted?
#gpp_rast <- rast(here::here("data/original/CarbonFlux_2000_2013/Average_GPP_2000_2013.img"))

#---Update categorical data?----


#---Process raster for use with geocmeans----

# Update the names if you want 
names(arch_attri)

# Remove the wilderness and crithab for now
arch_attri <- arch_attri[[c(1:11)]]

# Scale the data
arch_attri_sc <- scale(arch_attri)

# Quickly investigate the correlation between the attributes
layerCor(arch_attri_sc, "pearson", na.rm = TRUE)

# Convert to a simple list of SpatRaster
dataset <- lapply(names(arch_attri_sc), function(n){
  aband <- arch_attri_sc[[n]]
  return(aband)
})

names(dataset) <- names(arch_attri_sc)

#---Find appropriate parameter values----
### Code below from https://jeremygelb.github.io/geocmeans/articles/web_vignettes/rasters.html
# finding an appropriate k and m values
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, 
                                  k = 2:11, m = seq(1.0,2.5,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  verbose = TRUE)

# plotting the silhouette index values
sil.idx <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 8) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()

# plotting the explained inertia
ex.inert <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 8) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

xieb.inert <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = XieBeni.index)) + 
  geom_text(aes(x = m, y = k, label = round(XieBeni.index,2)), size = 8) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

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


# Test out running a FCM with k = 3 and m = 1.5
FCM_result_k3 <- CMeans(dataset, k = 5, m = 1.1, standardize = FALSE)
map.res.k3 <- rast(FCM_result_k3$rasters)
#writeRaster(map.res.k3[["Groups"]], filename = "data/processed/FCM_k3.tif")

## Visually review the clusters

Maps.k3 <- mapClusters(object = FCM_result_k3, undecided = 0.45)

test_fcm <- Maps.k3$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set2")
test_fcm

#ggsave("test_fcm.png", plot = test_fcm, width = 12, height = 12, dpi = 300)













