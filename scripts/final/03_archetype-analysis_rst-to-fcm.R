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
arch_attri <- rast(here::here("data/processed/arch_attri_04_2024-03-29.tif"))
mill_dist <- rast(here::here("data/processed/mill_dist_open.tif"))
mill_cap_hot <- rast(here::here("data/processed/test_hotspot.tif"))
agc_flux <- rast("~/Analysis/NEPA_Efficiency/data/original/forest_aboveground_carbon_flux_ED_GEDI_ICESat2.tif")
agc_stock <- rast("~/Analysis/NEPA_Efficiency/data/original/forest_aboveground_carbon_stock_ED_GEDI.tif")

# ignore for now, file corrupted?
#gpp_rast <- rast(here::here("data/original/CarbonFlux_2000_2013/Average_GPP_2000_2013.img"))

#---Process the original rasters----
# Resample then crop
#agc_stock_proj <- project(agc_stock, 
#                          y = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

#agc_stock_crop <- crop(agc_stock_proj, ref_rast, mask = TRUE)



# Check alignment 
rast_stack <- c(arch_attri, mill_dist)

#---Process raster for use with geocmeans----

# Update the names if you want 
names(rast_stack)

# Select Variables for FCmeans
rst_fcm <- rast_stack[[c("distance_to_crithab_m", "distance_to_wilderness_m", 
                         "last", "WHP", "recreation", "LIF_PFS", "LMI_PFS", 
                         "EALR_PFS", "av_vt_n", "pct_pay")]]
names(rst_fcm)
#writeRaster(rst_fcm, paste0("data/processed/rast_fcm_04_", Sys.Date(), ".tif"), overwrite = TRUE)

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
                                  k = 2:6, m = seq(1.0,2.5,0.25), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Silhouette.index"),
                                  verbose = TRUE)

# plotting the silhouette index values
sil.idx <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 8) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx
ggsave(here::here("figures/FCM_05_sil_idx.png"), sil.idx, 
       width = 12, height = 12, dpi = 300)

# plotting the explained inertia
ex.inert <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 8) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert
ggsave(here::here("figures/FCM_05_ex_inert.png"), ex.inert, 
       width = 12, height = 12, dpi = 300)

#---Run the FCM---- 
FCM_result <- CMeans(dataset, k = 5, m = 2.25, standardize = FALSE)
map.res <- rast(FCM_result$rasters)
writeRaster(map.res[["Groups"]], filename = paste0("data/processed/FCM_05_", Sys.Date(), ".tif"))


##---save the iteration, k, m as a dataframe----
#aa_iteration <- data.frame(iteration_name = character(),
#                           attris = character(),
#                           k = numeric(),
#                           m = numeric())

iteration_name <- "FCM_05"
attris <- paste(names(rst_fcm), collapse= ", ")
k <- 5
m <- 2.25

aa_iteration[nrow(aa_iteration) + 1,] <- list(iteration_name, attris, k, m)

write_csv(aa_iteration, here::here("data/processed/aa_iteration.csv"), append = TRUE)






