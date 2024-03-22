library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(tigris)

#---Load the shapefiles-----

# variables from csv county data
all_vars <- st_read(here::here("data/processed/all_vars_to_rst.shp"))

# Wilderness areas
wild <- st_read(here::here("data/original/S_USA.Wilderness.shp"))

# Critical habitat
crithab <- st_read(here::here("data/original/crithab_poly.shp"))

#---Load reference raster----
ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
crs(ref_rast)

#---Transform the projection of shapefiles----
st_crs(all_vars)

all_vars_proj <- all_vars %>% st_transform(., crs = crs(ref_rast))
wild_proj <- wild %>% st_transform(., crs = crs(ref_rast))
crithab_proj <- crithab %>% st_transform(., crs = crs(ref_rast))

#---Rasterize shapefiles with variables of interest----
rruc_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "RUCC_20")
vtpres_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "av_vt_p")
vtnopres_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "av_vt_n")
econ15_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "e__2015")
percent_demvt_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "ave_dem")
percent_repvt_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "ave_rep")
percent_forpay_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "pct_pay")
fordep_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "frst_dp")
lesscoll_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "lsscll_")
nam_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "NAT_AME")
delpop_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "R_NET_M")

crithab_rast <- rasterize(vect(crithab_proj), ref_rast, field = "listing_st")

wild_rast <- rasterize(vect(wild_proj), ref_rast, field = "BOUNDARYST")


#---Check alignment and extents-----
rast_stack <- c(rruc_rast, vtpres_rast, vtnopres_rast, econ15_rast,
                percent_demvt_rast, percent_repvt_rast, percent_forpay_rast,
                fordep_rast, lesscoll_rast, nam_rast, delpop_rast, crithab_rast, 
                wild_rast, ref_rast)

writeRaster(x = rast_stack, filename = paste0(here::here("data/processed/"), "arch_attri_", Sys.Date(), ".tif"), overwrite = TRUE)

#---Check on the plots----
plot(rast_stack$av_vt_p)
plot(rast_stack$av_vt_n)
plot(rast_stack)

rast_stack$av_vt_n <- clamp(rast_stack$av_vt_n, upper=1.0, value=FALSE)
plot(rast_stack$av_vt_n)
