# 
library(sf)
library(terra)

## Use tempfile() and unzip() to load the US Forest Service National Forest and Regional boundaries
tmp <- tempfile()
fs.nf.url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
download.file(fs.nf.url, tmp)
tmp2 <- tempfile()
unzip(zipfile=tmp, exdir = tmp2 )
fs.nf.bdry <- read_sf(tmp2)

tmp <- tempfile()
fs.rg.url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeRegion.zip"
download.file(fs.rg.url, tmp)
tmp2 <- tempfile()
unzip(zipfile=tmp, exdir = tmp2 )
fs.rg.bdry <- read_sf(tmp2)

## Use tempfile() and unzip() to load the Threatened and Endangered Species Critical Habitats

tmp <- tempfile()
fws.te.url <- "https://ecos.fws.gov/docs/crithab/crithab_all/crithab_all_layers.zip"
download.file(fws.te.url, tmp)
tmp2 <- tempfile()
unzip(zipfile=tmp, exdir = tmp2 )
fws.te.bdry <- read_sf(tmp2)

# Vegetation departure index (landfire 2020)
# downloaded from landfire.gov/viewer 13 Nov 2023 see LC20_VDep_220.GeoJSON file 

vdep <- rast("data/original/LC20_VDep_220.tif") # may still be too large? >2 GB

#tmp <- tempfile()
#vdep.url <- "https://www.landfire.gov/bulk/downloadfile.php?FNAME=US_220_mosaic-LF2020_VDep_220_CONUS.zip&TYPE=landfire"

# Load Election Context 2018
# This is a csv of county data so will need to join to county data


