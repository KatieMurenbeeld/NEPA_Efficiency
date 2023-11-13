# 
library(tidyverse)
library(sf)
library(terra)

# Some files are large so set the timeout to 2 minutes (120 seconds)
options(timeout=120)

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

## Use tempfile() and unzip() to load the Land Cover Change Index
## and the Land Cover data 

tmp <- tempfile()
nlcd.del.url <- "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2001_2021_land_cover_change_index_l48_20230630.zip"
download.file(nlcd.del.url, tmp)
tmp2 <- tempfile()
unzip(zipfile=tmp, exdir = tmp2 )
nlcd.del.rast <- rast(tmp2)

# Vegetation departure index (landfire 2020)
# downloaded from landfire.gov/viewer 13 Nov 2023 see LC20_VDep_220.GeoJSON file 

vdep <- rast("data/original/LC20_VDep_220.tif") # may still be too large? >2 GB

#tmp <- tempfile()
#vdep.url <- "https://www.landfire.gov/bulk/downloadfile.php?FNAME=US_220_mosaic-LF2020_VDep_220_CONUS.zip&TYPE=landfire"

# Load Election Context 2018
# This is a csv of county data and will need to be joined to county data

elect.cntx <- read_csv("data/original/election-context-2018.csv") 

