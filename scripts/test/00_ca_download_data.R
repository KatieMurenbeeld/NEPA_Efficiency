# 
library(tidyverse)
library(sf)
library(terra)
library(tigris)

# Some files are large so set the timeout to 2 minutes (120 seconds)
options(timeout=120)

## Use tempfile() and unzip() to load the US Forest Service National Forest and Regional boundaries (fs.nf, fs.rg)
tmp <- tempfile()
fs.nf.url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
download.file(fs.nf.url, tmp)
tmp2 <- tempfile()
unzip(zipfile=tmp, exdir = tmp2 )
fs.nf.bdry <- read_sf(tmp2)
# on borah write the shapefile to scratch space 
# write_sf(fs.nf.bdry, dsn = "/bsuscratch/katiemurenbeeld/archetype_analysis/data/original/fs_nf_bdry.shp", append = FALSE)

tmp <- tempfile()
fs.rg.url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeRegion.zip"
download.file(fs.rg.url, tmp)
tmp2 <- tempfile()
unzip(zipfile=tmp, exdir = tmp2 )
fs.rg.bdry <- read_sf(tmp2)

## Use tempfile() and unzip() to load the Threatened and Endangered Species Critical Habitats (fws.te)

tmp <- tempfile()
fws.te.url <- "https://ecos.fws.gov/docs/crithab/crithab_all/crithab_all_layers.zip"
download.file(fws.te.url, tmp)
tmp2 <- tempfile()
unzip(zipfile=tmp, exdir = tmp2 )
fws.te.bdry <- read_sf(tmp2) # This warning comes up because there are two shapefiles. One for line and one for polygons.

## Use tempfile() and unzip() to load the Climate and Economic Justice Screening Tool data (cejst)

tmp <- tempfile()
cejst.url <- "https://static-data-screeningtool.geoplatform.gov/data-versions/1.0/data/score/downloadable/1.0-shapefile-codebook.zip"
download.file(cejst.url, tmp)
tmp2 <- tempfile()
unzip(zipfile = tmp, exdir = tmp2)
#cejst.bdry <- read_sf(tmp2) # Error: Cannot open "/private/var/folders/rr/nq1j46pd355fn_0gk__qf1s40000gn/T/RtmpsH47Ws/filef2191f2cd424"; The source could be corrupt or not supported. See `st_drivers()` for a list of supported formats.

## Use tempfile() and unzip() to load the Land Cover Change Index
## and the Land Cover data 

tmp <- tempfile()
nlcd.del.url <- "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2001_2021_land_cover_change_index_l48_20230630.zip"
download.file(nlcd.del.url, tmp)
tmp2 <- tempfile()
unzip(zipfile=tmp, exdir = tmp2 )
#nlcd.del.rast <- rast(tmp2)

# Wildfire Incidents
tmp <- tempfile()
wf.mt.url <- "https://s3-us-west-2.amazonaws.com/fs.usda.rds/RDS-2020-0016/RDS-2020-0016_Montana.zip"
wf.id.url <- "https://s3-us-west-2.amazonaws.com/fs.usda.rds/RDS-2020-0016/RDS-2020-0016_Idaho.zip"
wf.nd.url <- "https://s3-us-west-2.amazonaws.com/fs.usda.rds/RDS-2020-0016/RDS-2020-0016_NorthDakota.zip"
wf.sd.url <- "https://s3-us-west-2.amazonaws.com/fs.usda.rds/RDS-2020-0016/RDS-2020-0016_SouthDakota.zip"
wf.wy.url <- "https://s3-us-west-2.amazonaws.com/fs.usda.rds/RDS-2020-0016/RDS-2020-0016_Wyoming.zip"

#wf.incidents <- read_csv(file = "/Users/kathrynmurenbeeld/Analysis/assignment-7-combiningdata-KatieMurenbeeld/data/ics209plus-wildfire/ics209-plus-wf_incidents_1999to2020.csv",
#                         col_names = TRUE, col_types = NULL,
#                         na = c("", "NA")) %>%
#  drop_na(POO_LONGITUDE, POO_LATITUDE)

#wf.sf <- st_as_sf(wf.incidents,
#                  coords = c("POO_LONGITUDE", "POO_LATITUDE"),
#                  crs = 4326)

# Land Use

land.use <- rast("/Users/kathrynmurenbeeld/Analysis/assignment-7-combiningdata-KatieMurenbeeld/data/usa_land_cover_2020_30m_tif/USA_NALCMS_landcover_2020_30m/data/USA_NALCMS_landcover_2020_30m.tif") 

# Vegetation departure index (landfire 2020)
# downloaded from landfire.gov/viewer 13 Nov 2023 see LC20_VDep_220.GeoJSON file 

#vdep <- rast("data/original/LC20_VDep_220.tif") # may still be too large? >2 GB

#tmp <- tempfile()
#vdep.url <- "https://www.landfire.gov/bulk/downloadfile.php?FNAME=US_220_mosaic-LF2020_VDep_220_CONUS.zip&TYPE=landfire"

# Load Election Context 2018
# This is a csv of county data and will need to be joined to county data

elect.cntx <- read_csv("data/original/election-context-2018.csv") 


