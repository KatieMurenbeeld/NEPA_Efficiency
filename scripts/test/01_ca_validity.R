## Check the validity of the geometries
library(tidyverse)
library(sf)
library(terra)


### will also combine the election context data to county data

# Load the polygon data

## Forest Service and Wish and Wildlife from the .zip files in 00_ca_download_data.R
fs.nf.bdry <- st_read("data/original/S_USA.AdministrativeForest.shp")
fs.rg.bdry <- st_read("data/original/S_USA.AdministrativeRegion.shp")
#fws.te.bdry <- st_read("data/original/") not sure where this goes??

## Wildfire Incidents

wf.incidents <- read_csv(file = "/Users/kathrynmurenbeeld/Analysis/assignment-7-combiningdata-KatieMurenbeeld/data/ics209plus-wildfire/ics209-plus-wf_incidents_1999to2020.csv",
                         col_names = TRUE, col_types = NULL,
                         na = c("", "NA")) %>%
  drop_na(POO_LONGITUDE, POO_LATITUDE)

wf.sf <- st_as_sf(wf.incidents,
                  coords = c("POO_LONGITUDE", "POO_LATITUDE"),
                  crs = 4326)

# Land Use

land.use <- rast("/Users/kathrynmurenbeeld/Analysis/assignment-7-combiningdata-KatieMurenbeeld/data/usa_land_cover_2020_30m_tif/USA_NALCMS_landcover_2020_30m/data/USA_NALCMS_landcover_2020_30m.tif") 


# Load Election Context 2018
# This is a csv of county data and will need to be joined to county data

elect.cntx <- read_csv("data/original/election-context-2018.csv") 

# Load US counties using tigris (only for states in Forest Region 1)

counties <- counties(state = c("MT", "ID", "SD", "WY"))

## Check the geometry validity

all(st_is_valid(fs.nf.bdry))
all(st_is_valid(fs.rg.bdry))
all(st_is_valid(fws.te.bdry))
all(st_is_valid(counties))

## Only the National Forest boundaries need to be fixed

fs.nf.valid <- st_make_valid(fs.nf.bdry)
all(st_is_valid(fs.nf.valid))

## Check raster alignment?

## Join election context to counties

elect.cntx$fips <- as.character(elect.cntx$fips)

elect.cntx.bdry <- left_join(counties, elect.cntx, 
                         by = c("GEOID" = "fips"))



