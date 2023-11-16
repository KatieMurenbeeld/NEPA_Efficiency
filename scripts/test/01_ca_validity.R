## Check the validity of the geometries
library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(tmap)


### will also combine the election context data to county data

# Load the polygon data

## Forest Service and Wish and Wildlife from the .zip files in 00_ca_download_data.R
fs.nf.bdry <- st_read("data/original/S_USA.AdministrativeForest.shp")
fs.rg.bdry <- st_read("data/original/S_USA.AdministrativeRegion.shp")
fws.te.bdry <- st_read("data/original/crithab_poly.shp")
cejst.bdry <- st_read("data/original/usa.shp")

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

counties <- counties(state = c("MT", "ID", "ND", "SD", "WY"))

## Check the geometry validity

all(st_is_valid(fs.nf.bdry))
all(st_is_valid(fs.rg.bdry))
all(st_is_valid(fws.te.bdry))
all(st_is_valid(cejst.bdry))
all(st_is_valid(counties))

## The National Forest and the critical habitat boundaries need to be fixed

fs.nf.valid <- st_make_valid(fs.nf.bdry)
all(st_is_valid(fs.nf.valid))


## Check raster alignment?

## Join election context to counties

elect.cntx$fips <- as.character(elect.cntx$fips)

elect.cntx.bdry <- left_join(counties, elect.cntx, 
                         by = c("GEOID" = "fips"))

### Check the geometry of the joined shp
all(st_is_valid(elect.cntx.bdry))

## Set the projection for the layers

fs.nf.proj <- fs.nf.valid %>% st_transform(., crs=crs(land.use))
fs.rg.proj <- fs.rg.bdry %>% st_transform(., crs=crs(land.use))
fws.te.proj <- fws.te.bdry %>% st_transform(., crs=crs(land.use))
wf.sf.proj <- wf.sf %>% st_transform(., crs=crs(land.use))
elect.cntx.proj <- elect.cntx.bdry %>% st_transform(., crs=crs(land.use))

## Subset the data sets to the FS Region boundary for Region 1

### Filter the Region shp
fs.rg1.proj <- fs.rg.proj %>%
  filter(REGION == "01")

### Subset the other datasets to fs.rg1.proj
fs.subset <- fs.nf.proj[fs.rg1.proj, ]
fws.subset <- fws.te.proj[fs.rg1.proj, ]
wf.subset <- wf.sf.proj[fs.rg1.proj, ]
elect.subset <- elect.cntx.proj[fs.rg1.proj, ]
elect.subset <- elect.subset %>% 
  select()

### Use terra::crop() and/or mask() to clip land.use raster to FS Region 1 extent 
landuse.subset <- crop(x = land.use, y = vect(fs.rg1.proj), snap = "near", mask = TRUE)

### Write the subset geometries and raster to data/processed
st_write(obj = fs.subset, dsn = "data/processed/fs_subset.shp")
st_write(obj = fws.subset, dsn = "data/processed/fws_subset.shp")
st_write(obj = wf.subset, dsn = "data/processed/wf_subset.shp")
st_write(obj = elect.subset, dsn = "data/processed/elect_subset.shp")
writeRaster(x = landuse.subset, filename = "data/processed/landuse_subset.tif")

  


