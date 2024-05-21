library(tidyverse)
library(terra)
library(sf)
library(sp)
library(readr)
library(ggplot2)
library(ggmap)
library(tigris)
library(tmap)
library(patchwork)
library(units)
library(stars)
library(gstat)
library(spdep)
library(raster)


# 1. Download the PADUS geodatabase
## From this website https://www.sciencebase.gov/catalog/item/652ef930d34edd15305a9b03
## download the PADUS4_0Geodatabase.zip 
## The download requires one to pass a CAPTCHA challenge 

# 2. Load the data

## Get list of states in the contiguous US
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name))
colnames(us.states) <- c("state","STATENAME")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS

## Download the county boundaries and filter by states
counties <- tigris::counties(state = continental.states$state, cb = TRUE)

## Load the PADUS4_0Fee layer of the PADUS geodatabase and filter for by states

### Check the layers of the geodatabase, here choose the PADUS4_0Fee layer
padus_layers <- st_layers(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"))

fed <- st_read(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"), layer = "PADUS4_0Fee")      

conus_fed <- fed %>%
  filter(State_Nm %in% continental.states$state) %>%
  filter(Mang_Type == "FED")

### Once the data has been filtered, remove fed
rm(fed)

# 3. Set the projection and check for shape validity and empty geometries
conus_fedp <- st_transform(conus_fed, st_crs(counties))
identical(st_crs(conus_fedp), st_crs(counties))

## Make the multisurface into multipolygons
conus_fedp <- conus_fedp %>% st_cast("MULTIPOLYGON")

# Turn off using spherical geometry??
sf_use_s2(FALSE)
## Check and fix validity
all(st_is_valid(conus_fedp))
conus_fedp_val <- st_make_valid(conus_fedp)
all(st_is_valid(conus_fedp_val))


#conus_fedp_val_buff <- st_buffer(conus_fedp_val, dist = 0)


## Check for and remove empty geometries
all(st_is_empty(conus_fedp_val))
#conus_fedp_val_noempty = conus_fedp_val[!st_is_empty(conus_fedp_val),]
#st_is_empty(conus_fedp_val_noempty)



# 4. Rasterize the data
## Load the reference raster from fire-data-prep
ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))

## Set to the same projection
conus_fed_val_noempty_p <- st_transform(conus_fedp_val_noempty, crs(ref_rast))

## Rasterize the data and write the raster
fed_rast <- rasterize(vect(conus_fed_val_noempty_p), ref_rast, field = "Mang_Name")
#writeRaster(fed_rast, here::here("data/processed/fed_lands_3000m.tif"), overwrite = TRUE)

# 5. Calculate the area of overlapping Fed and county polygons (maybe I didn't need to figure out the rasterization yet...)
## from stack overflow https://gis.stackexchange.com/questions/362466/calculate-percentage-overlap-of-2-sets-of-polygons-in-r

## Calculate area and tidy up
intersect_pct <- st_intersection(counties, conus_fedp_val) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(GEOID, intersect_area) %>%   # only select columns needed to merge
  st_drop_geometry()  # drop geometry as we don't need it

# Create a fresh area variable for counties
counties <- mutate(counties, county_area = st_area(counties))

# Merge by county name
counties <- merge(counties, intersect_pct, by = "GEOID", all.x = TRUE)

# Calculate coverage
counties <- counties %>% 
  mutate(coverage = as.numeric(intersect_area/county_area))





