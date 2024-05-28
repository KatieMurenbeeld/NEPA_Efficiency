## Download and process raster data of temperature, precipitation, and soil moisture
## Download elevation data and calculate a topographic complexity index

# 0. Load libraries and set projection
library(sf)
library(raster)
library(sp)
library(geodata) # this package removed from CRAN May 27, 2024...
library(terra)
library(tigris)
library(dplyr)

#proj <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83"

# Steps:
# 0. Load libraries and set projection
# 1. Download the rasters (vars = "bio")
# 3. Aggregate or disaggregate to 1.5km and 3km resolution
# 2. Crop to contiguous US
# 4. Set to analysis projection

# 1. Download the data using the geodata package

#r_prec <- geodata::worldclim_country(country = "USA", var = "prec", res = 0.5, path = here::here("data/original/"))
r_prec <- rast(here::here("data/original/wc2.1_country/USA_wc2.1_30s_prec.tif"))
#r_temp <- geodata::worldclim_country(country = "USA", var = "tavg", res = 0.5, path = here::here("data/original/"))
r_temp <- rast(here::here("data/original/wc2.1_country/USA_wc2.1_30s_tavg.tif"))
#r_bio <- geodata::worldclim_country(country = "USA", var = "bio", res = 0.5, path = here::here("data/original/"))
#r_bio <- rast(here::here("data/original/wc2.1_country/USA_wc2.1_30s_bio.tif"))
r_ele <- geodata::elevation_30s(country = "US", path = here::here("data/original/"))
r_tt <- geodata::travel_time(to = "city", size = 7, up = TRUE, path = here::here("data/original/"))

## Get the mean annual temperature, temperature seasonality, precipitation, and precipitation seasonality 
#temp_prec <- r_bio[[c(1, 4, 12, 15)]]
#names(temp_prec) <- c("Temp", "Seas_T", "Prec", "Seas_P")

# 2. Crop to CONUS
## Using tigris, download the state boundaries
states <- tigris::states(cb = TRUE)

## Filter out Alaska, Hawaii, DC, and territories
states <- states %>%
  filter(STUSPS != "AK" & STUSPS != "HI" & STUSPS != "DC") %>%
  filter(GEOID < 60)

## First crop to the extents
r_prec_crop <- crop(r_prec, ext(states))
r_temp_crop <- crop(r_temp, ext(states))
r_ele_crop <- crop(r_ele, ext(states))
r_tt_crop <- crop(r_tt, ext(states))

## Then set the projection of the rasters and recrop with mask = TRUE
test <- project(r_prec_crop, states)
test_conus <- crop(test, states, mask = TRUE)

## Function to crop to states extents, then reproject and crop again with mask = TRUE

crop_project <- function(raster, states){
  r_crop <- crop(raster, ext(states)) # First, crop to the extents of states
  tmp <- project(r_crop, states) # Then, reproject the raster to crs of states
  tmp_conus <- crop(tmp, states, mask = TRUE) # Finally, crop again with mask = TRUE
}

r_prec_conus <- crop_project(r_prec, states)
r_temp_conus <- crop_project(r_temp, states)
r_ele_conus <- crop_project(r_ele, states)
r_tt_conus <- crop_project(r_tt, states)


## Set the crs to the projection
#crs(r_prec) <- proj
#crs(r_temp) <- proj
#crs(r_ele) <- proj
#crs(r_tt) <- proj
#statesp <- st_transform(states, crs(r_tt))

r_prec_test <- project(r_prec_crop, states)
#states_test <- st_transform(states, crs(r_prec_test))
test_crop <- crop(r_prec_test, states, mask = TRUE)
# 3. Resample to 1.5km and 3km resolution

crs(r_prec_crop)

