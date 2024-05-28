## Download and process raster data of temperature, precipitation, and soil moisture
## Download elevation data and calculate a topographic complexity index

# 0. Load libraries and set projection
library(sf)
library(raster)
library(sp)
library(geodata)
library(terra)
library(tigris)
library(dplyr)

proj <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83"

# Steps:
# 0. Load libraries and set projection
# 1. Download the rasters (vars = "bio")
# 3. Aggregate or disaggregate to 1.5km and 3km resolution
# 2. Crop to contiguous US
# 4. Set to analysis projection

# 1. Download the data using the geodata package

r_prec <- geodata::worldclim_country(country = "USA", var = "prec", res = 0.5, path = here::here("data/original/"))
r_temp <- geodata::worldclim_country(country = "USA", var = "tavg", res = 0.5, path = here::here("data/original/"))
r_bio <- geodata::worldclim_country(country = "USA", var = "bio", res = 0.5, path = here::here("data/original/"))
r_ele <- geodata::elevation_30s(country = "US", path = here::here("data/original/"))
r_tt <- geodata::travel_time(to = "city", size = 7, up = TRUE, path = here::here("data/original/"))

## Get the mean annual temperature, temperature seasonality, precipitation, and precipitation seasonality 
temp_prec <- r[[c(1, 4, 12, 15)]]
names(temp_prec) <- c("Temp", "Seas_T", "Prec", "Seas_P")

# 2. Crop to CONUS
## Using tigris, download the state boundaries
states <- tigris::states(cb = TRUE)

## Filter out Alaska, Hawaii, DC, and territories
states <- states %>%
  filter(STUSPS != "AK" & STUSPS != "HI" & STUSPS != "DC") %>%
  filter(GEOID < 60)

## Set the crs of r_tt to states
crs(r_ele) <- proj
crs(r_tt) <- proj
states <- st_transform(states, proj)

r_tt_conus <- crop(r_tt, states, mask = TRUE)

# 3. Resample to 1.5km and 3km resolution


