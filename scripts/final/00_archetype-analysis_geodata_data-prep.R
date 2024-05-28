## Download and process raster data of temperature, precipitation, and soil moisture
## Download elevation data and calculate a topographic complexity index

#worldclim_country(country, var, path, version="2.1", ...)

library(raster)
library(sp)
library(geodata)
library(terra)

# Steps:
# 1. Download the rasters (vars = "bio")
# 2. Aggregate or disaggregate to 1.5km and 3km resolution
# 3. Crop to contiguous US
# 4. Set to analysis projection


r <- geodata::worldclim_country(country = "US", var = "bio", res = 10, path = here::here("data/original/"))
r2 <- geodata::elevation_30s(country = "US", path = here::here("data/original/"))
ru <- geodata::travel_time(to = "city", size = 7, up = TRUE, path = here::here("data/original/"))

# Get the mean annual temperature, temperature seasonality, precipitation, and precipitation seasonality 

temp_prec <- r[[c(1, 4, 12, 15)]]
names(temp_prec) <- c("Temp", "Seas_T", "Prec", "Seas_P")

plot(r_sub$Temp)
