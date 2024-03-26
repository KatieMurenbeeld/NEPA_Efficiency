library(tidyverse)
library(terra)
library(sf)
library(readr)
library(ggplot2)
library(ggmap)
library(tigris)
library(tmap)
library(patchwork)
library(units)

#----Distance to mills and Mill Capacity "hotspots"
# Pseudocode:
# 1. Load the data (FORISK and county boundaries)
# 2. Align the data
# 3. Rasterize data
# 4. Aggregate to 1.5km and 3km
# 5. Calculate distance from mill points
# 6. Mill capacity metric

# 1. Load the data
mill_sf <- read_sf(here::here("data/original/2024_Q1_Forisk_North_American_Ind_Cap_DB_Shape/Forisk_NA_FI_Capacity_DB_2024_Q1.shp"))





##Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name))
colnames(us.states) <- c("state","STATENAME")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS

counties <- tigris::counties(state = continental.states$state, cb = TRUE)

conus_mills <- mill_sf %>%
  filter(Region != "Canada West") %>%
  filter(Region != "Canada East") %>%
  filter(State_Prov %in% continental.states$state)

# What is the distribution of total wood capacity?

hist(conus_mills$Total_Wood, main = "Distribution of Total Wood Capacity", xlab = "Total Wood Capacity", ylab = "Frequency")

# 2. Align the data
st_crs(conus_mills)
st_crs(counties)

identical(st_crs(conus_mills), st_crs(counties))

#mill_county <- left_join()

# 3. Rasterize the data

ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))

mill_proj <- conus_mills %>% st_transform(., crs = crs(ref_rast))

# Rasterize shapefiles with variables of interest
tot_wood_rast <- rasterize(vect(mill_proj), ref_rast, field = "Total_Wood")
status_rast <- rasterize(vect(mill_proj), ref_rast, field = "Status")

# 3.5 Write rasters

#writeRaster(tot_wood_rast, here::here("data/processed/tot_wood_cap.tif"), overwrite = TRUE)
#writeRaster(status_rast, here::here("data/processed/mill_status.tif"), overwrite = TRUE)

test_rast <- points_to_raster(mill_proj, nrow = 966, ncol = 1539, by = "Total_Wood", to.Raster = TRUE)

r <- rasterize(vect(mill_proj), ref_rast, 'Total_Wood', fun = "sum")
r2 <- rasterize(vect(mill_proj), ref_rast, 'Total_Wood', fun=length(x))
r3 <- rasterizeWin(vect(mill_proj), ref_rast, 'Total_Wood', win = "circle", fun = "max", pars = 30000)
plot(r)
plot(r3)

fw <- focalMat(r, 3000, "circle") #The equivalent Terra function is focalMat
fw[fw > 0] <- 1 

test_heat <- focal(r, fw, fun = mean, na.rm=T)
plot(test_heat)
plot(ref_rast)

d2 <- distance(ref_rast, vect(mill_proj)) 
plot(d2)









