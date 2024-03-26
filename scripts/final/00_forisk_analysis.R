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
library(spdep)

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
counties$mill_count <- lengths(st_intersects(counties, conus_mills))

# 3. Rasterize the data

ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
mill_proj <- conus_mills %>% st_transform(., crs = crs(ref_rast))
counties_proj <- counties %>% st_transform(., crs = crs(ref_rast))

# Rasterize shapefiles with variables of interest

mill_proj$PRESENT <- 1
mill_pres_rast <- rasterize(vect(mill_proj), ref_rast, field = "PRESENT", fun = "mean")
tot_wood_rast <- rasterize(vect(mill_proj), ref_rast, field = "Total_Wood", fun = "sum")
status_rast <- rasterize(vect(mill_proj), ref_rast, field = "Status")
mill_ct_rast <- rasterize(vect(counties_proj), ref_rast, field = "mill_count", fun = "sum")

# 3.5 Write rasters

#writeRaster(tot_wood_rast, here::here("data/processed/tot_wood_cap.tif"), overwrite = TRUE)
#writeRaster(status_rast, here::here("data/processed/mill_status.tif"), overwrite = TRUE)
#writeRaster(mill_ct_rast, here::here("data/processed/mill_count.tif"), overwrite = TRUE)
#writeRaster(mill_pres_rast, here::here("data/processed/mill_present.tif"), overwrite = TRUE)

fw <- focalMat(tot_wood_rast, 3000, "circle") 
fw[fw > 0] <- 1 

test_heat <- focal(tot_wood_rast, fw, fun = mean, na.rm=T)
plot(test_heat)
plot(ref_rast)

d2 <- distance(ref_rast, vect(mill_proj)) 
plot(d2)

# From https://michaelminn.net/tutorials/r-point-analysis/

mill_proj$PRESENT <- 1
getisraster <- rasterize(vect(mill_proj), ref_rast, field = 'PRESENT', fun = "sum")
getisraster[is.na(getisraster[])] <- 0 
getisgrid <- as.polygons(getisraster, round = FALSE, aggregate = FALSE)
#getisgrid2 <- raster::rasterToPolygons(getisraster)
getisgrid_casted <- st_as_sf(getisgrid, "POLYGON") 
# Create the list of neighbors

neighbors = poly2nb(getisgrid_casted)
weighted_neighbors = nb2listw(neighbors, zero.policy=T)

#geog.nearnb <- knn2nb(knearneigh(mill_proj, k = 1), sym=TRUE); #estimate distance to first neareset neighbor
#nb.nearest <- dnearneigh(mill_proj, 0,  max( unlist(nbdists(geog.nearnb, mill_proj))))
#weighted_neighbors <- nb2listw(nb.nearest, zero.policy=T)

# Perform the local G analysis (Getis-Ord GI*)

getisgrid_casted$HOTSPOT <- as.vector(localG(getisgrid_casted$sum, weighted_neighbors))
breaks = c(-50, -1.96, -1, 1, 1.96, 50)
palette=c("#0000FF80", "#8080FF80", "#FFFFFF80", "#FF808080", "#FF000080")
col = palette[cut(getisgrid_casted$HOTSPOT, breaks)]

plot(getisgrid_casted, col = col)

plot(counties$geometry)




