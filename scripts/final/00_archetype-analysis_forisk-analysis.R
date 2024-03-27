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
library(spatstat)

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
  filter(State_Prov %in% continental.states$state) %>%
  filter(Status == "Open")

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
mill_counties_proj <- st_join(counties_proj, mill_proj) # make some county level metrics of capacity
mill_counties_proj$Total_Wood <- replace_na(mill_counties_proj$Total_Wood, 0)
  
test_cap_sum <- mill_counties_proj %>% 
  group_by(GEOID) %>%
  summarise(sum = sum(Total_Wood))
test_density <- density(mill_counties_proj$Total_Wood, bw=5)
summary(test_density)
plot(test_density, main = "Intensity")
cap_pp <- as.ppp(st_geometry(mill_proj))
plot(density(cap_pp, bw = 2))
plot(counties_proj$geometry, add = TRUE)

nb <- poly2nb(test_cap_sum, queen = TRUE) # queen shares point or border
nbw <- nb2listw(nb, style = "W", zero.policy = TRUE)

lmoran <- localmoran(test_cap_sum$sum, nbw, alternative = "two.sided")
head(lmoran)

test_cap_sum$lmp <- lmoran[, 5] # p-values are in column 5

mp <- moran.plot(as.vector(scale(test_cap_sum$sum)), nbw)
head(mp)

test_cap_sum$quadrant <- NA
# high-high
test_cap_sum[((mp$x) >= 0 & (mp$wx) >= 0) & ((test_cap_sum$lmp) <= 0.05), "quadrant"]<- 1
# low-low
test_cap_sum[((mp$x) <= 0 & (mp$wx) <= 0) & (-is.na(test_cap_sum$lmp) <= 0.05), "quadrant"]<- 2
# high-low
test_cap_sum[((mp$x) >= 0 & (mp$wx) <= 0) & (-is.na(test_cap_sum$lmp) <= 0.05), "quadrant"]<- 3
# low-high
test_cap_sum[((mp$x) <= 0 & (mp$wx) >= 0) & (-is.na(test_cap_sum$lmp) <= 0.05), "quadrant"]<- 4
# non-significant
test_cap_sum[(is.na(test_cap_sum$lmp) > 0.05), "quadrant"] <- 5

tm_shape(test_cap_sum) + tm_fill(col = "quadrant", title = "",
                        breaks = c(1, 2, 3, 4, 5, 6),
                        palette =  c("red", "blue", "lightpink", "skyblue2", "white"),
                        labels = c("High-High", "Low-Low", "High-Low",
                                   "Low-High", "Non-significant")) +
  tm_legend(text.size = 1)  + tm_borders(alpha = 0.5) +
  tm_layout(frame = FALSE,  title = "Clusters")  +
  tm_layout(legend.outside = TRUE)


mills <- ref_rast
XMIN <- ext(mills)$xmin
XMAX <- ext(mills)$xmax
YMIN <- ext(mills)$ymin
YMAX <- ext(mills)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(mills))

mlls <- rasterize(vect(mill_proj),templateRas)
#ext(mlls) <- ext(ref_rast)
mldist <- terra::distance(mlls)
plot(mldist)
points(mlls)

mldist_crop <- crop(mldist, ref_rast, mask = TRUE)
plot(mldist_crop)

# Classify the distances
distclasses <-matrix(c(1,100000,1, 
                       100000,250000,2, 
                       250000,500000,3,
                       500000, 1000000,4), 
                     ncol=3, 
                     byrow=TRUE)
distclass <- terra::classify(mldist_crop, rcl = distclasses, include.lowest = TRUE)
plot(distclass)

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
#writeRaster(mldist_crop, here::here("data/processed/mill_dist_open.tif"), overwrite = TRUE)

fw <- focalMat(tot_wood_rast, 50000, "circle") 
fw[fw > 0] <- 1 

test_heat <- focal(tot_wood_rast, fw, fun = mean, na.rm=T)
plot(test_heat)
plot(ref_rast)

# could do kernel density estimate of mills themselves instead of distance to mills
# distance to mills doesn't help distinguish between places near many mills or 1 mill
# or anything about mill capacity
# create sum of capacity in each county...

mill_dist <- distance(ref_rast, vect(mill_proj)) 
names(mill_dist) <- "distance_m"
#writeRaster(mill_dist), here::here("data/processed/dist_to_mill.tif"), overwrite = TRUE)
plot(mill_dist)

# From https://michaelminn.net/tutorials/r-point-analysis/

mill_proj$PRESENT <- 1
getisraster <- rasterize(vect(mill_proj), ref_rast, field = 'PRESENT', fun = "sum")
getisraster[is.na(getisraster[])] <- 0 
getisgrid <- as.polygons(getisraster, round = FALSE, aggregate = FALSE)
#getisgrid2 <- raster::rasterToPolygons(getisraster)
getisgrid_casted <- st_as_sf(getisgrid, "POLYGON") 
# Create the list of neighbors

neighbors = poly2nb(getisgrid_casted) #neighborhood is too small
weighted_neighbors = nb2listw(neighbors, zero.policy=T)

#geog.nearnb <- knn2nb(knearneigh(mill_proj, k = 1), sym=TRUE); #estimate distance to first neareset neighbor
#nb.nearest <- dnearneigh(mill_proj, 0,  max( unlist(nbdists(geog.nearnb, mill_proj))))
#weighted_neighbors <- nb2listw(nb.nearest, zero.policy=T)

# Perform the local G analysis (Getis-Ord GI*?)

getisgrid_casted$HOTSPOT <- as.vector(localG(getisgrid_casted$sum, weighted_neighbors))
#breaks = c(-50, -1.96, -1, 1, 1.96, 50)
#palette=c("#0000FF80", "#8080FF80", "#FFFFFF80", "#FF808080", "#FF000080")
#col = palette[cut(getisgrid_casted$HOTSPOT, breaks)]

#plot(getisgrid_casted, col = col)

hotspot <- rasterize(vect(getisgrid_casted), ref_rast, field = "HOTSPOT")
writeRaster(hotspot, here::here("data/processed/test_hotspot.tif"), overwrite = TRUE)

plot(hotspot, xlim=c(-2300000, -1400000), ylim=c(2500000, 3150000))
plot(counties_proj$geometry, border = "grey", add = TRUE)

plot(d2, xlim=c(-2500000, -1000000), ylim=c(1800000, 3150000))
plot(counties_proj$geometry, border = "grey", add = TRUE)
