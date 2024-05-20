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

#----Testing out calculating distance to mills in Idaho
# Pseudocode:
# 1. Load the data (FORISK and county boundaries)
# 2. Align the data
# 3. Filter for Idaho
# 4. Calculate distance from mill points

# 1. Load the data
mill_sf <- read_sf(here::here("data/original/2024_Q1_Forisk_North_American_Ind_Cap_DB_Shape/Forisk_NA_FI_Capacity_DB_2024_Q1.shp"))

id_counties <- tigris::counties(state = "ID")
test_urban <- tigris::urban_areas(year = 2020, criteria = "2020")
ggplot() +
  geom_sf(data = test_urban)

test_ua <- read_sf(here::here("data/original/tl_2020_us_uac20/tl_2020_us_uac20.shp"))
urban_names <- read_csv(here::here("data/original/ua_list_all.csv"))

test_urb_join <- left_join(test_ua, urban_names, by = c("UACE20" = "UACE"))

ggplot() + 
  geom_sf(data = test_ua)

# 2. Align the data
st_crs(mill_sf)
st_crs(id_counties)

identical(st_crs(mill_sf), st_crs(id_counties))

# 3. Filter FORISK data for Idaho mills
mill_id <- mill_sf %>%
  filter(State_Prov == "ID")

# 3.1 Filter for open mills 
mill_id <- mill_id %>%
  filter(Status == "Open")

# 4. Calculate distance to mills

nearest_mill <- st_nearest_feature(id_counties, mill_id)

nearest_mill_sf <- mill_id[nearest_mill,]
mill_dist <- st_distance(id_counties, nearest_mill_sf, by_element = TRUE)
mill_dist <- set_units(mill_dist, "km")
county_mill_dist <- id_counties %>%
  mutate(., distmill = drop_units(mill_dist))

# 5. Plot the results!

ggplot() +
  geom_sf(data = county_mill_dist, mapping = aes(fill = distmill)) +
  geom_sf(data = mill_id, mapping = aes(color = Type))
 
#----Testing out calculating distance to mills in CONUS

# 1. Load the FORISK data and Counties boundaries

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
 
# 2. Align the data
st_crs(conus_mills)
st_crs(counties)

identical(st_crs(conus_mills), st_crs(counties))

# 4. Calculate distance to mills

nearest_mill_conus <- st_nearest_feature(counties, conus_mills)

nearest_mill_conus_sf <- conus_mills[nearest_mill_conus,]
mill_dist_conus <- st_distance(counties, nearest_mill_conus_sf, by_element = TRUE)
mill_dist_conus <- set_units(mill_dist_conus, "km")
county_mill_dist_conus <- counties %>%
  mutate(., distmill = drop_units(mill_dist_conus))

# 4.1 Calculate the change in mill capacity from 2019-2023 (5 years)

# Need to figure out a way to deal with mills that came online from 2019-2023

conus_mills$CPT_2019[conus_mills$CPT_2019 == 0.0] <- 1.0 

conus_mills <- conus_mills %>%
  mutate(millcap_5yr = (CPT_2024 - CPT_2019) / CPT_2019)

### Make categorical?
conus_mills <- conus_mills %>%
  mutate(change_millcap = case_when(millcap_5yr < 0.0 ~ "decrease",
                                 millcap_5yr == 0.0 ~ "no change",
                                 millcap_5yr > 0.0 & millcap_5yr < Inf ~ "increase",
                                 millcap_5yr == Inf ~"new mill"))

### Have it range from -1 to 1?
conus_mills <- conus_mills %>%
  mutate(change_millcap02 = case_when(millcap_5yr < 0.0 ~ -1.0,
                                    millcap_5yr == 0.0 ~ 0.0,
                                    millcap_5yr > 0.0 ~ 1.0))

### Have it range from 0 to 1?
conus_mills <- conus_mills %>%
  mutate(change_millcap03 = case_when(millcap_5yr < 0.0 ~ 0.0,
                                      millcap_5yr == 0.0 ~ 0.5,
                                      millcap_5yr > 0.0 ~ 1.0))

conus_mills <- conus_mills %>%
  select(CPT_2019, CPT_2024, Current_Ca, Total_Wood, millcap_5yr, 
         change_millcap, change_millcap02, change_millcap03, geometry)

conus_mills$millcap_5yr[conus_mills$millcap_5yr == Inf] <- 10

# Check for missing data
print ("Row and Col positions of NA values") 
which(is.na(conus_mills), arr.ind = TRUE)

conus_mills <- conus_mills[-c(1228), ]

print ("Row and Col positions of NA values") 
which(is.na(conus_mills), arr.ind = TRUE)

## Interpolate del mill capacity
nodes <- st_make_grid(counties,
                      n = c(50,50),
                      what = "centers")
dist <- distance(vect(nodes), vect(conus_mills))
nearest_conus <- apply(dist, 1, function(x) which(x == min(x)))
nearest_conus1 <- as.data.frame(nearest_conus[1:2500])
nearest_conus2 <- nearest_conus1 %>% filter(row_number() == 2)
#nearest_conus3 <- as.array(nearest_conus2)
nearest <- as.numeric(as.vector(nearest_conus2[1,]))
millcap5.nn <- conus_mills$millcap_5yr[nearest]
delmill5.nn <- conus_mills$change_millcap02[nearest]
currcap.nn <- conus_mills$Current_Ca[nearest]
totwood.nn <- conus_mills$Total_Wood[nearest]
preds <- st_as_sf(nodes)
preds$millcap5 <- millcap5.nn
preds$currcap <- currcap.nn
preds$totwood <- totwood.nn
preds$delmill <- delmill5.nn
preds <- as(preds, "Spatial")
sp::gridded(preds) <- TRUE
preds.rast <- rast(preds)

mc5sf05 <- gstat(id = "millcap_5yr", formula = millcap_5yr~1, data=conus_mills,  nmax=7, set=list(idp = 0.5))
mc5sf1 <- gstat(id = "millcap_5yr", formula = millcap_5yr~1, data=conus_mills,  nmax=7, set=list(idp = 1))
mc5sf2 <- gstat(id = "millcap_5yr", formula = millcap_5yr~1, data=conus_mills,  nmax=7, set=list(idp = 2))

test05 <- gstat(id = "change_millcap02", formula = change_millcap02~1, data=conus_mills,  nmax=7, set=list(idp = 0.5))
test2 <- gstat(id = "change_millcap02", formula = change_millcap02~1, data=conus_mills,  nmax=7, set=list(idp = 2))

ccsf05 <- gstat(id = "Current_Ca", formula = Current_Ca~1, data=conus_mills,  nmax=7, set=list(idp = 0.5))
ccsf2 <- gstat(id = "Current_Ca", formula = Current_Ca~1, data=conus_mills,  nmax=7, set=list(idp = 2))
twsf05 <- gstat(id = "Total_Wood", formula = Total_Wood~1, data=conus_mills,  nmax=7, set=list(idp = 0.5))
twsf2 <- gstat(id = "Total_Wood", formula = Total_Wood~1, data=conus_mills,  nmax=7, set=list(idp = 2))

interpolate_gstat <- function(model, x, crs, ...) {
  v <- st_as_sf(x, coords=c("x", "y"), crs=crs)
  p <- predict(model, v, ...)
  as.data.frame(p)[,1:2]
}

zmc5sf05 <- interpolate(preds.rast, mc5sf05, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)
zmc5sf1 <- interpolate(preds.rast, mc5sf1, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)
zmc5sf2 <- interpolate(preds.rast, mc5sf2, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)

zmc5sf05_crop <- crop(zmc5sf05, counties, mask = TRUE)
zmc5sf1_crop <- crop(zmc5sf1, counties, mask = TRUE)
zmc5sf2_crop <- crop(zmc5sf2, counties, mask = TRUE)

ztest05 <- interpolate(preds.rast, test05, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)
ztest2 <- interpolate(preds.rast, test2, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)

ztest05_crop <- crop(ztest05, counties, mask = TRUE)
ztest2_crop <- crop(ztest2, counties, mask = TRUE)

zccsf05 <- interpolate(preds.rast, ccsf05, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)
zccsf2 <- interpolate(preds.rast, ccsf2, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)

zccsf05_crop <- crop(zccsf05, counties, mask = TRUE)
zccsf2_crop <- crop(zccsf2, counties, mask = TRUE)

ztwsf05 <- interpolate(preds.rast, twsf05, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)
ztwsf2 <- interpolate(preds.rast, twsf2, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)

ztwsf05_crop <- crop(ztwsf05, counties, mask = TRUE)
ztwsf2_crop <- crop(ztwsf2, counties, mask = TRUE)

ggplot(data = test, mapping = aes(x = x, y = y, fill = Current_Ca.pred)) + 
  geom_raster()

# 5. Plot the results!
# transform to log scale
# for log scale set breaks

county_mill_dist_conus$distmill_log <- log(county_mill_dist_conus$distmill + 1, base = 10)

my_breaks <- c(0, 1, 2)

mill_dist_map <- ggplot() +
  geom_sf(data = county_mill_dist_conus, mapping = aes(fill = distmill_log)) +
  geom_sf(data = conus_mills, mapping = aes(color = Type)) + 
  labs(fill = "Distance to Mill, km", color = "Mill Type") +
  scale_fill_gradient(name = "Distance to Mill, (log scale)",
                      breaks = my_breaks, labels = my_breaks) +
  theme_bw() + 
  theme()

ggsave(here::here("figures/mill_distance_conus_map_log_trans.png"), 
       mill_dist_map,
       width = 1920/72, 
       height = 1080/72, 
       dpi = 300) 

## Write county_mill_dist_conus as shapefile
st_write(obj = county_mill_dist_conus, dsn = here::here("data/processed/county_mill_dist.shp"))

## Try out an interpolation of mill capacity for Idaho

# Calculate the change in capacity between 2019 and 2024

mill_id <- mill_id %>%
  mutate(millcap_5yr = CPT_2024 - CPT_2019)

ggplot() +
  geom_sf(data = id_counties) + 
  geom_sf(data = mill_id, mapping = aes(color = Current_Ca))

ggplot() +
  geom_sf(data = id_counties) + 
  geom_sf(data = mill_id, mapping = aes(color = millcap_5yr))

## See https://isdrfall23.classes.spaseslab.com/content/19-content.html

nodes <- st_make_grid(mill_id,
                      what = "centers")
dist <- distance(vect(nodes), vect(mill_id))
nearest <- apply(dist, 1, function(x) which(x == min(x)))
mill.nn <- mill_id$Current_Ca[nearest]
preds <- st_as_sf(nodes)
preds$cap <- mill.nn

preds <- as(preds, "Spatial")
sp::gridded(preds) <- TRUE
preds.rast <- rast(preds)

mgsf05 <- gstat(id = "Current_Ca", formula = Current_Ca~1, data=mill_id,  nmax=7, set=list(idp = 0.5))
mgsf2 <- gstat(id = "Current_Ca", formula = Current_Ca~1, data=mill_id,  nmax=7, set=list(idp = 2))
interpolate_gstat <- function(model, x, crs, ...) {
  v <- st_as_sf(x, coords=c("x", "y"), crs=crs)
  p <- predict(model, v, ...)
  as.data.frame(p)[,1:2]
}
zsf05 <- interpolate(preds.rast, mgsf05, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)
zsf2 <- interpolate(preds.rast, mgsf2, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)

zsf05_crop <- crop(zsf05, id_counties, mask = TRUE)
zsf2_crop <- crop(zsf2, id_counties, mask = TRUE)

ggplot(data = zsf2_crop) + 
  geom_raster(mapping = aes(x = x, y = y), fill = Current_Ca.pred)

# Define the neighbors (use queen case)
nb <- poly2nb(id_counties, queen=TRUE)

# Compute the neighboring average homicide rates
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
#estimate Moran's I
moran.test(mill_id$Current_Ca,lw, alternative="greater")

#-------
grd <- st_bbox(id_counties) %>%
  st_as_stars(dx = 100) %>%
  st_crop(id_counties)

grd <- as.data.frame(spsample(as(id_counties, "Spatial"), "regular", n=20000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(as(mill_id, "Spatial"))

f.1  <- as.formula(meanpm25 ~ X + Y)

i <- idw(Current_Ca~1, mill_id, grd)

v <- variogram(Current_Ca~1, mill_id)
plot(v, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(v$dist)))

v0 <- variogram(Current_Ca~1, mill_id, cutoff = 150, width = 15)
plot(v0, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(v0$dist)))

v.m <- fit.variogram(v, vgm("Exp"))



### Look at Padus data

padus1 <- read_sf(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"))

ggplot() +
  geom_sf(data = padus)

library(FedData)

PADUS <- get_padus(
  template = FedData::meve,
  label = "fed",
  layer = "Federal_Management_Agencies" 
)
PADUS

padus2 <- st_read(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"))


padus_layers <- st_layers(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"))

fed <- st_read(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"), layer = "PADUS4_0Designation")      

conus_fed <- fed %>%
  filter(State_Nm %in% continental.states$state) %>%
  filter(Mang_Type == "FED")

identical(st_crs(conus_fed), st_crs(counties))

conus_fedp <- st_transform(conus_fed, st_crs(counties))
identical(st_crs(conus_fedp), st_crs(counties))

# Make the multisurface into multipolygons
conus_fedp <- conus_fedp %>% st_cast("MULTIPOLYGON")

# Check and fix validity
st_is_valid(conus_fedp)
st_make_valid(conus_fedp)
#geoms <- lapply(conus_fedp.split$geometry, `[` )
#mp <- lapply( geoms, function(x) sf::st_multipolygon( x = x ) )

#system('ogr2ogr  /home/x/Downloads/output.gpkg /home/x/Downloads/gc_cast.gpkg -explodecollections -nlt CONVERT_TO_LINEAR')

ggplot() +
  geom_sf(data = conus_fed, mapping = aes(fill = Mang_Name, color = Mang_Name))




