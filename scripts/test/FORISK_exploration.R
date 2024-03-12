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


#----Testing out calculating distance to mills in Idaho
# Pseudocode:
# 1. Load the data (FORISK and county boundaries)
# 2. Align the data
# 3. Filter for Idaho
# 4. Calculate distance from mill points

# 1. Load the data
mill_sf <- read_sf(here::here("data/original/2024_Q1_Forisk_North_American_Ind_Cap_DB_Shape/Forisk_NA_FI_Capacity_DB_2024_Q1.shp"))

id_counties <- tigris::counties(state = "ID")

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

identical(st_crs(mill_sf), st_crs(id_counties))

# 4. Calculate distance to mills

nearest_mill_conus <- st_nearest_feature(counties, conus_mills)

nearest_mill_conus_sf <- conus_mills[nearest_mill_conus,]
mill_dist_conus <- st_distance(counties, nearest_mill_conus_sf, by_element = TRUE)
mill_dist_conus <- set_units(mill_dist_conus, "km")
county_mill_dist_conus <- counties %>%
  mutate(., distmill = drop_units(mill_dist_conus))

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




