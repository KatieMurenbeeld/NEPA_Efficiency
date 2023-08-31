library(ggplot2)
library(ggmap)
library(gganimate)
library(gifski)
library(maps)
library(sf)
library(stringr)
library(dplyr)
library(tidyr)
library(transformr)

#library(tidyverse)
library(forcats)
library(survminer)
library(survival)
library(lubridate)
library(reshape2)
#library(ggplot2)
library(ggpubr)
library(fastDummies)
library(chron)
library(vtable)

## 00 Load Data

# Set working directory
setwd('/Users/kathrynmurenbeeld/Analysis/NEPA_Efficiency/')

# Join the X data to the shape file data for forests and regions
states <- st_read('data/original/cb_2018_us_state_5m/cb_2018_us_state_5m.shp')
counties <- st_read('data/original/S_USA.ALPGeopoliticalUnit/S_USA.ALPGeopoliticalUnit.shp')
forests <- st_read('data/original/S_USA.AdministrativeForest/S_USA.AdministrativeForest.shp')
budget <- read.csv('data/original/FY2013-2018_National_Forest_Allotment.csv')
pals <- read.csv('data/original/pals_ongoing_projects_11-2022.csv', sep = ";")

## 01 Process the Data
# Pals: convert date columns from characters to datetime?
# Pals: drop extraneous columns
# Pals: 

# To join I need to strip the leading 0 from FORESTORGC in the shapefile. 
# Change the numeric budget$Unit to a string
# Then I can left the budget data and the GIS data.


budget$Unit <- as.character(budget$Unit)
forests$FORESTORGC
forests$FORESTORGC <- str_remove(forests$FORESTORGC, "^0+")

joined_df <- left_join(forests, budget,
                       by = c("FORESTORGC" = "Unit"))

# Then I want to drop: Unit.Name, ADMINFORES, REGION, FORESTNUMB

joined_df <- select(joined_df, -c(Unit.Name, ADMINFORES, REGION, FORESTNUMB))

region1 <- joined_df %>%
  filter(Region == 1)
region4 <- joined_df %>%
  filter(Region == 4)

idaho <- states %>%
  filter(NAME == "Idaho")
pnw <- states %>%
  filter(NAME == "Idaho" | NAME =="Washington" | NAME =="Oregon" | NAME =="Montana" | NAME =="Nevada" | NAME =="Utah" | NAME =="California" | NAME =="Nevada" | NAME == "Colorado" | NAME == "Wyoming")
idaho_co <- counties %>%
  filter(STATENAME == "Idaho")

#id_basemap <- get_map(location=c(lon = -75.16522, lat = 39.95258), zoom=11, maptype = 'terrain-background', source = 'stamen')

ggplot() +
  geom_sf(data = idaho, size = 0.5) + 
  coord_sf()


test_region4 <- ggplot() + 
  #geom_sf(data = region4, size = 0.5, fill = "forestgreen") + 
  geom_sf(data = pnw, size = 0.5) + 
  geom_sf(data = region4, size = 0.5, aes(fill = X2013)) + 
  ggtitle("Test Forest Plot") + 
  coord_sf()
ggsave("test_pnw_2013.png", test_region4)
dev.off()

test_region4 <- ggplot() + 
  #geom_sf(data = region4, size = 0.5, fill = "forestgreen") + 
  geom_sf(data = pnw, size = 0.5) + 
  geom_sf(data = region4, size = 0.5, aes(fill = X2014)) + 
  ggtitle("Test Forest Plot") + 
  coord_sf()
ggsave("test_pnw_2014.png", test_region4)
dev.off()

# Now need to figure out animation. Probably need to reformat data so that year is a column?
# Need: Forest | Location Info | Budget | Year
# so each Forest should account for 6 rows?

region4_long <- region4 %>%
  pivot_longer(cols = c("X2013", "X2014", "X2015", "X2016", "X2017", "X2018"), names_to = "year", values_to = "budget")

region4_long$year<-gsub("X","",as.character(region4_long$year))
region4_long$year <- as.numeric(region4_long$year)

# Let's try animation!

map_with_data <- 
  ggplot() + 
  geom_sf(data = pnw, size = 0.5) + 
  geom_sf(data = region4_long, size = 0.5, aes(fill = budget)) + 
  ggtitle("Test Forest Plot") +  
  coord_sf()
ggsave("map_with_data.png", map_with_data)


map_with_animation <- map_with_data +
  transition_time(year) +
  ggtitle('Year: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')
num_years <- max(region4_long$year) - min(region4_long$year) + 1
animate(map_with_animation, nframes = num_years, fps = 2)
anim_save("region4_animated.gif", map_with_animation, fps =2, nframes = num_years)

## Need to figure out how to add a basemap. I want the state outlines. 
#map_with_data <- idaho_map +
#  geom_sf(data = region1, size = 3, fill = "green") + 
#  coord_sf(crs = st_crs(4269)) 

# If I leave coord_df() empty I still get the "Coordinate system already present" error.
# NAD83 has an EPSG code of 4269

# I think I will need to reformat the data so that year is a column like what I did in the jupyter notebook