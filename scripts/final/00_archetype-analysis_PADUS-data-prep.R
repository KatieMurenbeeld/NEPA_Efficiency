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

## Load the PADUS4_0Designation layer of the PADUS geodatabase and filter for by states

### Check the layers of the geodatabase, here choose the PADUS4_0Fee layer
padus_layers <- st_layers(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"))

fed <- st_read(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"), layer = "PADUS4_0Fee")      

conus_fed <- fed %>%
  filter(State_Nm %in% continental.states$state) %>%
  filter(Mang_Type == "FED")

# 3. Set the projection and check for shape validity
conus_fedp <- st_transform(conus_fed, st_crs(counties))
identical(st_crs(conus_fedp), st_crs(counties))

## Make the multisurface into multipolygons
conus_fedp <- conus_fedp %>% st_cast("MULTIPOLYGON")

## Check and fix validity
st_is_valid(conus_fedp)
conus_fedp_val <- st_make_valid(conus_fedp)
st_is_valid(conus_fedp_val)

### Look at a quick map
ggplot() +
  geom_sf(data = conus_fedp_val, mapping = aes(fill = Mang_Name, color = Mang_Name))

## Rasterize the data
### Load the reference raster from fire-data-prep
ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))

### Set to the same projection
conus_fedp_val_p <- st_transform(conus_fedp_val, crs(ref_rast))

### Rasterize the data
fed_rast <- rasterize(vect(conus_fedp_val_p), ref_rast, field = "Mang_Name")

### Having a hard time rasterizing the data...may want to just pick out BLM, USFS and NPS?

