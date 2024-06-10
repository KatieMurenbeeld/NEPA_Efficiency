library(tidyverse)
library(terra)
library(sf)
library(sp)
library(ggplot2)
library(tigris)
library(tmap)
library(raster)
library(units)


# 1. Download the PADUS geodatabase
## From this website https://www.sciencebase.gov/catalog/item/652ef930d34edd15305a9b03
## download the PADUS4_0Geodatabase.zip 
## The download requires one to pass a CAPTCHA challenge 

# 2. Load the data and filter for the contiguous US (CONUS)
projection = "epsg:5070"
## Get list of states in the CONUS
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name))
colnames(us.states) <- c("state","STATENAME")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS

## Download the county boundaries and filter by states
counties <- tigris::counties(state = continental.states$state, cb = TRUE)

## Load the PADUS4_0Fee layer of the PADUS geodatabase and filter for by states

### Check the layers of the geodatabase, here choose the PADUS4_0Fee layer
padus_layers <- st_layers(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"))

fed <- st_read(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"), layer = "PADUS4_0Fee")      

conus_fed <- fed %>%
  filter(State_Nm %in% continental.states$state) %>%
  filter(Mang_Type == "FED")

### Once the data has been filtered, remove fed
rm(fed)

# 3. Set the projection and check for shape validity and empty geometries
conus_fedp <- conus_fed %>% st_transform(., crs = projection)

## Make the multisurface into multipolygons
conus_fedp <- conus_fedp %>% st_cast("MULTIPOLYGON")

# Turn off using spherical geometry
sf_use_s2(FALSE)

## Check and fix validity
all(st_is_valid(conus_fedp))
conus_fedp_val <- st_make_valid(conus_fedp)
all(st_is_valid(conus_fedp_val))
#conus_fedp_val_buff <- st_buffer(conus_fedp_val, dist = 0)

## Check for and remove empty geometries
all(st_is_empty(conus_fedp_val))
#conus_fedp_val_noempty = conus_fedp_val[!st_is_empty(conus_fedp_val),]
#st_is_empty(conus_fedp_val_noempty)

# 4. -----
#----testing % area fed and % area fed type----
ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
ref_rast_proj <- project(ref_rast, projection)
counties_proj <- counties %>% st_transform(., crs = projection)

conus_cells <- st_make_grid(counties_proj, cellsize = 3000)

conus_cells <- st_sf(conus_cells) 

# add unique cell id
conus_cells <- conus_cells %>% 
  mutate(GRIDCELL_REFID = as.character(row_number()))

## Create a template raster for the shapefiles
XMIN <- ext(conus_cells)$xmin
XMAX <- ext(conus_cells)$xmax
YMIN <- ext(conus_cells)$ymin
YMAX <- ext(conus_cells)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
conus_cells_rst <- rast(ncol=NCOLS, nrow=NROWS, 
                        xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                        vals=1, crs=projection)


conus_fed_type <- conus_fedp_val %>%
  dplyr::select(Mang_Type)

conus_fed_type_proj <- conus_fed_type %>% st_transform(., crs = projection) #%>%
#st_cast(., to = "POLYGON")

# Replace NAs with 0
conus_fed_type_proj[is.na(conus_fed_type_proj)] <- 0
saveRDS(conus_fed_type_proj, here::here("data/processed/conus_fed_type_proj_na_0.rds"))

# for different Federal agencies
conus_fed_type_int <- st_intersection(conus_cells, conus_fed_type_proj)
saveRDS(conus_fed_type_int, here::here("data/processed/conus_fed_type_int.rds"))
conus_fed_type_areas <- conus_fed_type_int %>%
  mutate(area = st_area(.)) %>%
  mutate(percent_area = drop_units(area) / (3000*3000))
saveRDS(conus_fed_type_areas, here::here("data/processed/conus_fed_type_int_pctarea.rds"))

# Calculate the Shannon Evenness
conus_fed_type_areas_prop <- conus_fed_type_areas %>% 
  mutate(step1 = -percent_area * log(percent_area))
#id_fed_all_areas_prop$step1[is.na(id_fed_all_areas_prop$step1)] <- 0

conus_fed_rich <- conus_fed_type_areas %>%
  group_by(., GRIDCELL_REFID) %>%
  summarise(., numfed = n())

conus_shannon <- conus_fed_type_areas_prop %>% 
  group_by(., GRIDCELL_REFID) %>% 
  summarise(., numfed = n(), 
            H = sum(step1),
            fedarea = sum(unique(drop_units(area))),
            E = H/log(fedarea))

conus_shan_rich_rast <- rasterize(conus_shannon, conus_cells_rst, field = "numfed")
conus_shan_rich_rast[is.na(conus_shan_rich_rast)] <- 0
writeRaster(conus_shan_rich_rast, here::here("data/processed/conus_fed_rich_na0.tif"))
conus_shan_E_rast <- rasterize(conus_shannon, conus_cells_rst, field = "E")
writeRaster(conus_shan_E_rast, here::here("data/processed/conus_fed_E.tif"))

plot(conus_shan_rich_rast)
conus_shan_rich_rast
conus_shan_rich_rast[is.na(conus_shan_rich_rast)] <- 0
plot(conus_shan_rich_rast)

# Try out the % area governed by a Federal agency. I think the other project may be stuck
#conus_fed_name_proj <- conus_fed_name %>% st_transform(., crs = "epsg:5070")

# for all Federal agencies need to union first otherwise I got >1.00 percent_area
#conus_fed_union <- conus_fed_type_proj %>%
#  st_crop(., conus_cells) %>%
#  st_union(.)

#saveRDS(conus_fed_union, here::here("data/processed/conus_fed_union.rds"))
conus_fed_union <- readRDS("~/Analysis/Archetype_Analysis/data/processed/conus_fed_union.rds")

# intersection for all Fed fee lands
#d[is.na(d)] <- 0
#conus_fed_union[is.na(conus_fed_union)] <- 0
#saveRDS(conus_fed_union, here::here("data/processed/conus_fed_union_na_0.rds"))
conus_fed_union_na_0 <- readRDS("~/Analysis/Archetype_Analysis/data/processed/conus_fed_union_na_0.rds")

conus_fed_uni_int <- st_intersection(conus_cells, conus_fed_union_na_0)
#saveRDS(conus_fed_uni_int, here::here("data/processed/conus_fed_uni_int.rds"))
saveRDS(conus_fed_uni_int, "~/Analysis/Archetype_Analysis/data/processed/conus_fed_uni_int.rds")
conus_fed_uni_int <- conus_fed_uni_int %>%
  mutate(area = st_area(.)) %>%
  mutate(percent_area = drop_units(area) / (3000*3000)) # add another field if percent area is NA return 0 else return percent area
conus_fed_uni_rast <- rasterize(conus_fed_uni_int, conus_cells_rst, field = "percent_area")
plot(id_fed_uni_rast)
writeRaster(conus_fed_uni_rast, here::here("data/processed/fed_area_3km_conus_", Sys.Date(), ".tif"))
