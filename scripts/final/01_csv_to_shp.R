library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(tigris)

## Load the csv files
elect_cntx <- read_csv(paste0(here::here("data/original/election_context_2018.csv")))
forest_depend <- read_csv(paste0(here::here("data/original/Data/County-Forest_Dep_Comm_Capital.csv")))
nat_amen <- read.csv(paste0(here::here("data/original/natural_amenity.csv")), skip = 104)
econ_typ <- read_csv(paste0(here::here("data/original/county_typology_codes_2015.csv")))
parti_sort <- read_csv(paste0(here::here("data/original/Codebook-County-relative-exposure-aggregate-data-updated.csv")))
rrlrbn_cc <- read_csv(paste0(here::here("data/original/rural_urban_cc_2023.csv")))
del_pop <- read_csv(paste0(here::here("data/original/population_estimates_2022.csv")))
lcv_score <- read_csv(paste0(here::here("data/original/2019-house.csv")))

## Load county boundaries from tigris
counties <- tigris::counties()
##Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]
us.fips <- unique(fips_codes$state_code)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name, us.fips))
colnames(us.states) <- c("state", "STATENAME", "FIPS")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS

counties <- tigris::counties()
counties <- counties %>%
  filter(STATEFP %in% us.states$FIPS) %>%
  select(GEOID, geometry)

#cong_dist <- tigris::congressional_districts()

# Create one large table with FIPS codes and desired variables

rucc <- rrlrbn_cc %>%
  filter(Attribute == "RUCC_2023") %>%
  select(FIPS, Value) %>%
  rename("RUCC_2023" = "Value")

delpop <- del_pop %>%
  filter(Attribute == "R_NET_MIG_2021") %>%
  select(FIPStxt, Value) %>%
  rename("FIPS" = "FIPStxt", "R_NET_MIG_2021" = "Value")

nam <- nat_amen %>%
  select(FIPS.Code, X1.Low..7.High) %>%
  rename("FIPS" = "FIPS.Code", "NAT_AMEN_SCORE_1999" = "X1.Low..7.High")

fordep <- forest_depend %>%
  select(fips, forest.area, pct.pay, pct.forest, forest.dependent) %>%
  rename("FIPS" = "fips")

econ <- econ_typ %>%
  select(FIPStxt, `Economic Types Type_2015_Update non-overlapping`, Economic_Type_Label) %>%
  rename("FIPS" = "FIPStxt", "econ_type_2015" = `Economic Types Type_2015_Update non-overlapping`, 
         "econ_label" = "Economic_Type_Label")

`%+%` <- function(x, y)  mapply(sum, x, y, MoreArgs = list(na.rm = TRUE))

elect <- elect_cntx %>% #need to ignore NAs when calcualting things
  mutate("vt_pres16" = (trump16 %+% clinton16 %+% otherpres16) / total_population) %>%
  mutate("vt_pres12" = (romney12 %+% obama12 %+% otherpres12) / total_population) %>%
  mutate("ave_vt_pres" = (vt_pres16 %+% vt_pres12) / 2 ) %>%
  mutate("ave_vt_nopres" = (((demsen16 %+% repsen16 %+% othersen16) / total_population) %+%
           ((demhouse16 %+% rephouse16 %+% otherhouse16) / total_population) %+%
           ((demgov14 %+% repgov14 %+% othergov14) / total_population) %+%
             ((demgov16 %+% repgov16 %+% othergov16) / total_population)) / 3) %>% 
  mutate("ave_rep" = ((trump16 / (trump16 %+% clinton16 + otherpres16)) %+% 
           (romney12 / (romney12 %+% obama12 %+% otherpres12)) %+% 
           (repsen16 / (repsen16 %+% demsen16 %+% othersen16)) %+% 
           (rephouse16 / (rephouse16 + demhouse16 %+% otherhouse16)) %+%
           (repgov16/ (repgov16 %+% demgov16 %+% othergov16)) %+%
           (repgov14 / (repgov14 %+% demgov14 %+% othergov14))) / 5) %>%
  mutate("ave_dem" = ((clinton16 / (trump16 %+% clinton16 %+% otherpres16)) %+% 
                              (obama12 / (romney12 %+% obama12 %+% otherpres12)) %+% 
                              (demsen16 / (repsen16 %+% demsen16 %+% othersen16)) %+% 
                              (demhouse16 / (rephouse16 %+% demhouse16 %+% otherhouse16)) %+%
                              (demgov14 / (repgov14 %+% demgov14 %+% othergov14))) / 5) %>%
  select(fips, ave_vt_pres, ave_vt_nopres, ave_rep, ave_dem, lesscollege_pct) %>%
  rename("FIPS" = "fips")

update_fips <- function(date_set) {
  data_set$FIPS <- as.character(data_set$FIPS)
  data_set$FIPS <- str_pad(data_set$FIPS, 5, side="left", pad="0")
  return(data_set)
}

data_set <- elect
elect_fips <- update_fips(elect)
data_set <- fordep
fordep_fips <- update_fips(fordep)
data_set <- nam
nam_fips <- update_fips(nam)
data_set <- econ
econ_fips <- update_fips(econ)
data_set <- delpop
delpop_fips <- update_fips(delpop)
data_set <- rucc
rucc_fips <- update_fips(rucc)

all_vars <- plyr::join_all(list(elect_fips, fordep_fips, nam_fips, econ_fips, delpop_fips, rucc_fips),
                     by='FIPS', 
                     type='left')

# Join to counties

var_bdry <- left_join(all_vars, counties,
                    by = c("FIPS" = "GEOID"))

var_bdry <- st_as_sf(var_bdry)

# Join to congressional districts...only needed for LCV score

## Check and fix validity
all(st_is_valid(var_bdry))

## Check for empty geometries and invalid or corrupt geometries 
any(st_is_empty(var_bdry))
any(is.na(st_is_valid(var_bdry)))
any(na.omit(st_is_valid(var_bdry)) == FALSE)
st_is_longlat(var_bdry)

# Set the projection and a base shape
#prj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # projection for NLCD2021
#base_shp <- var_bdry %>% st_transform(., crs = prj)


## Write the validated and factorized shp to a new shp
write_sf(obj = var_bdry, dsn = paste0(here::here("data/processed/"), "all_vars_to_rst.shp"), overwrite = TRUE, append = FALSE)
print("new shapefile written")
