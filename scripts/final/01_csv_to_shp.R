#source("/bsuhome/katiemurenbeeld/setup.R")
library(stringr)

## Load the csv files
elect_cntx <- read_csv(paste0(here::here("data/original/election_context_2018.csv")))
forest_depend <- read_csv(paste0(here::here("data/original/County-Forest_Dep_Comm_Capital.csv")))
nat_amen <- read_csv(paste0(here::here("data/original/natural_amenity.csv")))
econ_typ <- read_csv(paste0(here::here("data/original/county_typology_codes_2015.csv")))
parti_sort <- read_csv(paste0(here::here("data/original/Codebook-County-relative-exposure-aggregate-data-updated.csv")))
rrlrbn_cc <- read_csv(paste0(here::here("data/original/rural_urban_cc_2023.csv")))
del_pop <- read_csv(paste0(here::here("data/original/population_estimates_2022.csv")))
lcv_score <- read_csv(paste0(here::here("data/original/2019-house.csv")))

## Load county boundaries from tigris
states <- tigris::states()
counties <- tigris::counties(c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"))
counties <- tigris::counties()
#counties <- counties %>%
#  filter out states you don't want'

cong_dist <- tigris::congressional_districts()

# Join to counties

join_counties <- function(data_set) {
  data_set$fips <- as.character(data_set$fips)
  data_set$fips <- str_pad(elect.cntx$fips, 5, side="left", pad="0")
  bdry <- left_join(counties, data_set,
                    by = c("GEOID" = "fips"))
  return(bdry)
}

# Join to congressional districts...only needed for LCV score
# join_cong <- function(data_set) {
#data_set$district
#bdry <- left_join(cong_dist, data_set,
#by = c("", "district"))
#return(bdry)
#}

## Check and fix validity
#elect.cntx.bdry <- st_make_valid(elect.cntx.bdry)
forest.depend.bdry <- st_make_valid(forest.depend.bdry)

# make list of files to make valid
not_valid <- list()

for (i in not_valid) {
  st_make_valid(i)
}

## Check for NA in desired variables
### I already know there are some in the election context
#elect.cntx.bdry$ruralurban_cc[is.na(elect.cntx.bdry$ruralurban_cc)] <- 0
print("replaced NAs")

## Write the validated and factorized shp to a new shp
write_sf(obj = forest.depend.bdry, dsn = paste0(proc_dir, "forest_depend_to_rst.shp"), overwrite = TRUE, append = FALSE)
print("new shapefiles written")