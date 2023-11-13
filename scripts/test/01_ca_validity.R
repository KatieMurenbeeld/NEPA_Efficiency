## Check the validity of the geometries
library(tidyverse)
library(sf)
library(terra)


### will also combine the election context data to county data

## Check the geometry validity

all(st_is_valid(fs.nf.bdry))
all(st_is_valid(fs.rg.bdry))
all(st_is_valid(fws.te.bdry))
all(st_is_valid(counties))

## Only the National Forest boundaries need to be fixed

fs.nf.valid <- st_make_valid(fs.nf.bdry)
all(st_is_valid(fs.nf.valid))

## Check raster alignment?

## Join election context to counties

elect.cntx$fips <- as.character(elect.cntx$fips)

elect.cntx.bdry <- counties %>%
  left_join(elect.cntx, by = c("COUNTYFP" = "fips"))



