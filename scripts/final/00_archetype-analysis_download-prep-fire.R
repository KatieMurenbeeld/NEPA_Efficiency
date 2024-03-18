library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)

# Set the timeout to 10 minutes (6000 seconds)
options(timeout=6000)

# Only want the wildfire hazard potential (WHP)

download_fire <- function(st){    
  tmp <- tempfile()
  fs.url <- paste0("https://s3-us-west-2.amazonaws.com/fs.usda.rds/RDS-2020-0016/RDS-2020-0016_",st,".zip")
  download.file(fs.url, tmp)
  tmp2 <- tempfile()
  unzip(zipfile=tmp, exdir = tmp2 )
  dir.name <- list.files(tmp2)
  rast.file <- list.files(paste0(tmp2,"/", dir.name), pattern="*.tif$", full.names = TRUE)
  whp.rast.file <- rast.file[grepl("WHP", rast.file)]
  rasters <- rast(whp.rast.file)
  fnames <- paste0("data/original/",names(rasters), ".tif")
  writeRaster(rasters, filename = fnames)
  return(fnames)
}

# Test with small, wet states
st_list <- c("Connecticut", "Delaware")

for (state in st_list) {
  download_fire(state)
}

# For next time update this function to aggregate at 3km (fact = 100) and 1.5km (fact = 50)
agg_fire <- function(ogrst){
  rasters <- rast(ogrst)
  fnames.process <- paste0("data/processed/aggregated/",names(rasters), "_1-5km.tif")
  rasters.agg <- aggregate(rasters, fact=50, cores = 2)
  writeRaster(rasters.agg, fnames.process, overwrite=TRUE)
  return(fnames.process) 
}

ogrst_list <- c("data/original/WHP_CT.tif", "data/original/WHP_DE.tif")

for (rst in ogrst_list) {
  agg_fire(rst)
}

merge_all_rst <- function(prefix){
  file.list <- list.files(here::here("data/processed/aggregated/"), pattern = paste0("^[",prefix, "_]"), full.names = TRUE)
  rasters <- lapply(file.list, function(x) rast(x))
  rst.sprc <- sprc(rasters)
  m <- merge(rst.sprc)
  names(m) <- prefix
  fnames.merge <- paste0(prefix, "_merge240.tif")
  writeRaster(m, filename = paste0("data/processed/merged/", fnames.merge), overwrite=TRUE)
  return( paste0("data/processed/merged/", fnames.merge))
}

