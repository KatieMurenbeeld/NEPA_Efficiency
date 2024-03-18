library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)

# Set the timeout to 10 minutes (6000 seconds)
options(timeout=6000)

# For this I only want the wildfire hazard potential (WHP)

download_fire <- function(st){    
  tmp <- tempfile()
  fs.url <- paste0("https://s3-us-west-2.amazonaws.com/fs.usda.rds/RDS-2020-0016/RDS-2020-0016_",st,".zip")
  download.file(fs.url, tmp)
  tmp2 <- tempfile()
  unzip(zipfile=tmp, exdir = tmp2 )
  dir.name <- list.files(tmp2)
  rast.file <- list.files(paste0(tmp2,"/", dir.name), pattern="*.tif$", full.names = TRUE)
  rasters <- rast(rast.file)
  fnames <- paste0("data/original/",names(rasters), ".tif")
  writeRaster(rasters, filename = fnames)
  return(fnames)
}


agg_fire <- function(ogrst){
  rasters <- rast(ogrst)
  fnames.process <- paste0("data/processed/aggregated/",names(rasters), "_240.tif")
  rasters.agg <- aggregate(rasters, fact=8, cores = 10)
  writeRaster(rasters.agg, fnames.process, overwrite=TRUE)
  return(fnames.process) 
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

