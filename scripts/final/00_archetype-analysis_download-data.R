# 
library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)

# Some files are large so set the timeout to 10 minutes (6000 seconds)
options(timeout=6000)

# There are three file types we will download: csv, shp, and tif/img.

download_data <- function(url) {
  if (str_detect(url, ".zip")) {
    tmp <- tempfile()
    download.file(url, tmp)
    unzip(zipfile = tmp, exdir = here::here("data/original/"))
  } else {
    download.file(url, here::here("data/original/"))
  }
}

#---Download csv data----

# Partisan sorting

url <- "https://dataverse.harvard.edu/file.xhtml?fileId=8165593&version=3.0#"

download_data(url)

# Natural amenity rank

# League of Conservation Voters

# Economy typology

# Change in population

# Rural-Urban continuum

# Election Context 2018