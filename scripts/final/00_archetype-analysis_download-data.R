# 
library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)

# Some files are large so set the timeout to 10 minutes (6000 seconds)
options(timeout=6000)

# There are three file types we will download: csv, shp, and tif/img.

download_data <- function(url, file_name) {
  if (str_detect(url, ".zip")) {
    tmp <- tempfile()
    download.file(url, tmp)
    unzip(zipfile = tmp, exdir = here::here("data/original/"))
  } else {
    download.file(url, here::here(paste0("data/original/", file_name)))
  }
}

#---Download csv data-----------------------------------------------------------

# Partisan sorting, need to download from website

url <- "https://dataverse.harvard.edu/file.xhtml?fileId=8165593&version=3.0#"
file_name <- "partisan_sorting"

download_data(url, file_name)

# Natural amenity rank
url <- "https://www.ers.usda.gov/webdocs/DataFiles/52201/natamenf_1_.xls?v=723.9"
file_name <- "natural_amenity.csv"

download_data(url, file_name)

# League of Conservation Voters
rt <- "http://scorecard.lcv.org/exports/"
yr <- seq(from=2018,to=2019,by=1)
hs <- "-house"
fl <- "-scorecard-grid-export.csv"

for(i in 1:length(yr)){
  y <- yr[i]
  link <- paste0(rt,y,hs,fl)
  fname <- paste0(here::here("data/original/"),y,hs,".csv")
  download.file(url=link, destfile=fname)
}

# Economy typology
url <- "https://www.ers.usda.gov/webdocs/DataFiles/48652/2015CountyTypologyCodes.csv?v=7440.8"
file_name <- "county_typology_codes_2015.csv"

download_data(url, file_name)

# Change in population
url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.csv?v=3995.5"
file_name <- "population_estimates_2022.csv"

download_data(url, file_name)

# Rural-Urban continuum
url <- "https://www.ers.usda.gov/webdocs/DataFiles/53251/Ruralurbancontinuumcodes2023.csv?v=4628.7"
file_name <- "rural_urban_cc_2023.csv"
  
download_data(url, file_name)

# Election Context 2018
x <- getURL("https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv")
y <- read.csv(text = x)
write.csv(y, here::here("data/original/election_context_2018.csv"))

#---Download shp data-----------------------------------------------------------

# USFS National Forest Boundaries
url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
file_name <- ""

download_data(url, file_name)

# USFS Regional Boundaries
url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeRegion.zip"
file_name <- ""

download_data(url, file_name)

# FWS Critical Habitat
url <- "https://ecos.fws.gov/docs/crithab/crithab_all/crithab_all_layers.zip"
file_name <- ""

download_data(url, file_name)

# National Wilderness Areas
url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.Wilderness.zip"
file_name = ""

download_data(url, file_name)

# Climate and Economic Justice Screening Tool
url <- "https://static-data-screeningtool.geoplatform.gov/data-versions/1.0/data/score/downloadable/1.0-shapefile-codebook.zip"
file_name <- ""

download_data(url, file_name)

# Forest Dependence 
url <- "https://www.fs.usda.gov/rds/archive/products/RDS-2021-0077/RDS-2021-0077.zip"
file_name <- ""

download_data(url, file_name)

#---Download tif data-----------------------------------------------------------
## These may be too large to do through RStudio?

# Elevation
url <- "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Topo_2020-LF2020_Elev_220_CONUS.zip&TYPE=landfire"
file_name <- ""

download_data(url, file_name)

# Wildfire Risk
# See script 00_archetype-analysis_download-fire.R
