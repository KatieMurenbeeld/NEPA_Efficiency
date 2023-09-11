## For this spatial analysis, need to download two datasets
## PALS ongoing
## The Forest Service Forest Administration Boundaries

library(googledrive)

## Download Data from Lab Google Drive
# This bit will open an authorization window in Google Drive asking if you want to allow RStudio to access your folders. 
# You have to click yes and then it will store your credentials locally
options(  gargle_oauth_cache = ".secrets",  gargle_oauth_email = TRUE)

# Assign the link for the GDrive folder to an object
folder_url <- "https://drive.google.com/drive/folders/1O5fcqx7pVag5r-qBAJm-hDl02pkK1ty4"

# drive_get will open the authorization window; you have to give Rstudio access to edit files once you do that you'll get the necessary attributes for the GDrive folder
folder <- drive_get(as_id(folder_url))

# drive_ls lists all files in the folder. Note that if there are subfolders, each folder has to be processed separately
gdrive_files <- drive_ls(folder)
# lapply steps through each file in the gdrive_list and downloads them to data/original/
lapply(gdrive_files$id, function(x) drive_download(as_id(x), 
                                                   path = paste0(here::here("data/original/"), 
                                                                 gdrive_files[gdrive_files$id==x,]$name), 
                                                   overwrite = TRUE))

## Download the administrative region, forest, and district boundaries from the USFS geodata clearing house
# Use download.file() and set the url, destination file, and method = "curl"
# unzip() the file (same as destfile)

# Region
download.file(url = "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeRegion.zip",
              destfile = "data/original/S_USA.AdministrativeRegion.zip", method = "curl")
unzip("data/original/S_USA.AdministrativeRegion.zip", exdir = "data/original")

# Forest
download.file(url = "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip",
              destfile = "data/original/S_USA.AdministrativeForest.zip", method = "curl")
unzip("data/original/S_USA.AdministrativeForest.zip", exdir = "data/original")

# District
#download.file(url = "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.RangerDistrict.zip",
#              destfile = "data/original/S_USA.RangerDistrict.zip", method = "curl")
#unzip("data/original/S_USA.RangerDistrict.zip", exdir = "data/original")



