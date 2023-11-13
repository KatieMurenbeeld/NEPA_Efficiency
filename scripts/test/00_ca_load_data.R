# 
library(sf)

## Use tempfile() and unzip() to load the US Forest Service National Forest and Regional boundaries
tmp <- tempfile()
fs.nf.url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
download.file(fs.nf.url, tmp)
tmp2 <- tempfile()
unzip(zipfile=tmp, exdir = tmp2 )
fs.nf.bdry <- read_sf(tmp2)

tmp <- tempfile()
fs.rg.url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeRegion.zip"
download.file(fs.rg.url, tmp)
tmp2 <- tempfile()
unzip(zipfile=tmp, exdir = tmp2 )
fs.rg.bdry <- read_sf(tmp2)

## Use tempfile() and unzip() to load the Threatened and Endangered Species Critical Habitats

tmp <- tempfile()
fws.te.url <- "https://ecos.fws.gov/docs/crithab/crithab_all/crithab_all_layers.zip"
download.file(fws.te.url, tmp)
tmp2 <- tempfile()
unzip(zipfile=tmp, exdir = tmp2 )
fws.te.bdry <- read_sf(tmp2)




