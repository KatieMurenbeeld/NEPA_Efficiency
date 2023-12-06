library(tigris)
library(sf)
library(tidyverse)
# Download LCV Data -------------------------------------------------------
rt <- "http://scorecard.lcv.org/exports/"
yr <- seq(from=1972,to=2019,by=1)
hs <- "-house"
fl <- "-scorecard-grid-export.csv"

for(i in 1:length(yr)){
  y <- yr[i]
  link <- paste0(rt,y,hs,fl)
  fname <- paste0(here::here("data/original/"),y,hs,".csv")
  download.file(url=link, destfile=fname)
}


# Download Congressional Data ---------------------------------------------

rt <- "http://cdmaps.polisci.ucla.edu/shp/districts"
cn <- seq(from=92, to=114, by = 1)
cnp <- ifelse(cn < 100, paste0("0",cn), cn)
ext <- ".zip"

for(i in 1:length(cnp)){
  y <- cnp[i]
  link <- paste0(rt,y,ext)
  fname <- paste0(here::here("data/original/"),y,ext)
  download.file(url=link, destfile=fname)
}

dist.zip <- list.files(here::here("data/original/"), pattern=".zip")
lapply(paste0(here::here("data/original/"),dist.zip),unzip, exdir = here::here("cong_dist_shp/"), junkpaths=TRUE)
##Load Cong District
dist.files <- list.files(here::here("cong_dist_shp/"),pattern=".shp")
prj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" #projection for NatLandCover in Monument Dataset

options(warn=-1)  # temporarily turn off warnings
for(i in 1:length(dist.files)) {
  # need to update readOGR to appropriate command in sf package
  temp1 <- st_read(paste0(here::here("cong_dist_shp/"), dist.files[i])) 
  temp1 <- st_transform(temp1, prj)
  if(length(which(st_is_valid(temp1)==FALSE))==0) {
    print(paste("Geometry is valid for layer ",dist.files[i], sep=""))
    st_write(as(temp1,"sf"),paste0(here::here("data/processed/"),dist.files[i]))
  } else {  # if invalid geometries are found (e.g., Ring Self-intersection), convert to sp and then add zero-width buffer
    print("Invalid geometry found - applying zero-width buffer...")
    temp2 <- st_buffer(temp1, byid=TRUE, dist=0)  # add zero-width buffer
    if(length(which(st_is_valid(temp2)==FALSE))==0) {  # check again for invalid geometries
      print(paste("Geometry corrected for layer ", dist.files[i], sep=""))
      temp3 <- as(temp2, "sf")
      st_write(temp3,paste0(here::here("data/processed/"),dist.files[i]))
    } else {
      stop(paste("Unable to correct geometry for layer ",dist.files[i],"!!!", sep=""))
    }
    rm(temp1, temp2, temp3)
  }
}
options(warn=0)  # turn warnings back on


# download 115 and 116 congress -------------------------------------------
tigris::congressional_districts(year = 2016) %>% 
  as(., "sf") %>% 
  st_transform(., prj) %>% 
  rename(., DISTRICT = CD115FP) %>% 
  left_join(., fips_codes[,2:3], by = c("STATEFP" = "state_code")) %>%
  rename(., STATENAME = state_name) %>% 
  group_by(GEOID,STATENAME, DISTRICT) %>% summarise() %>% 
  st_write(., here::here("data/processed/districts115.shp"))

tigris::congressional_districts(year = 2018) %>% as(., "sf") %>% st_transform(., prj) %>% 
  rename(., DISTRICT = CD116FP) %>% 
  left_join(., fips_codes[,2:3], by = c("STATEFP" = "state_code")) %>% 
  rename(., STATENAME = state_name) %>% 
  group_by(GEOID,STATENAME, DISTRICT) %>% summarise() %>% 
  st_write(., here::here("data/processed/districts116.shp"))


district.f.name <- list.files(here::here("data/processed/"), pattern="districts.*.shp")
cong_num <- substr(district.f.name, start=10, stop=12)
st_yr <- seq(from=1971, by=2, length.out=length(cong_num))
end_yr <- st_yr + 1
cong_data <- as.data.frame(cbind(cong_num,st_yr,end_yr))

##Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name))
colnames(us.states) <- c("state","STATENAME")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS
house.files <- list.files(here::here("data/original"), pattern="house.csv", full.names = TRUE)

LCVHouseScore <- function(fname){
  df <- read.csv(fname, skip=6)
  df$state <- substr(df$District, 1,2)
  df$dist <- substr(df$District, 4,5)
  df$year <- substr(colnames(df)[4],2,5)
  NameList <- c("state","dist", "year", paste0("X",substr(fname,65,68),".Score")) #Takes only the year score, not the lifetime score of the rep
  idx <- match(NameList, names(df))
  df2 <- df[,idx]
  colnames(df2)[4] <- "LCVScore"
  return(df2)
}
hse <- lapply(house.files, LCVHouseScore)
LCVhse <- do.call(rbind, hse)
LCVhse <- merge(LCVhse, continental.states, by="state")

years <- seq(1972,2019,by=1)
y <- 46
for (y in 1:length(years)){
  yr <- years[y]
  spatial_id <- cong_data[st_yr==yr | end_yr==yr,]
  cd <- st_read(paste0(here::here("data/processed/"),"districts",spatial_id[,1], ".shp"), stringsAsFactors=FALSE)
  cd$DISTRICT <- ifelse( y <= 45 & as.numeric(cd$DISTRICT) <10, paste0("0",cd$DISTRICT), cd$DISTRICT)
  cd$stid <- paste(cd$STATENAME, cd$DISTRICT, sep="")
  hsLCV <- dplyr::filter(LCVhse, year==yr)
  hsLCV$dist <- ifelse(hsLCV$dist == "AL","00",hsLCV$dist)
  hsLCV$stid <- paste(hsLCV$STATENAME, hsLCV$dist, sep="")
  hsLCV$LCVScore <- as.numeric(hsLCV$LCVScore)
  #hsunique <- plyr::ddply(hsLCV,"stid",plyr::numcolwise(mean, na.rm=TRUE))
  d <- cd %>% left_join(., hsLCV) 
  st_write(d, paste0(here::here('data/processed/'),yr,"districtLCV.shp"))
  print(yr)
}



# rasterize ---------------------------------------------------------------

library(tigris)
library(raster)
library(sf)
library(sp)
library(rgdal)
library(rgeos)



##Select all districts within a state
#rasterOptions(progress="text", maxmemory=1e+10, chunksize=1e+09)
#setwd("D:/Data/MonumentData/LCV/Outputs/GeomClean/")
i <- 1
s <- 1
proj.dist.files <- list.files(here::here("LCVshapes"),pattern=".shp", full.names = TRUE)
aea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
base.shape <- st_read(proj.dist.files[1]) %>% st_transform(., crs=aea)

LCV_rasterize <- function(proj.dist.files,base.shape, yr, dest.dir, aoi){  
  dist.shp <- st_read(proj.dist.files[yr]) %>% 
    filter(STATENAME %in% aoi, drop = TRUE) %>% st_transform(., st_crs(base.shape))
  a.box <- st_bbox(dist.shp)
  print(paste0("Rasterizing", years[yr], sep=" "))
  r <- raster(extent(a.box[1], a.box[3], a.box[2], a.box[4]), crs=proj4string(as(dist.shp,"Spatial")))
  res(r) <- c(120,120)
  r[] <- NA
  #st.cd.sp <- as(st.cd, "Spatial")
  cdr <- fasterize::fasterize(dist.shp,r,field= "LCVScore")
  writeRaster(cdr,filename=paste0(here::here(paste0(dest.dir,"/")),"LCV_score_", years[yr]),format="GTiff", options= c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
}

lapply(1:length(years), function(x) LCV_rasterize(proj.dist.files = proj.dist.files, base.shape = base.shape, yr = x, dest.dir = "LCV_rst", aoi = continental.states$STATENAME))

rst.list <- list.files(here::here("LCV_rst/"), pattern = "*.tif", full.names = TRUE)

rast_resamp <- function(base.rst, resamp.rast.list, yr, dest.dir){
  resamp.rast <- raster(resamp.rast.list[[yr]])
  if (raster::all.equal(base.raster, resamp.rast, values=FALSE)){
    print(paste0(years[yr], "LCV raster is correct"))
    writeRaster(resamp.rast, filename=paste0(here::here(paste0(dest.dir,"/")),"LCV_score_res", years[yr]),format="GTiff", options= c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
    }else{
      print(paste0(years[yr], "LCV raster is being resampled"))
      cor.rast <- resample(resamp.rast, base.rst, filename=paste0(here::here(paste0(dest.dir,"/")),"LCV_score_res", years[yr]),format="GTiff", options= c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
    }
}

base.raster <- raster(rst.list[[1]])

lapply(1:length(years), function (x) rast_resamp(base.rst = base.raster, resamp.rast.list = rst.list, yr = x, dest.dir = "LCV_align"))

aligned.rasters <- list.files(here::here("LCV_align/"), pattern = "*.tif", full.names = TRUE)
aligned.rasters.list <- lapply(1:length(aligned.rasters), function(x) raster(aligned.rasters[[x]]))

LCV.brick <- do.call(brick, aligned.rasters.list)
br.mn <- mean(LCV.brick, na.rm=TRUE)
writeRaster(br.mn, filename=paste0(here::here("processed/"), "LCVmn"),format="GTiff", options= c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))

rasterOptions(maxmemory = 5e+10, chunksize = 7e+08)
br.var <- calc(LCV.brick, var, na.rm=TRUE, filename=paste0(here::here("processed/"), "LCVvar"),format="GTiff", options= c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))

br.med <- calc(LCV.brick, median, na.rm=TRUE, filename=paste0(here::here("processed/"), "LCVmed"),format="GTiff", options= c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))

br.max <- calc(LCV.brick, max, na.rm=TRUE, filename=paste0(here::here("processed/"), "LCVmax"),format="GTiff", options= c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))

br.min <- calc(LCV.brick, min, na.rm=TRUE, filename=paste0(here::here("processed/"), "LCVmin"),format="GTiff", options= c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))

br.swing <- br.max-br.min


br.delta <- (LCV.brick[[48]] - LCV.brick[[1]])/length(years)
writeRaster(br.delta, filename=paste0(here::here("processed/"), "LCVdelta"),format="GTiff", options= c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
writeRaster(br.swing, filename=paste0(here::here("processed/"), "LCVswing"),format="GTiff", options= c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))