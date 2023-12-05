library(parallel)
library(raster)
library(rgdal)
library(tigris)
library(sf)
library(tidyverse)
LCVrsts <- list.files(here::here("LCV_rst/"), full.names = TRUE)
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name))
colnames(us.states) <- c("state","STATENAME")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS

st <- tigris::states() %>% as(., "sf") %>% 
  st_transform(., proj4string(base)) %>% 
  filter(NAME %in% continental.states$STATENAME, drop=TRUE)
base <- raster(LCVrsts[1])
proj.rstr <- raster(LCVrsts[46])
all.equal(base, proj.rstr)
p.crop <- crop(proj.rstr, as(st, "Spatial"))
b.crop <- crop(base.rstr, as(st, "Spatial"))
all.equal(p.crop, b.crop)

#maybe 'alignExtent'?


THIS DOESNT DO WHAT I NEED TO
#' Reprojects/resamples and aligns a raster
#'
#' Reprojects/resamples and aligns a raster by matching a raster a raster to a specified origin, resolution, and coordinate reference system, or that of a reference raster. Useful for preparing adjacent areas before using raster::merge.
#' @param rast raster to be reprojected or resampled
#' @param ref_rast reference raster with desired properties (Alternatively can supply desired_origin, desired_res, and desired_crs)
#' @param desired_origin desired origin of output raster as a vector with length 2 (x,y)
#' @param desired_res  desired resolution of output raster. Either an integer or a vector of length 2 (x,y)
#' @param desired_crs desired coordinate reference system of output raster (CRS class)
#' @param method resampling method. Either "bilinear" for bilinear interpolation (the default), or "ngb" for using the nearest neighbor
#' @importFrom  raster crs
#' @importFrom  raster extent
#' @importFrom  raster origin
#' @importFrom  raster projectExtent
#' @importFrom raster raster
#' @importFrom raster resample
#' @importFrom raster projectRaster
#' @export

reproject_align_raster<- function(rast, ref_rast=NULL, desired_origin, desired_res, desired_crs, method= "bilinear"){
  
  if (!is.null(ref_rast)) {
    desired_origin<- origin(ref_rast) #Desired origin
    desired_res<- res(ref_rast) #Desired resolution
    desired_crs<- crs(ref_rast) #Desired crs
  } #Set parameters based on ref rast if it was supplied
  if(length(desired_res)==1){
    desired_res<- rep(desired_res,2)}
  
  if(identical(crs(rast), desired_crs) & identical(origin(rast), desired_origin) & identical(desired_res, res(rast))){
    message("raster was already aligned")
    return(rast)} #Raster already aligned
  
  if(identical(crs(rast), desired_crs)){
    rast_orig_extent<- extent(rast)} else{
      rast_orig_extent<- extent(projectExtent(object = rast, crs = desired_crs))} #reproject extent if crs is not the same
  var1<- floor((rast_orig_extent@xmin - desired_origin[1])/desired_res[1])
  new_xmin<-desired_origin[1]+ desired_res[1]*var1 #Calculate new minimum x value for extent
  var2<- floor((rast_orig_extent@ymin - desired_origin[2])/desired_res[2])
  new_ymin<-desired_origin[2]+ desired_res[2]*var2 #Calculate new minimum y value for extent
  n_cols<- ceiling((rast_orig_extent@xmax-new_xmin)/desired_res[1]) #number of cols to be in output raster
  n_rows<- ceiling((rast_orig_extent@ymax-new_ymin)/desired_res[2]) #number of rows to be in output raster
  new_xmax<- new_xmin+(n_cols*desired_res[1]) #Calculate new max x value for extent
  new_ymax<- new_ymin+(n_rows*desired_res[2]) #Calculate new max y value for extent
  rast_new_template<- raster(xmn=new_xmin, xmx =new_xmax,  ymn=new_ymin, ymx= new_ymax, res=desired_res, crs= desired_crs) #Create a blank template raster to fill with desired properties
  if(!identical(desired_origin,origin(rast_new_template))){
    message("desired origin does not match output origin")
    stop()} #Throw error if origin doesn't match
  if(identical(crs(rast),desired_crs)){
    rast_new<- raster::resample(x=rast, y=rast_new_template, method = method)} else{
      rast_new<- projectRaster(from=rast, to=rast_new_template, method = method)} #Use projectRaster if crs doesn't match and resample if they do
  if(!identical(desired_origin,origin(rast_new))){
    message("desired origin does not match output origin")
    stop()} #Throw error if origin doesn't match
  return(rast_new)
}

proj.rst <- reproject_align_raster(rast = proj.raster, ref_rast = base.rstr)
