# functions ----
FoV_W <- function(SW,WD,FL,Perc){
  
  TotalM <- (SW*WD)/FL
  
  Mod <- Perc * TotalM / 100
  
  return(round(Mod,2))
}

FoV_H <- function(SH,WD,FL,Perc){
  
  TotalM <- (SH*WD)/FL
  
  Mod <- Perc * TotalM / 100
  
  return(round(Mod,2))
}

GSD <-  function(SW, H, FL, IW){
  
  GroundSamplingDistance <- ((SW*H*100)/(FL*IW))
  
  return(round(GroundSamplingDistance,2))
}

DW <- function(SW, H, FL, IW){
  
  x <- (((SW*H*100)/(FL*IW))*IW)/100
  
  return(round(x, 2))
}


DH <- function(SW, H, FL, IW, IH){
  
  x <- (((SW*H*100)/(FL*IW))*IH)/100
  
  return(round(x,2))
  
}

CoverageM <-  function(SW, H, FL, IW, IH){
  x <- (((SW*H*100)/(FL*IW))*IW)/100
  y <- (((SW*H*100)/(FL*IW))*IH)/100
  return(round(x*y,2))
}

CoverageHa <- function(SW, H, FL, IW, IH){
  x <- (((SW*H*100)/(FL*IW))*IW)/100
  y <- (((SW*H*100)/(FL*IW))*IH)/100
  return(round((x*y)/10000,2))
}

M2toHa <-  function(M2){
  return(M2/10000)
}
HatoM2 <- function(Ha){
  return(Ha*10000)
}

# create SpatialPointsDataFrame from gpx file
gpx_to_SPDF <- function(gpx){
  gpx_parsed <- htmlTreeParse(gpx, useInternalNodes = T)
  coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
  elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)
  times <- xpathSApply(gpx_parsed, path = "//trkpt/time", xmlValue)
  
  df <- data.frame(
    lat = as.numeric(coords["lat", ]),
    lon = as.numeric(coords["lon", ]),
    elevation = as.numeric(elevation),
    time = times
  )
  # shift vector function
  shift.vec <- function (vec, shift) {
    if(length(vec) <= abs(shift)) {
      rep(NA ,length(vec))
    }else{
      if (shift >= 0) {
        c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
      else {
        c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }
  # Shift vectors for lat and lon so that each row also contains the next position.
  df$lat.p1 <- shift.vec(df$lat, -1)
  df$lon.p1 <- shift.vec(df$lon, -1)
  # create spatial dataframe
  spdf_geo <- df
  # assign gooridnate colums and projections
  coordinates(spdf_geo) <- ~ lon + lat
  proj4string(spdf_geo) <- "+init=epsg:4326"
  return(df)
}

# Download DTM data and crop to desired area using elevatr.
DTM_download_crop <- function(aoi, filename) {
  x <- st_read(aoi)
  dtm <- get_elev_raster(x, z = 11, override_size_check = TRUE)
  mask <- as(extent(x), "SpatialPolygons")
  dir.create("DTM/")
  crop_dtm <- mask(x = crop(dtm, mask), mask) %>%
    raster::writeRaster(file.path("DTM/", paste0(filename, ".tif")),
                        format = "GTiff",
                        overwrite = T)
  return(crop_dtm)
}

# Calculate Height difference compared to starting point
FEP_calc <- function(x, FES, ES){
  
  return(FES+(ES-x))
}

# find center of raster
center <- function(r) {
    lng <- xmin(r) + (xmax(r) - xmin(r)) / 2
    lat <- ymin(r) + (ymax(r) - ymin(r)) / 2
    center <- cbind(lng, lat)
  return(center)
}


calc_coverage_img <- function(lat, lon, dwidth, dheight) {
  #Earth’s radius, sphere
  R = 6378137
  #Coordinate offsets in radians
  dLat = dwidth / 2 / R
  dLon = dheight / 2 / (R * cos(pi * lat / 180))
  
  #OffsetPosition, decimal degrees
  latn = lat + dLat * 180 / pi
  lats = lat - dLat * 180 / pi
  lone = lon + dLon * 180 / pi
  lonw = lon - dLat * 180 / pi
  # create DF with all the information
  df <- data.frame(lonw, lone, lats, latn)
  return(df)
}