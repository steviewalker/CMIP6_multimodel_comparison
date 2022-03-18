#' @title Regrid Model Resolution
#' @author Stevie Walker
#' @date 3/7/22
#' @source https://rpubs.com/markpayne/132500

## Ignore this section --------------

rGFDL <- readRDS("~/senior_thesis/plotting_dataframes/expc/GFDL_mean_expc_st.Rds")
rMPI <- readRDS("~/senior_thesis/plotting_dataframes/expc/MPI_mean_expc_st.Rds")

rGFDL <- raster(rGFDL, crs = "+proj=longlat +datum=WGS84")
rMPI <- raster(rMPI, crs = "+proj=longlat +datum=WGS84")

res(rGFDL) <- c(1,1)

#set for all models before matching to GFDL resolution
#proj4string(rMPI) = "+proj=longlat +datum=WGS84"
#proj4string(rGFDL) = "+proj=longlat +datum=WGS84"

new_res_MPI <- matchResolution(x= rMPI,ref= rGFDL,method= "bilinear")

plot(new_res_MPI)

ggsave("MPI_res_test.png", test, path = "~/senior_thesis/figures", width = 20, height = 20, units = "cm", dpi = 400)


#set for all models before matching to GFDL resolution
proj4string(rMPI) = "+proj=longlat +datum=WGS84"
proj4string(rGFDL) = "+proj=longlat +datum=WGS84"

bb = extent(0,404,0,802)
bb = extent(0,360,0,180)
rMPI = setExtent(rMPI, bb, keepres=TRUE)
rGFDL = setExtent(rGFDL, bb, keepres=TRUE)

rCESM <- readRDS("~/senior_thesis/plotting_dataframes/CESM_mean_expc_st.Rds")
rCMCC <- readRDS("~/senior_thesis/plotting_dataframes/CMCC_mean_expc_st.Rds")
rECEarth <- readRDS("~/senior_thesis/plotting_dataframes/EC-Earth_mean_expc_st.Rds")
rGFDL <- readRDS("~/senior_thesis/plotting_dataframes/GFDL_mean_expc_st.Rds")
rMPI <- readRDS("~/senior_thesis/plotting_dataframes/MPI_mean_expc_st.Rds")
rUKESM <- readRDS("~/senior_thesis/plotting_dataframes/UKESM_mean_expc_st.Rds")

rCESM <- raster(rCESM)
rCMCC <- raster(rCMCC)
rECEarth <- raster(rECEarth)
rGFDL <- raster(rGFDL, xmn = 0, xmx = 360, ymn = 0, ymx = 180, crs = "+proj=longlat +datum=WGS84")
rMPI <- raster(x = rMPI, xmn = 0, xmx = 802, ymn = 0, ymx = 404, crs = "+proj=longlat +datum=WGS84")
rMPI <- raster(rMPI, crs = "+proj=longlat +datum=WGS84")
rUKESM <- raster(rUKESM)


## Regridding a slice of CESM NPP data to regular lat lon grid in the North Atlantic  --------------

#example from https://rpubs.com/markpayne/132500 but with my data

setwd("~/senior_thesis/combined_CESM_files/")
nc <- "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
nc_file <- nc_open("pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc")

#create pp raster
b <- brick(nc,varname="pp")

#extract coordinates at each grid cell
lon <- raster(nc,varname="lon")
lat <- raster(nc,varname="lat")

#convert grid cells and corresponding lat-lon to a data.frame
library(reshape)
remap.tbl <- data.frame(coordinates(lon),
                        lon=as.vector(lon),lat=as.vector(lat))
tail(remap.tbl)

#correct lon scale
remap.tbl$lon <- ifelse(remap.tbl$lon>180,
                        remap.tbl$lon-360,
                        remap.tbl$lon)
range(remap.tbl$lon)

#shows entire model native grid
library(maps)
map("world",fill=TRUE,mar=c(0,0,0,0))
points(lat ~lon,remap.tbl,pch=".",col="red")

library(akima)
#sample points from North Atlantic
remap.NA <- subset(remap.tbl,lon>-60 & lon<30 & lat > 30)

geo.r <- raster(extent(-50,0,50,75))
res(geo.r) <- c(0.25,0.25)

r.coords.x <- interp(remap.NA$lon,remap.NA$lat,remap.NA$x,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords.y <- interp(remap.NA$lon,remap.NA$lat,remap.NA$y,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords <- expand.grid(lon=xFromCol(geo.r),
                        lat=yFromRow(geo.r))
r.coords$x <- as.vector(r.coords.x$z)
r.coords$y <- as.vector(r.coords.y$z)

r.coords.sp <- r.coords
coordinates(r.coords.sp) <- ~x +y
#important to make sure extract is from right package
r.coords$pp <- raster::extract(b[[1]],r.coords.sp,method="bilinear")
geo.r <- setValues(geo.r,r.coords$pp)
plot(geo.r)
map("worldHires",add=TRUE)

# Raster practice (Ignore this) ----------

#create empty raster of shape and resolution we want

firstRaster <- raster(xmn = 0,   # set minimum x coordinate
                      xmx = 360,    # set maximum x coordinate
                      ymn = 0,     # set minimum y coordinate
                      ymx = 180,     # set maximum y coordinate
                      res = c(1,1)) # resolution in c(x,y) direction
#add values to raster (must be equal to number of cells)

firstRaster[] <- seq(from = 1, to = ncell(firstRaster),by = 1)

plot(firstRaster)

# regridding CESM to regular lat lon grid and adding 20 year long-term average NPP data ----------

matrix <- readRDS("~/senior_thesis/plotting_dataframes/NPP/CESM_mean_npp_lt.Rds")

setwd("~/senior_thesis/combined_CESM_files/")
nc <- "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
nc_file <- nc_open("pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc")

#create pp raster
pp <- raster(matrix, crs = "+proj=longlat +datum=WGS84")

plot(pp)

#empty raster that matches CESM grid
gn <- raster(xmn = 1,   # set minimum x coordinate
             xmx = 320,    # set maximum x coordinate
             ymn = 1,     # set minimum y coordinate
             ymx = 384,     # set maximum y coordinate
             res = c(1,1)) # resolution in c(x,y) direction

#add npp data
gn[] <- pp

#trying another way
matrix <- readRDS("~/senior_thesis/plotting_dataframes/NPP/CESM_mean_npp_lt.Rds")
lat.old = ncvar_get(nc_file, "nlat")
lon.old = ncvar_get(nc_file, "nlon")

dimnames(matrix) = list(lon = lon.old, lat = lat.old)
old.df = melt(matrix)

head(old.df)

r = rasterFromXYZ(old.df)
t = raster::resample(r, firstRaster, method = "bilinear")

lon <- raster(nc,varname="lon")
lat <- raster(nc,varname="lat")

remap.tbl <- data.frame(coordinates(lon),
                        lon=as.vector(lon),lat=as.vector(lat))
#correct lon scale
remap.tbl$lon <- ifelse(remap.tbl$lon>180,
                        remap.tbl$lon-360,
                        remap.tbl$lon)

#subset north atlantic raster
remap.NA <- subset(remap.tbl,lon>-60 & lon<30 & lat > 30)

geo.r <- raster(extent(-50,0,50,75))
res(geo.r) <- c(1,1)

r.coords.x <- interp(remap.NA$lon,remap.NA$lat,remap.NA$x,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords.y <- interp(remap.NA$lon,remap.NA$lat,remap.NA$y,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords <- expand.grid(lon=xFromCol(geo.r),
                        lat=yFromRow(geo.r))
r.coords$x <- as.vector(r.coords.x$z)
r.coords$y <- as.vector(r.coords.y$z)

r.coords.sp <- as_tibble(r.coords)
coordinates(r.coords.sp) <- ~x +y
#important to make sure extract is from right package
r.coords$pp <- raster::extract(gn[[1]],r.coords.sp,method="bilinear")
geo.r <- setValues(geo.r,r.coords$gn)
plot(geo.r)
map("worldHires",add=TRUE)


## trying something different -----------

matrix <- readRDS("~/senior_thesis/plotting_dataframes/NPP/CESM_mean_npp_lt.Rds")

setwd("~/senior_thesis/combined_CESM_files/")
nc <- "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
nc_file <- nc_open("pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc")

lat.old = ncvar_get(nc_file, "nlat")
lon.old = ncvar_get(nc_file, "nlon")

dimnames(matrix) = list(lon = lon.old, lat = lat.old)
old.df = melt(matrix)

test <-bind_cols(remap.tbl,old.df)

head(old.df)

r = rasterFromXYZ(old.df)

# define resolution of the new grid
dlat.new = 1.9 # new delta lat
nlat.new = 180  # number of new lat
dlon.new = 2.5 # new delta lon
nlon.new = 360 # number of new lon

# make raster for the new grid
s = raster(nrow = nlat.new, ncol = nlon.new)

# use resample to regridded data
t = raster::resample(r, s, method = "bilinear")

new.matrix = as.matrix(t)

# or, further, into a dataframe
new.df = melt(new.matrix)

head(new.df)

test %>% ggplot(aes(x = x, y = y, fill = value)) + 
  geom_raster(interpolate = FALSE) +  # adding heat maps
  scale_fill_viridis_b(na.value = NA ) + # change color
  borders() + # adding country borders
  coord_equal(expand = FALSE) # keeping a nice aspect ratio




## plots regridded CESM mean long-term NPP data in North Atlantic -------------

matrix <- readRDS("~/senior_thesis/plotting_dataframes/NPP/CESM_mean_npp_lt.Rds")

setwd("~/senior_thesis/combined_CESM_files/")
nc <- "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
nc_file <- nc_open("pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc")

lat.old = ncvar_get(nc_file, "nlat")
lon.old = ncvar_get(nc_file, "nlon")

dimnames(matrix) = list(lon = lon.old, lat = lat.old)
old.df = melt(matrix)

r = rasterFromXYZ(old.df)

#extract coordinates at each grid cell
lon <- raster(nc,varname="lon")
lat <- raster(nc,varname="lat")

#convert grid cells and corresponding lat-lon to a data.frame
library(reshape)
remap.tbl <- data.frame(coordinates(lon),
                        lon=as.vector(lon),lat=as.vector(lat))
tail(remap.tbl)

#correct lon scale
remap.tbl$lon <- ifelse(remap.tbl$lon>180,
                        remap.tbl$lon-360,
                        remap.tbl$lon)
range(remap.tbl$lon)

#shows entire model native grid
library(maps)
map("world",fill=TRUE,mar=c(0,0,0,0))
points(lat ~lon,remap.tbl,pch=".",col="red")

library(akima)
#sample points from North Atlantic
remap.NA <- subset(remap.tbl,lon>-60 & lon<30 & lat > 30)

geo.r <- raster(extent(-50,0,50,75))
res(geo.r) <- c(1,1)

r.coords.x <- interp(remap.NA$lon,remap.NA$lat,remap.NA$x,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords.y <- interp(remap.NA$lon,remap.NA$lat,remap.NA$y,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords <- expand.grid(lon=xFromCol(geo.r),
                        lat=yFromRow(geo.r))
r.coords$x <- as.vector(r.coords.x$z)
r.coords$y <- as.vector(r.coords.y$z)

r.coords.sp <- r.coords
coordinates(r.coords.sp) <- ~x +y
#important to make sure extract is from right package
r.coords$pp <- raster::extract(r[[1]],r.coords.sp,method="bilinear")
geo.r <- setValues(geo.r,r.coords$pp)
#not sure why but the plot function stopped working
plot(geo.r)
map("worldHires",add=TRUE)

# First, to a SpatialPointsDataFrame
geo.r.pts <- rasterToPoints(geo.r, spatial = TRUE)
# Then to a 'conventional' dataframe
df  <- data.frame(geo.r.pts)

world <- ne_countries(scale = "medium", returnclass = "sf")

#map of regridded NA subset
map <- ggplot() +
  geom_raster(data = df , aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = world) +
  coord_sf(xlim = c(-50, 0), ylim = c(50, 75)) +
  labs(title = "CESM long-term (2079-2099) NPP",
       subtitle = "Regridded to regular 1x1 degree",
       fill = "NPP (mol m-2 yr-1)") +
  scale_fill_cmocean(limits = c(0,10), oob = squish, name = "deep", direction = 1) +
  xlab("Longitude") +
  ylab("Latitude")
  
ggsave("~/senior_thesis/figures/regridded_CESM_lt_npp_NA.png", map, width = 12, height = 14, units = "cm", dpi = 500)

# plotting for the whole world  -------------


matrix <- readRDS("~/senior_thesis/plotting_dataframes/NPP/CESM_mean_npp_lt.Rds")

setwd("~/senior_thesis/combined_CESM_files/")
nc <- "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
nc_file <- nc_open("pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc")

lat.old = ncvar_get(nc_file, "nlat")
lon.old = ncvar_get(nc_file, "nlon")

dimnames(matrix) = list(lon = lon.old, lat = lat.old)
old.df = melt(matrix)

r = rasterFromXYZ(old.df)

#extract coordinates at each grid cell
lon <- raster(nc,varname="lon")
lat <- raster(nc,varname="lat")

#convert grid cells and corresponding lat-lon to a data.frame
library(reshape)
remap.tbl <- data.frame(coordinates(lon),
                        lon=as.vector(lon),lat=as.vector(lat))
tail(remap.tbl)

#correct lon scale
remap.tbl$lon <- ifelse(remap.tbl$lon>180,
                        remap.tbl$lon-360,
                        remap.tbl$lon)
range(remap.tbl$lon)

library(akima)

geo.r <- raster()

remap.NA <- subset(remap.tbl, lon>-180 & lon<180 & lat>-90 & lat<90)

r.coords.x <- interp(remap.NA$lon,remap.NA$lat,remap.NA$x,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords.y <- interp(remap.NA$lon,remap.NA$lat,remap.NA$y,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords <- expand.grid(lon=xFromCol(geo.r),
                        lat=yFromRow(geo.r))
r.coords$x <- as.vector(r.coords.x$z)
r.coords$y <- as.vector(r.coords.y$z)

r.coords.sp <- r.coords
coordinates(r.coords.sp) <- ~x +y
#important to make sure extract is from right package
r.coords$pp <- raster::extract(r[[1]],r.coords.sp,method="bilinear")
geo.r <- setValues(geo.r,r.coords$pp)
plot(geo.r)
map("worldHires",add=TRUE)


## Double checking -----------

## plots regridded CESM mean long-term NPP data in North Atlantic -------------

matrix <- readRDS("~/senior_thesis/plotting_dataframes/NPP/CESM_mean_npp_lt.Rds")

setwd("~/senior_thesis/combined_CESM_files/")
nc <- "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
nc_file <- nc_open("pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc")

lat.old = ncvar_get(nc_file, "nlat")
lon.old = ncvar_get(nc_file, "nlon")

dimnames(matrix) = list(lon = lon.old, lat = lat.old)
old.df = melt(matrix)

r = rasterFromXYZ(old.df)

#extract coordinates at each grid cell
lon <- raster(nc,varname="lon")
lat <- raster(nc,varname="lat")

#convert grid cells and corresponding lat-lon to a data.frame
library(reshape)
remap.tbl <- data.frame(coordinates(lon),
                        lon=as.vector(lon),lat=as.vector(lat))
tail(remap.tbl)

#correct lon scale
remap.tbl$lon <- ifelse(remap.tbl$lon>180,
                        remap.tbl$lon-360,
                        remap.tbl$lon)
range(remap.tbl$lon)

#shows entire model native grid
library(maps)
map("world",fill=TRUE,mar=c(0,0,0,0))
points(lat ~lon,remap.tbl,pch=".",col="red")

library(akima)
#sample points from North Atlantic
remap.NA <- subset(remap.tbl,lon>-170 & lon<170 & lat > -80 & lat < 80)

geo.r <- raster(extent(-169,169,-79,79))
res(geo.r) <- c(1,1)

r.coords.x <- interp(remap.NA$lon,remap.NA$lat,remap.NA$x,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords.y <- interp(remap.NA$lon,remap.NA$lat,remap.NA$y,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords <- expand.grid(lon=xFromCol(geo.r),
                        lat=yFromRow(geo.r))
r.coords$x <- as.vector(r.coords.x$z)
r.coords$y <- as.vector(r.coords.y$z)

r.coords.sp <- r.coords
coordinates(r.coords.sp) <- ~x +y
#important to make sure extract is from right package
r.coords$pp <- raster::extract(r[[1]],r.coords.sp,method="bilinear")
geo.r <- setValues(geo.r,r.coords$pp)
#not sure why but the plot function stopped working
plot(geo.r)
map("worldHires",add=TRUE)

# First, to a SpatialPointsDataFrame
geo.r.pts <- rasterToPoints(geo.r, spatial = TRUE)
# Then to a 'conventional' dataframe
df  <- data.frame(geo.r.pts)

world <- ne_coastline(scale = "small", returnclass = "sf")

#map of regridded NA subset
map <- ggplot() +
  geom_raster(data = df , aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = world) +
  coord_sf(xlim = c(-160, 160), ylim = c(-70, 70)) +
  labs(title = "CESM long-term (2079-2099) NPP",
       subtitle = "Regridded to regular 1x1 degree",
       fill = "NPP (mol m-2 yr-1)") +
  scale_fill_cmocean(limits = c(0,20), oob = squish, name = "deep", direction = 1) +
  xlab("Longitude") +
  ylab("Latitude")

map

ggsave("~/senior_thesis/figures/regridded_CESM_lt_npp_world.png", map, width = 20, height = 10, units = "cm", dpi = 500)

## Regrid without subset -------------------

matrix <- readRDS("~/senior_thesis/plotting_dataframes/NPP/CESM_mean_npp_lt.Rds")

setwd("~/senior_thesis/combined_CESM_files/")
nc <- "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
nc_file <- nc_open("pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc")

lat.old = ncvar_get(nc_file, "nlat")
lon.old = ncvar_get(nc_file, "nlon")

dimnames(matrix) = list(lon = lon.old, lat = lat.old)
old.df = melt(matrix)

r = rasterFromXYZ(old.df)

#extract coordinates at each grid cell
lon <- raster(nc,varname="lon")
lat <- raster(nc,varname="lat")

#convert grid cells and corresponding lat-lon to a data.frame
library(reshape)
remap.tbl <- data.frame(coordinates(lon),
                        lon=as.vector(lon),lat=as.vector(lat))
tail(remap.tbl)

#correct lon scale
remap.tbl$lon <- ifelse(remap.tbl$lon>180,
                        remap.tbl$lon-360,
                        remap.tbl$lon)
range(remap.tbl$lon)
range(remap.tbl$lat)

#shows entire model native grid
library(maps)
map("world",fill=TRUE,mar=c(0,0,0,0))
points(lat ~lon,remap.tbl,pch=".",col="red")

library(akima)
#sample points from North Atlantic
#remap.NA <- subset(remap.tbl,lon>-170 & lon<170 & lat > -80 & lat < 80)

geo.r <- raster(extent(-178,178,-79,88))
res(geo.r) <- c(1,1)

r.coords.x <- interp(remap.tbl$lon,remap.tbl$lat,remap.tbl$x,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords.y <- interp(remap.tbl$lon,remap.tbl$lat,remap.tbl$y,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords <- expand.grid(lon=xFromCol(geo.r),
                        lat=yFromRow(geo.r))
r.coords$x <- as.vector(r.coords.x$z)
r.coords$y <- as.vector(r.coords.y$z)

r.coords.sp <- r.coords
coordinates(r.coords.sp) <- ~x +y
#important to make sure extract is from right package
r.coords$pp <- raster::extract(r[[1]],r.coords.sp,method="bilinear")
geo.r <- setValues(geo.r,r.coords$pp)
#not sure why but the plot function stopped working
plot(geo.r)
map("worldHires",add=TRUE)

# First, to a SpatialPointsDataFrame
geo.r.pts <- rasterToPoints(geo.r, spatial = TRUE)
# Then to a 'conventional' dataframe
df  <- data.frame(geo.r.pts)

world <- ne_coastline(scale = "medium", returnclass = "sf")

#map of regridded NA subset
map <- ggplot() +
  geom_raster(data = df , aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = world, fill = "grey70") +
  coord_sf(xlim = c(-178, 178), ylim = c(-79, 88)) +
  labs(title = "CESM long-term (2079-2099) NPP",
       subtitle = "Regridded to regular 1x1 degree",
       fill = "NPP (mol m-2 yr-1)") +
  scale_fill_cmocean(limits = c(0,20), oob = squish, name = "deep", direction = 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()

map

ggsave("~/senior_thesis/figures/regridded_CESM_lt_npp_world.png", map, width = 20, height = 10, units = "cm", dpi = 500)


## duplicating matrix to get rid of seam -----------


matrix <- readRDS("~/senior_thesis/plotting_dataframes/NPP/CESM_mean_npp_lt.Rds")
matrix2 <- rbind(matrix,matrix)

setwd("~/senior_thesis/combined_CESM_files/")
nc <- "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
nc_file <- nc_open("pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc")

lat.old = ncvar_get(nc_file, "nlat")
lon.old = ncvar_get(nc_file, "nlon")
lon.old = cbind(lon.old,lon.old)

dimnames(matrix2) = list(lon = lon.old, lat = lat.old)
old.df = melt(matrix2)

r = rasterFromXYZ(old.df)

#extract coordinates at each grid cell
lon <- raster(nc,varname="lon")
lat <- raster(nc,varname="lat")

#convert grid cells and corresponding lat-lon to a data.frame
library(reshape)
remap.tbl <- data.frame(coordinates(lon),
                        lon=as.vector(lon),lat=as.vector(lat))
tail(remap.tbl)

#correct lon scale
remap.tbl$lon <- ifelse(remap.tbl$lon>180,
                        remap.tbl$lon-360,
                        remap.tbl$lon)
range(remap.tbl$lon)
range(remap.tbl$lat)

geo.r <- raster(extent(-178, 180, -79, 88))
res(geo.r) <- c(1,1)

r.coords.x <- interp(remap.tbl$lon,remap.tbl$lat,remap.tbl$x,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords.y <- interp(remap.tbl$lon,remap.tbl$lat,remap.tbl$y,
                     xo=xFromCol(geo.r),yo=yFromRow(geo.r))
r.coords <- expand.grid(lon=xFromCol(geo.r),
                        lat=yFromRow(geo.r))
r.coords$x <- as.vector(r.coords.x$z)
r.coords$y <- as.vector(r.coords.y$z)

r.coords.sp <- r.coords
coordinates(r.coords.sp) <- ~x +y
#important to make sure extract is from right package
r.coords$pp <- raster::extract(r[[1]],r.coords.sp,method="bilinear")
geo.r <- setValues(geo.r,r.coords$pp)
#not sure why but the plot function stopped working
plot(geo.r)
map("worldHires",add=TRUE)

# First, to a SpatialPointsDataFrame
geo.r.pts <- rasterToPoints(geo.r, spatial = TRUE)
# Then to a 'conventional' dataframe
df  <- data.frame(geo.r.pts)

world <- ne_coastline(scale = "medium", returnclass = "sf")

#map of regridded NA subset
map <- ggplot() +
  geom_raster(data = df , aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = world, fill = "grey70") +
  coord_sf(xlim = c(-178, 178), ylim = c(-79, 88)) +
  labs(title = "CESM long-term (2079-2099) NPP",
       subtitle = "Regridded to regular 1x1 degree",
       fill = "NPP (mol m-2 yr-1)") +
  scale_fill_cmocean(limits = c(0,20), oob = squish, name = "deep", direction = 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()

map

ggsave("~/senior_thesis/figures/regridded_CESM_lt_npp_world.png", map, width = 20, height = 10, units = "cm", dpi = 500)



