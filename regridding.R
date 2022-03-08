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

firstRaster <- raster(xmn = -180,   # set minimum x coordinate
                      xmx = 180,    # set maximum x coordinate
                      ymn = -90,     # set minimum y coordinate
                      ymx = 90,     # set maximum y coordinate
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

# after this is where I am lost

#empty raster that matches CESM grid
gn <- raster(xmn = 1,   # set minimum x coordinate
             xmx = 320,    # set maximum x coordinate
             ymn = 1,     # set minimum y coordinate
             ymx = 384,     # set maximum y coordinate
             res = c(1,1)) # resolution in c(x,y) direction

#add npp data
gn[] <- pp

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
