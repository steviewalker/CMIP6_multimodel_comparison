## Regrid entire map -------------------


# List of packages needed
.packages = c("here","ncdf4","raster","tidyverse")


# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session
lapply(.packages, require, character.only=TRUE)


#install nco

system("sudo apt-get install nco", wait = TRUE)

#open nc to check variable names
setwd("~/senior_thesis/combined_CESM_files/")
nc <- "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
nc_file <- nc_open(nc)

# Create Raster map of this variable:
raster.nc <- raster(nc,  varname = "pp", stopIfNotEqualSpaced = FALSE)
proj4string(raster.nc) = "+proj=longlat +datum=WGS84"

# Deal with fact that we want West Longitude as negative longitude,
# http://nco.sourceforge.net/nco.html#msa_usr_rdr

#longitude
system("ncks -O --msa -d nlon,180.,360. -d nlon,0.,179.999 pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc out.nc", wait = TRUE)

raster.nc.test <- raster("out.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test)

system("ncap2 -O -s 'where(nlon > 180) nlon=nlon-360' out.nc out180.nc", wait = TRUE)

raster.nc.test.2 <- raster("out180.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.2)

#latitude, which for some reason was on a 0.5-384.5 scale

system("ncap2 -O -s 'where(nlat > 0) nlat=nlat/2.133333' out180.nc out90lat.nc", wait = TRUE)
raster.nc.test.3 <- raster("out90lat.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.3)


system("ncap2 -O -s 'where(nlat < 89.999) nlat=nlat-89.999' out90lat.nc out90lat2.nc", wait = TRUE)
raster.nc.test.4 <- raster("out90lat2.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.4)


system("ncap2 -O -s 'where(nlat > 89.999) nlat=nlat-89.999' out90lat2.nc out90lat3.nc", wait = TRUE)
raster.nc.test.5 <- raster("out90lat3.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.5)


### STEVIE'S TROUBLESHOOTING ---------------

df = "~/senior_thesis/plotting_dataframes/NPP/CESM_mean_npp_lt.Rds"
nc.file = "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
model.name = "CESM"
lat.name = "nlat"
lon.name = "nlon"

matrix <- readRDS(df)

wd = paste0("~/senior_thesis/combined_", model.name,"_files/")
setwd(wd)
nc_file <- nc_open(nc.file)

#pulls out corresponding lat and lon values for cells in the native grid
lat.old = ncvar_get(nc_file, lat.name)
lon.old = ncvar_get(nc_file, lon.name)

dimnames(matrix) = list(lon = lon.old, lat = lat.old)
old.df = melt(matrix)

r = rasterFromXYZ(old.df)
writeRaster(r, filename = "pp_lt_CESM.nc", varname = "pp", xname = "nlon", yname = "nlat", overwrite = TRUE)
output_pp <- nc_open("pp_lt_CESM.nc")
print(output_pp)
t <- raster("pp_lt_CESM.nc", varname = "pp")

system("ncap2 -O -s 'where(nlon > 180) nlon=nlon-360' pp_lt_CESM.nc out180.nc", wait = TRUE)

raster.nc.test.2 <- raster("out180.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.2)


res(raster.nc.test.5) = c(1,1)
proj4string(raster.nc.test.5) = "+proj=longlat +datum=WGS84"
proj4string(r) = "+proj=longlat +datum=WGS84"

#geo.r = raster(extent(-178,178,-89,89), res = c(1,1))

## Trying to regrid matrix to this new regular grid nc file -----------

#two ways to do the same thing
test <- raster::resample(r, raster.nc.test.5, method = "bilinear")
test2 <- projectRaster(from = r, to = raster.nc.test.5, 
                       res = c(1,1), crs = "+proj=longlat +datum=WGS84",method = "bilinear")

writeRaster(x = test, "output_pp.nc", varname = "pp",overwrite = TRUE)

#test <- projectExtent(raster.nc.test.5, crs = "+proj=longlat +datum=WGS84")

test2 <- projectRaster(from = r, to = raster.nc.test.5, 
                      res = c(1,1), crs = "+proj=longlat +datum=WGS84",method = "bilinear")

plot(test2)

test <- raster::merge(r,raster.nc.test)

r22 <- raster(vals=values(r),ext=extent(raster.nc.test.5),crs=crs(raster.nc.test.5),
              nrows=dim(raster.nc.test.5)[1],ncols=dim(raster.nc.test.5)[2])

#Stevie's test
system("ncap2 -O -s 'where(nlon > 180) nlon=nlon-360' output_pp.nc out180.nc", wait = TRUE)

raster.nc.test.2 <- raster("out180.nc",  varname = "nlon", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.2)


#longitude
system("ncks -O --msa -d nlon,180.,360. -d nlon,0.,179.999 pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc out.nc", wait = TRUE)

raster.nc.test <- raster("out.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test)

test <- raster::resample(r, raster.nc.test, method = "bilinear")

setwd("~/senior_thesis/combined_CESM_files/")
file.copy(from = "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc" , to = "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100_2.nc")

output_pp <- nc_open("pp_lt_CESM.nc")
nc_file2 <- nc_open("pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100_2.nc",write = TRUE)
ncvar_put(nc_file2, varid = c("nlon","nlat"), vals = output_pp, start = c())



## I'm gonna shit my pants if this actually worked -----------


df = "~/senior_thesis/plotting_dataframes/NPP/CESM_mean_npp_lt.Rds"
nc.file = "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
model.name = "CESM"
lat.name = "nlat"
lon.name = "nlon"

#read in native grid matrix
matrix <- readRDS(df)

#open nc file the matrix was derived from
wd = paste0("~/senior_thesis/combined_", model.name,"_files/")
setwd(wd)
nc_file <- nc_open(nc.file)

#pulls out corresponding lat and lon values for cells in the native grid
lat.old = ncvar_get(nc_file, lat.name)
lon.old = ncvar_get(nc_file, lon.name)

dimnames(matrix) = list(lon = lon.old, lat = lat.old)
old.df = melt(matrix)

r = rasterFromXYZ(old.df)

#save as nc file
writeRaster(r, filename = "pp_lt_CESM.nc", varname = "pp", xname = "nlon", yname = "nlat", overwrite = TRUE)

output_pp <- nc_open("pp_lt_CESM.nc")
print(output_pp)
t <- raster("pp_lt_CESM.nc", varname = "pp")
 
#longitude
system("ncks -O --msa -d nlon,180.,360. -d nlon,0.,179.999 pp_lt_CESM.nc out.nc", wait = TRUE)

raster.nc.test <- raster("out.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test)

system("ncap2 -O -s 'where(nlon > 180) nlon=nlon-360' out.nc out180.nc", wait = TRUE)

raster.nc.test.2 <- raster("out180.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.2)

#latitude, which for some reason was on a 0.5-384.5 scale

system("ncap2 -O -s 'where(nlat > 0) nlat=nlat/2.133333' out180.nc out90lat.nc", wait = TRUE)
raster.nc.test.3 <- raster("out90lat.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.3)


system("ncap2 -O -s 'where(nlat < 89.999) nlat=nlat-89.999' out90lat.nc out90lat2.nc", wait = TRUE)
raster.nc.test.4 <- raster("out90lat2.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.4)


system("ncap2 -O -s 'where(nlat > 89.999) nlat=nlat-89.999' out90lat2.nc out90lat3.nc", wait = TRUE)
raster.nc.test.5 <- raster("out90lat3.nc",  varname = "pp", stopIfNotEqualSpaced = FALSE)
plot(raster.nc.test.5)

res(raster.nc.test.5) = c(1,1)

geo.r <- raster(extent(-179.5627, 180.5627, -89.76462, 90.2354),
                crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                res = c(1,1))

test <- raster::resample(raster.nc.test.5, geo.r, method = "bilinear")

plot(test)

# First, to a SpatialPointsDataFrame
geo.r.pts <- rasterToPoints(test, spatial = TRUE)
# Then to a 'conventional' dataframe
df  <- data.frame(geo.r.pts)

world <- ne_coastline(scale = "medium", returnclass = "sf")
title = paste(model.name, "long-term (2079-2099) NPP", sep = " ")

#map of regridded NA subset
map <- ggplot() +
  geom_tile(data = df , aes(x = x, y = y, fill = pp)) + 
  geom_sf(data = world, fill = "grey70") +
  coord_sf(xlim = c(-178, 178), ylim = c(-79, 88), expand = FALSE) +
  labs(title = title,
       subtitle = "Regridded to regular 1x1 degree",
       fill = "NPP (mol m-2 yr-1)") +
  scale_fill_cmocean(limits = c(0,20), oob = squish, name = "deep", direction = 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()

map

ggsave(paste0("~/senior_thesis/figures/regridded/regridded_",model.name,"_lt_npp_world_2.png"), map, width = 20, height = 10, units = "cm", dpi = 500)


