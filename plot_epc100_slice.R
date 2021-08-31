library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

setwd("~/senior_thesis/combined_CESM_files")
nc_data <- nc_open('epc100_Omon_CESM2_ssp585_r10i1p1f1_gn_201501-210012.nc')

# Save the metadata to a text file
  sink('epc100_Omon_CESM2_ssp585_r10i1p1f1_gn_201501-210012.txt')
  print(nc_data)
  setwd("~/senior_thesis/ncfile_metadata/")
  sink()

#change back to current working directory
setwd("~/senior_thesis/combined_CESM_files")

#naming variable
dname <- "epc100"

#obtaining longitude
lon <- ncvar_get(nc_data, "nlon")
#lon[lon > 180] <- lon[lon > 180] - 360

#lists dimensions, first few and last few values of lon
dim(lon)
head(lon)
tail(lon)

#obtaining latitude
lat <- ncvar_get(nc_data, "nlat")

#obtaining time
time <- ncvar_get(nc_data, "time")

#listing units of time
tunits <- ncatt_get(nc_data,"time","units")
nt <- dim(time)

#get epc100
variable_array <- ncvar_get(nc_data,dname)

dlname <- ncatt_get(nc_data,dname,"long_name")
dunits <- ncatt_get(nc_data,dname,"units")
fillvalue <- ncatt_get(nc_data,dname,"_FillValue")
dim(variable_array)

# get global attributes
title <- ncatt_get(nc_data,0,"title")
institution <- ncatt_get(nc_data,0,"institution")
datasource <- ncatt_get(nc_data,0,"source")
references <- ncatt_get(nc_data,0,"references")
history <- ncatt_get(nc_data,0,"history")
Conventions <- ncatt_get(nc_data,0,"Conventions")

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
time.corrected <- chron(time,origin=c(tmonth, tday, tyear))

# replace netCDF fill values with NA's
variable_array[variable_array==fillvalue$value] <- NA
length(na.omit(as.vector(variable_array[,,1])))

# get a single slice or layer (January)
m <- 1
variable_slice <- variable_array[,,m]

# quick map
image(lon,lat,variable_slice, col=rev(brewer.pal(10,"RdBu")))

# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
levelplot(variable_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))



# Trying to plot epc100 in ggplot -----------------------------------------


the_file <- nc_open('epc100_Omon_CESM2_ssp585_r10i1p1f1_gn_201501-210012.nc')

# now, grab stuff out of the netcdf file and return it in a list
# called ret
ret <- list()
ret$lat <- ncvar_get(the_file, "nlat")
ret$lon <- ncvar_get(the_file, "nlon") # - 360 # we need them as negative values
ret$time <- ncvar_get(the_file, "time")
ret$epc100 <- variable_slice
#ret$variable_array <- ncvar_get(the_file,"epc100")
#ret$date <- paste("12-12-", year, sep = "")
nc_close(the_file)

file_info <- ret

melt_epc100 <- function(L) {
  dimnames(L$epc100) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$epc100, value.name = "epc100")
}

mepc100 <- melt_epc100(file_info)

basic.plot<- ggplot(data = mepc100, aes(x = lon, y = lat, fill = epc100)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_gradientn(colours = rev(rainbow(7)), na.value = NA) +
  theme_bw()
