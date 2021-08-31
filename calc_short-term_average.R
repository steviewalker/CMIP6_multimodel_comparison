
#code from https://www.researchgate.net/post/How-can-I-extract-a-time-series-of-variable-for-a-specific-location-specific-longitude-and-latitude-from-a-CMIP5-experiment-netCDF-data-set


#open nc file
setwd("~/senior_thesis/combined_CESM_files")
nc_data <- nc_open('epc100_Omon_CESM2_ssp585_r10i1p1f1_gn_201501-210012.nc')

#get longitude and latitude
lon <- ncvar_get(nc_data, "nlon")
lat <- ncvar_get(nc_data, "nlat")

#get time
time <- ncvar_get(nc_data, "time")

#listing units of time
tunits <- ncatt_get(nc_data,"time","units")
nt <- dim(time)

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
time.corrected <- chron(time,origin=c(tmonth, tday, tyear))

#lists the dates for the averages that are being calculated
print(time.corrected)

#use time.corrected to see the actual dates for the average you are calculating

#get variable at specified time range (12 months x 20 years = 240 months, aka 240 time count)
variable <- ncvar_get(nc_data,"epc100",start= c(1,1,1), count = c(-1,-1, 240))

#calculate average global POC flux for every month between 2013-2033 NOTE: come back and try and plot this later for selected regions
var_ts <- apply(variable,3,mean,na.rm=TRUE)

#calculate average POC flux for each grid cell over the years 2013-2033
var_average1 <- apply(variable, c(1,2),mean,na.rm=TRUE)

var_year1 = var_average1*31536000


# #basic plot of downward POC flux at the beginning of the 21st ce --------



#plot average POC flux for early 21st century
ret <- list()
ret$lat <- ncvar_get(nc_data, "nlat")
ret$lon <- ncvar_get(nc_data, "nlon") # - 360 # we need them as negative values
ret$time <- ncvar_get(nc_data, "time")
ret$epc100 <- var_average1

file_info <- ret

#melt data so I can plot it in ggplot
melt_epc100 <- function(L) {
  dimnames(L$epc100) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$epc100, value.name = "epc100")
}

mepc100 <- melt_epc100(file_info)

basic.plot<- ggplot(data = mepc100, aes(x = lon, y = lat, fill = epc100)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_gradientn(colours = rev(rainbow(7)), na.value = NA) +
  theme_bw()

basic.plot