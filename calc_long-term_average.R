#open nc file
setwd("~/senior_thesis/combined_CESM_files")
nc_data <- nc_open('epc100_Omon_CESM2_ssp585_r10i1p1f1_gn_201501-210012.nc')

#get longitude and latitude
lon <- ncvar_get(nc_data, "nlon")
lat <- ncvar_get(nc_data, "nlat")

#obtaining time
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



#get variable at specified time range (12 months x 20 years = 240 months, 1023 months - 240 months = 792th month start)
variable <- ncvar_get(nc_data,"epc100",start= c(1,1,793), count = c(-1,-1,-1))

#calculate average global POC flux for every month between 2013-2033 NOTE: come back and try and plot this later for selected regions
var_ts <- apply(variable,3,mean,na.rm=TRUE)

#calculate average POC flux for each grid cell over the years 2013-2033
var_average2 <- apply(variable, c(1,2),mean,na.rm=TRUE)

#convert from mol/m2/s to mol/m2/yr I need help here
var_year2 = var_average2*31536000

#calculate change in average POC flux between beginning and end of 21st c
var_difference = var_average2 - var_average1

var_difference_yr = var_difference*31536000

