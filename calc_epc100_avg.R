#' @title Calculate average POC flux at 100m
#' @author Stevie Walker
#' @date 7/14/21
#' @description finds the 20 year short-term and long-term average global POC flux and calculates the difference between them
#' @note be sure to run function get_time first so you know what to make start.st and start.lt
#' @source used code from code from https://www.researchgate.net/post/How-can-I-extract-a-time-series-of-variable-for-a-specific-location-specific-longitude-and-latitude-from-a-CMIP5-experiment-netCDF-data-set

calc_epc100_avg <- function(data.path, file.name, start.st, start.lt, save.name) {

## Calculating short term average (2014-2034) ---------------

#open nc file
setwd(data.path)
nc_data <- nc_open(file.name)


#get variable at specified time range (12 months x 20 years = 240 months, aka 240 time count)
variable_st <- ncvar_get(nc_data,"epc100",start= start.st, count = c(-1,-1, 240))

#calculate average global POC flux for every month between 2014-2034 NOTE: come back and try and plot this later for selected regions
var_st <- apply(variable_st,3,mean,na.rm=TRUE)

#calculate average POC flux for each grid cell over the years 2014-2034
var_average1 <- apply(variable_st, c(1,2),mean,na.rm=FALSE)

#converted value to plot
var_year1 = var_average1*31536000

## Calculating long-term average (2079-2099) and change in epc100 -----------


#get variable at specified time range (12 months x 20 years = 240 months, 1023 months - 240 months)
#starting at Jan 2079

variable_lt <- ncvar_get(nc_data,"epc100",start= start.lt, count = c(-1,-1,240))

#calculate average POC flux for each grid cell over the years 2013-2033
var_average2 <- apply(variable_lt, c(1,2),mean,na.rm=FALSE)

#convert from mol/m2/s to mol/m2/yr I need help here
var_year2 = var_average2*31536000

#calculate change in average POC flux between beginning and end of 21st c
var_difference = var_average2 - var_average1

#for plotting
var_difference_yr = var_difference*31536000

setwd("~/senior_thesis/plotting_dataframes/")
saveRDS(var_year1, file = paste(save.name,"_st.Rds", sep=""), ascii = TRUE)
saveRDS(var_year2, file = paste(save.name,"_lt.Rds", sep=""), ascii = TRUE)
saveRDS(var_difference_yr, file = paste(save.name,"_diff.Rds", sep=""), ascii = TRUE)

}
