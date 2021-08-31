#' @title Ocean month to ocean year converter
#' @description take the average of Omon_expc files to make them Oyr before combining
#' @author Stevie Walker
#' @date 7/12/21


#open nc file
setwd("~/senior_thesis/UKESM_data")
nc_data <- nc_open('expc_Omon_UKESM1-0-LL_ssp585_r1i1p1f2_gn_201501-204912.nc')

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

#STUCK HERE
expc <- ncvar_get(nc_data,"expc", start = c(1,1,1,5), count = c(-1,-1,-1,12))
