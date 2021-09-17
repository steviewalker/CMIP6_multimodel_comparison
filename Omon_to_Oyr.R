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
expc <- ncvar_get(nc_data,"expc", start = c(1,1,1,30), count = c(-1,-1,-1,12))

#short term sequence
v <- seq(from = 30, to = 258, by = 12)
n = length(v)

#create list to store for loop output in
list_yr <- list()

for(i in 1:n) {
  #read in a year of MLD data
  t <- v[i]
  var <- ncvar_get(nc_data,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,12))
  #calculate the average expc each year at each lat, lon, and depth
  avg <- apply(var, c(1,2,3),mean,na.rm=FALSE)
  #store output into the list
  list_yr[[i]] <- avg
}

nc_create('expc_Oyr_UKESM1-0-LL_ssp585_r1i1p1f2_gn_201501-204912.nc',list_yr, force_v4 = TRUE, verbose= TRUE)

ncdim_def()

Y <- do.call(cbind, list_yr)
Y <- array(Y, dim=c(dim(list_yr[[1]]), length(list_yr)))

saveRDS(Y, file = "UKESM_expc_yr_st.Rds", ascii = TRUE)

