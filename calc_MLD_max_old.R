setwd("~/senior_thesis/combined_CESM_files")
nc_data <- nc_open('mlotst_Omon_CESM2_ssp585_r10i1p1f1_gn_201501-210012.nc')


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

#get MLD from Jan 2014 on
#variable <- ncvar_get(nc_data,"mlotst",start= c(1,1,5), count = c(-1,-1,-1))

#extracts one year of data
#year <- extract(variable, indices = list(1:12), dims = 3)

#calculate maximum annual MLD for one year
#var_max <- apply(variable, c(1,2),max,na.rm=FALSE)

#get MLD all
#variable <- ncvar_get(nc_data,"mlotst")

#get MLD for one year (2014)
#variable <- ncvar_get(nc_data,"mlotst",start= c(1,1,5), count = c(-1,-1,12))

#calculate maximum annual MLD for one year
#var_max <- apply(variable, c(1,2),max,na.rm=FALSE)


## Calculating average maximum MLD for 2014-2034 ---------

#start.list <- c(5,17,29,41,53,65,77,89,101,113,125,137,149,161,173,185,197,209,221,233)

#var.names <- c("var2014","var2015","var2016","var2017","var2018","var2019","var2020",
 #              "var2021",	"var2022","var2023", "var2024",	"var2025",	"var2026",	"var2027",
 #              "var2028",	"var2029","var2030", "var2031",	"var2032",	"var2033")

#MLD <- "mlotst"

#command.list <- paste(var.names,"<- ncvar_get(nc_data,",MLD,",start= c(1,1,",start.list,"), count = c(-1,-1,12))", sep = "")

#I edited the list I generated and now will pull out the MLD for each year needed to calculate the short-term
#I know this is bad code, but I have no idea how to use for loops so I just did this
var2014<- ncvar_get(nc_data,"mlotst",start= c(1,1,5), count = c(-1,-1,12))
max2014 <- apply(var2014, c(1,2),max,na.rm=FALSE)

var2015<- ncvar_get(nc_data,"mlotst",start= c(1,1,17), count = c(-1,-1,12))
max2015 <- apply(var2015, c(1,2),max,na.rm=FALSE)

var2016<- ncvar_get(nc_data,"mlotst",start= c(1,1,29), count = c(-1,-1,12))
max2016 <- apply(var2016, c(1,2),max,na.rm=FALSE)

var2017<- ncvar_get(nc_data,"mlotst",start= c(1,1,41), count = c(-1,-1,12))
max2017 <- apply(var2017, c(1,2),max,na.rm=FALSE)

var2018<- ncvar_get(nc_data,"mlotst",start= c(1,1,53), count = c(-1,-1,12))
max2018 <- apply(var2018, c(1,2),max,na.rm=FALSE)

var2019<- ncvar_get(nc_data,"mlotst",start= c(1,1,65), count = c(-1,-1,12))
max2019 <- apply(var2019, c(1,2),max,na.rm=FALSE)

var2020<- ncvar_get(nc_data,"mlotst",start= c(1,1,77), count = c(-1,-1,12))
max2020 <- apply(var2020, c(1,2),max,na.rm=FALSE)

var2021<- ncvar_get(nc_data,"mlotst",start= c(1,1,89), count = c(-1,-1,12))
max2021 <- apply(var2021, c(1,2),max,na.rm=FALSE)

var2022<- ncvar_get(nc_data,"mlotst",start= c(1,1,101), count = c(-1,-1,12))
max2022 <- apply(var2022, c(1,2),max,na.rm=FALSE)

var2023<- ncvar_get(nc_data,"mlotst",start= c(1,1,113), count = c(-1,-1,12))
max2023 <- apply(var2023, c(1,2),max,na.rm=FALSE)

var2024<- ncvar_get(nc_data,"mlotst",start= c(1,1,125), count = c(-1,-1,12))
max2024 <- apply(var2024, c(1,2),max,na.rm=FALSE)

var2025<- ncvar_get(nc_data,"mlotst",start= c(1,1,137), count = c(-1,-1,12))
max2025 <- apply(var2025, c(1,2),max,na.rm=FALSE)

var2026<- ncvar_get(nc_data,"mlotst",start= c(1,1,149), count = c(-1,-1,12))
max2026 <- apply(var2026, c(1,2),max,na.rm=FALSE)

var2027<- ncvar_get(nc_data,"mlotst",start= c(1,1,161), count = c(-1,-1,12))
max2027 <- apply(var2027, c(1,2),max,na.rm=FALSE)

var2028<- ncvar_get(nc_data,"mlotst",start= c(1,1,173), count = c(-1,-1,12))
max2028 <- apply(var2028, c(1,2),max,na.rm=FALSE)

var2029<- ncvar_get(nc_data,"mlotst",start= c(1,1,185), count = c(-1,-1,12))
max2029 <- apply(var2029, c(1,2),max,na.rm=FALSE)

var2030<- ncvar_get(nc_data,"mlotst",start= c(1,1,197), count = c(-1,-1,12))
max2030 <- apply(var2030, c(1,2),max,na.rm=FALSE)

var2031<- ncvar_get(nc_data,"mlotst",start= c(1,1,209), count = c(-1,-1,12))
max2031 <- apply(var2031, c(1,2),max,na.rm=FALSE)

var2032<- ncvar_get(nc_data,"mlotst",start= c(1,1,221), count = c(-1,-1,12))
max2032 <- apply(var2032, c(1,2),max,na.rm=FALSE)

var2033<- ncvar_get(nc_data,"mlotst",start= c(1,1,233), count = c(-1,-1,12))
max2033 <- apply(var2033, c(1,2),max,na.rm=FALSE)



#combining arrays into a matrix and take the average
X <- list(max2014,max2015, max2016, max2017, max2018, max2019,max2020, max2021, max2022, max2023,
          max2024, max2025, max2026, max2027, max2028, max2029, max2030, max2031, max2032, max2033)
Y <- do.call(cbind, X)
Y <- array(Y, dim=c(dim(X[[1]]), length(X)))

#20 year mean for the beginning of the 21st century
mean_MLD_max_2014_2033 <- apply(Y, c(1, 2), mean, na.rm = FALSE)


## Calculating average max MLD for 2079-2099 ----------


#start.list <- c(786,798,810,822,834,846,858,870,882,894,906,918,930,942,954,966,978,990,1002,1014)

#var.names <- c("var2079","var2080","var2081","var2082","var2083","var2084","var2085",
    #           "var2086",	"var2087","var2088", "var2089",	"var2090",	"var2091",	"var2092",
    #           "var2093",	"var2094","var2095", "var2096",	"var2097",	"var2098")

#MLD <- "mlotst"

#command.list <- paste(var.names,"<- ncvar_get(nc_data,",MLD,",start= c(1,1,",start.list,"), count = c(-1,-1,12))", sep = "")


#Again, I know this isn't an optimal and time-saving way to do this, but it's as good as I know how to do for now
var2079<- ncvar_get(nc_data,"mlotst",start= c(1,1,786), count = c(-1,-1,12)) 
max2079 <- apply(var2079, c(1,2),max,na.rm=FALSE)

var2080<- ncvar_get(nc_data,"mlotst",start= c(1,1,798), count = c(-1,-1,12)) 
max2080 <- apply(var2080, c(1,2),max,na.rm=FALSE)

var2081<- ncvar_get(nc_data,"mlotst",start= c(1,1,810), count = c(-1,-1,12)) 
max2081 <- apply(var2081, c(1,2),max,na.rm=FALSE)

var2082<- ncvar_get(nc_data,"mlotst",start= c(1,1,822), count = c(-1,-1,12))
max2082 <- apply(var2082, c(1,2),max,na.rm=FALSE)

var2083<- ncvar_get(nc_data,"mlotst",start= c(1,1,834), count = c(-1,-1,12))
max2083 <- apply(var2083, c(1,2),max,na.rm=FALSE)

var2084<- ncvar_get(nc_data,"mlotst",start= c(1,1,846), count = c(-1,-1,12)) 
max2084 <- apply(var2084, c(1,2),max,na.rm=FALSE)

var2085<- ncvar_get(nc_data,"mlotst",start= c(1,1,858), count = c(-1,-1,12)) 
max2085 <- apply(var2085, c(1,2),max,na.rm=FALSE)

var2086<- ncvar_get(nc_data,"mlotst",start= c(1,1,870), count = c(-1,-1,12))
max2086 <- apply(var2086, c(1,2),max,na.rm=FALSE)

var2087<- ncvar_get(nc_data,"mlotst",start= c(1,1,882), count = c(-1,-1,12)) 
max2087 <- apply(var2087, c(1,2),max,na.rm=FALSE)

var2088<- ncvar_get(nc_data,"mlotst",start= c(1,1,894), count = c(-1,-1,12)) 
max2088 <- apply(var2088, c(1,2),max,na.rm=FALSE)

var2089<- ncvar_get(nc_data,"mlotst",start= c(1,1,906), count = c(-1,-1,12)) 
max2089 <- apply(var2089, c(1,2),max,na.rm=FALSE)

var2090<- ncvar_get(nc_data,"mlotst",start= c(1,1,918), count = c(-1,-1,12)) 
max2090 <- apply(var2090, c(1,2),max,na.rm=FALSE)

var2091<- ncvar_get(nc_data,"mlotst",start= c(1,1,930), count = c(-1,-1,12)) 
max2091 <- apply(var2091, c(1,2),max,na.rm=FALSE)

var2092<- ncvar_get(nc_data,"mlotst",start= c(1,1,942), count = c(-1,-1,12)) 
max2092 <- apply(var2092, c(1,2),max,na.rm=FALSE)

var2093<- ncvar_get(nc_data,"mlotst",start= c(1,1,954), count = c(-1,-1,12)) 
max2093 <- apply(var2093, c(1,2),max,na.rm=FALSE)

var2094<- ncvar_get(nc_data,"mlotst",start= c(1,1,966), count = c(-1,-1,12)) 
max2094 <- apply(var2094, c(1,2),max,na.rm=FALSE)

var2095<- ncvar_get(nc_data,"mlotst",start= c(1,1,978), count = c(-1,-1,12))
max2095 <- apply(var2095, c(1,2),max,na.rm=FALSE)

var2096<- ncvar_get(nc_data,"mlotst",start= c(1,1,990), count = c(-1,-1,12)) 
max2096 <- apply(var2096, c(1,2),max,na.rm=FALSE)

var2097<- ncvar_get(nc_data,"mlotst",start= c(1,1,1002), count = c(-1,-1,12))
max2097 <- apply(var2097, c(1,2),max,na.rm=FALSE)

var2098<- ncvar_get(nc_data,"mlotst",start= c(1,1,1014), count = c(-1,-1,12))
max2098 <- apply(var2098, c(1,2),max,na.rm=FALSE)

#combining arrays into a matrix and take the average
X <- list(max2079,max2080, max2081, max2082, max2083, max2084,max2085, max2086, max2087, max2088,
          max2089, max2090, max2091, max2092, max2093, max2094, max2095, max2096, max2097, max2098)
Y <- do.call(cbind, X)
Y <- array(Y, dim=c(dim(X[[1]]), length(X)))

#20 year mean for the end of the 21st century
mean_MLD_max_2079_2099 <- apply(Y, c(1, 2), mean, na.rm = FALSE)

MLD_change = mean_MLD_max_2079_2099 - mean_MLD_max_2014_2033

#saving rds objects
setwd("~/senior_thesis/plotting_dataframes/")
saveRDS(mean_MLD_max_2014_2033, file = "mean_MLD_max_2014_2033.Rds", ascii = TRUE)
saveRDS(mean_MLD_max_2079_2099, file = "mean_MLD_max_2079_2099.Rds", ascii = TRUE)
saveRDS(MLD_change, file = "MLD_change.Rds", ascii = TRUE)


# test plot of a slice ----------------------------------------------------



# get a single slice or layer (January)
m <- 1
variable_slice <- variable[,,m]

image(lon,lat,variable_slice, col=rev(brewer.pal(10,"RdBu")))

ret <- list()
ret$lat <- ncvar_get(nc_data, "nlat")
ret$lon <- ncvar_get(nc_data, "nlon") # - 360 # we need them as negative values
#ret$time <- ncvar_get(nc_data, "time")
ret$MLD <- var_max
#ret$variable_array <- ncvar_get(the_file,"epc100")
#ret$date <- paste("12-12-", year, sep = "")
nc_close(nc_data)

file_info <- ret

melt_mld <- function(L) {
  dimnames(L$MLD) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$MLD, value.name = "mlotst")
}

melt_mlotst <- melt_mld(file_info)

basic.plot<- ggplot(data = melt_mlotst, aes(x = lon, y = lat, fill = mlotst)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_gradientn(colours = "YlGnBu", na.value = NA) +
  theme_bw()