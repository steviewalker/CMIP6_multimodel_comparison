#' @title Calculate short-term POC flux at maximum annual MLD for UKESM
#' @author Stevie Walker
#' @date 9/13/21
#' @description finds the short-term 20 year average in POC flux at the maximum annual MLD for CMIP6 earth system models
#' @note this function takes a very long time to run
#' @note UKESM is not run using the function calc_expc_avg because you need to take the yearly average first (UKESM expc file is monthly, all the others are yearly)

setwd("~/senior_thesis/")
source('libraries.R')

#note, this comes from a different directory than short-term UKESM because this was combined
setwd("~/senior_thesis/combined_UKESM_files")
nc_data <- nc_open('expc_Omon_UKESM1-0-LL_ssp585_r1i1p1f2_gn_205001-210012.nc')

#read in MLDmax arrays - MLDmax for each year (these objects come from calc_MLD_max.R)
MLD_max_st <- readRDS("~/senior_thesis/plotting_dataframes/UKESM_array_MLD_max_lt.Rds")

## SHORT-TERM FOR LOOP ----------------

v <- seq(from = 365, to = 596, by = 12)
n = length(v)

#storage container for list of matrices
list_expc <- list()

#creates list of 20 arrays with POC flux at every lat,lon,depth
for(k in 1:length(v)) {
  #read in a year of data
  t <- v[k]
  #pulls out array for one year, 3D with lat,lon,depth
  expc2015 <- ncvar_get(nc_data,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,12))*31536000
  expc2015 <- apply(expc2015, c(1,2,3),mean,na.rm=FALSE)
  
  #get longitude and latitude
  #lon <- ncvar_get(nc_data, lon.name) #320 for CESM
  #lat <- ncvar_get(nc_data, lat.name) #384 for CESM
  
  lon <- 360
  lat <- 330
  
  #storage container for second for loop
  output <- matrix(nrow = length(lon), ncol = length(lat))
  
  #calculates POC flux at MLD max for every grid cell for one year
  for(i in 1:length(lon)) {
    for(j in 1:length(lat)) {
      
      #make list and add needed columns
      ret <- list()
      ret$depth <-  ncvar_get(nc_data, "lev")
      #subset expc for select lat and lon
      ret$expc <- extract(expc2015, indices = c(i,j), dims = c(1,2))
      #subset MLD max for each lat and lon
      ret$MLD <- extract(MLD_max_st[, , k], indices = c(i,j), dims = c(1,2))
      
      
      #melt data into data frame format and combine by lat and lon - NOTE: Must manually go in and make sure the dimnames match what's in the nc file metadata
      melt_depth <- function(L) {
        dimnames(L$expc) <- list(lon = L$longitude, lat = L$latitude, depth = L$depth)
        dimnames(L$MLD) <- list(lon = L$longitude, lat = L$latitude)
        ret <- melt(L$expc, value.name = "expc")
        ret2 <- melt(L$MLD, value.name = "mlotst")
        df2 <- merge(ret,ret2,by=c("lat","lon"))
      }
      
      #creates df with columns lat,lon,depth,expc - nrows = number of depth observations (ex. 60 obs. for CESM)
      profile_2015 <- melt_depth(ret)
      
      #put values of lat and lon back into df (not sure why melt_depth makes them 1's)
      profile_2015$lat = j
      profile_2015$lon = i
      
      #if expc values exist in the data frame, then interpolate (aka if this is a plot point on the model interpolate)
      if (is.na(profile_2015$expc) == FALSE) {
        
        #produces two column df with 60 rows of interpolated expc at mld max (all 60 values are the same)
        interp <- approx(x = profile_2015$depth, y = profile_2015$expc, xout = profile_2015$mlotst, method = "linear")
        #extracts one value of expc and mld max
        expc <- interp$y[1]
        mlotst <- interp$x[1]
        
        #store interpolated POC flux into the output matrix
        output[i, j] <- interp$y[1]
        
        ##if expc values DON'T exist in the data frame, then just assign NA to the new output matrix
      } else {
        output[i,j] <- NA
      }
      
    }
  }  
  
  
  list_expc[[k]] <- output
}

#converts from list to matrices
expc_lt <- do.call(cbind, list_expc)
#combines matrices into a single array
expc_lt <- array(expc_lt, dim=c(dim(list_expc[[1]]), length(list_expc)))

#20 year mean for the beginning of the 21st century
mean_expc_lt <- apply(expc_lt, c(1, 2), mean, na.rm = FALSE)

#save matrices for plotting
setwd("~/senior_thesis/plotting_dataframes/")
saveRDS(mean_expc_lt, file = "UKESM_mean_expc_lt.Rds", ascii = TRUE)
