#' @title Calculate long-term POC flux at maximum annual MLD
#' @author Stevie Walker
#' @date 7/27/21
#' @description finds the long-term 20 year average and change in POC flux 
#' at the maximum annual MLD for CMIP6 earth system models
#' @note this function takes a very long time to run

#model.name = "CESM"
#wd = "~/senior_thesis/combined_CESM_files"
#nc.file = 'expc_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc'
#start.lt = 65
#lon.name = "nlon"
#lat.name = "nlat"

calc_expc_avg_lt <- function(wd, nc.file, model.name, start.lt, lon.length, lat.length) {


setwd(wd)
nc_data <- nc_open(nc.file)


#read in MLDmax arrays - MLDmax for each year (these objects come from calc_MLD_max.R)
MLD_max_lt <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_array_MLD_max_lt.Rds",sep = ""))


## LONG-TERM FOR LOOP ----------------

v <- seq(from = start.lt, to = start.lt + 19, by = 1)
n = length(v)

#storage container for list of matrices
list_expc <- list()

#creates list of 20 arrays with POC flux at every lat,lon,depth
for(k in 1:length(v)) {
  #read in a year of data
  t <- v[k]
  #pulls out array for one year, 3D with lat,lon,depth
  expc2015 <- ncvar_get(nc_data,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
  
  #get longitude and latitude
  lon <- ncvar_get(nc_data, lon.name) #320
  lat <- ncvar_get(nc_data, lat.name) #384
  
  #storage container for second for loop
  output <- matrix(nrow = length(lon), ncol = length(lat))
  
  #calculates POC flux at MLD max for every grid cell for one year
  for(i in 1:length(lon)) {
    for(j in 1:length(lat)) {
      
      #make list and add needed columns
      ret <- list()
      ret$depth <-  ncvar_get(nc_data, "lev")/100
      #subset expc for select lat and lon
      ret$expc <- extract(expc2015, indices = c(i,j), dims = c(1,2))
      #subset MLD max for each lat and lon
      ret$MLD <- extract(MLD_max_lt[, , k], indices = c(i,j), dims = c(1,2))
      
      #melt data into data frame format and combine by lat and lon - NOTE: Must manually go in and make sure the dimnames match what's in the nc file metadata
      melt_depth <- function(L) {
        dimnames(L$expc) <- list(lon = L$longitude, lat = L$latitude, depth = L$lev)
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
expc_lt <- array(expc_lt, dim=c(dim(list_expc[[1]]), length(list_max)))

#20 year mean for the end of the 21st century
mean_expc_lt <- apply(expc_lt, c(1, 2), mean, na.rm = FALSE)

#read in short-term POC flux at MLDmax - be sure to run the short term function first
readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_mean_expc_lt.Rds",sep = ""))

#change in POC flux at the MLD max
expc_change = mean_expc_lt - mean_expc_st

#save matrices for plotting
setwd("~/senior_thesis/plotting_dataframes/")
saveRDS(mean_expc_lt, file = paste(model.name,"_mean_expc_lt.Rds",sep=""), ascii = TRUE)
saveRDS(expc_change, file = paste(model.name,"_expc_change.Rds",sep=""), ascii = TRUE)

}