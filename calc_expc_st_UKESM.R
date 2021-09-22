#' @title Calculate short-term POC flux at maximum annual MLD for UKESM
#' @author Stevie Walker
#' @date 7/27/21
#' @description finds the short-term 20 year average in POC flux at the maximum annual MLD for CMIP6 earth system models
#' @note this function takes a very long time to run

setwd("~/senior_thesis/")
source('libraries.R')

setwd("~/senior_thesis/UKESM_data")
nc_data <- nc_open('expc_Omon_UKESM1-0-LL_ssp585_r1i1p1f2_gn_201501-204912.nc')

#read in MLDmax arrays - MLDmax for each year (these objects come from calc_MLD_max.R)
MLD_max_st <- readRDS("~/senior_thesis/plotting_dataframes/UKESM_array_MLD_max_st.Rds")

## SHORT-TERM FOR LOOP ----------------

v <- seq(from = 30, to = 258, by = 12)
n = length(v)

lon <- 1:360
lat <- 1:330

#storage container for list of matrices
list_expc <- list()
#storage container for second for loop
output <- matrix(nrow = length(lon), ncol = length(lat))

#creates list of 20 arrays with POC flux at every lat,lon,depth
for(k in 1:length(v)) {
  #read in a year of data
  t <- v[k]
  #pulls out array for one year, 3D with lat,lon,depth
  expc2015 <- ncvar_get(nc_data,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,12))*31536000
  #take average of 12 months (since UKESM data is monthly)
  expc2015 <- apply(expc2015, c(1,2,3),mean,na.rm=FALSE)
  
  #calculates POC flux at MLD max for every grid cell for one year
  for(i in 1:length(lon)) {
    for(j in 1:length(lat)) {
      
      #make list and add needed columns
      ret <- list()
      #NOTE - make sure you divide by 100 here if the depth units are in cm (CESM), otherwise the depth units are in m
      ret$depth <-  ncvar_get(nc_data, "lev")
      #subset expc for select lat and lon
      ret$expc <- extract(expc2015, indices = c(i,j), dims = c(1,2))
      #subset MLD max for each lat and lon
      ret$MLD <- extract(MLD_max_st[, , k], indices = c(i,j), dims = c(1,2))
      
      #ocean values - if a value exists for MLDmax, then interpolate and store new interpolated POC flux in output matrix
      if(!is.na(ret$MLD)) {
        
        #find interpolated expc at mld max
        interp <- approx(x = ret$depth, y  = ret$expc, xout = ret$MLD)
        #store interpolated POC flux into the output matrix
        output[i, j] <- interp$y[1]
        #land values - if a value doesn't exist for MLDmax, then don't interpolate, just put an NA value in output matrix  
      } else {
        output[i,j] <- NA
      }
    }
  }
  #store each year of output into a list
  list_expc[[k]] <- output
}

#converts from list to matrices
expc_st <- do.call(cbind, list_expc)
#combines matrices into a single array
expc_st <- array(expc_st, dim=c(dim(list_expc[[1]]), length(list_expc)))

#20 year mean for the beginning of the 21st century
mean_expc_st <- apply(expc_st, c(1, 2), mean, na.rm = FALSE)

#save matrices for plotting
setwd("~/senior_thesis/plotting_dataframes/")
saveRDS(mean_expc_st, file = "UKESM_mean_expc_st.Rds", ascii = TRUE)
