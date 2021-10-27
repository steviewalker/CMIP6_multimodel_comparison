#' @title Calculate short-term POC flux at any depth
#' @author Stevie Walker
#' @date 9/17/21
#' @description finds the short-term and long-term 20 year average in POC flux at any singular depth CMIP6 earth system models
#' @note this function takes a while to run, you can run each model in a different R session to make things go faster
#' @note check metadata for lat and lon name and depth units before running function, CESM depth is in cm not m
#' @inputs depth resolved POC flux file
#' @output 3 matrices (lat x lon) of interpolated POC flux values for a 20 year short-term average (2015-2035), long-term average (2079-2099), and change

#example inputs
#model.name = "CESM"
#wd = "~/senior_thesis/combined_CESM_files"
#nc.file = 'expc_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc'
#start.st = 2
#start.lt = 65
#lon.length = 1:320
#lat.length = 1:384
#depth = 200

calc_expc_meters <- function(wd, nc.file, model.name, start.st, start.lt, lon.length, lat.length, depth) {
  
  setwd(wd)
  nc_data <- nc_open(nc.file)
  
  #read in MLDmax arrays - MLDmax for each year (these objects come from calc_MLD_max.R)
  #the only reason you need MLDmax arrays here is for the NA test
  MLD_max_st <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_array_MLD_max_st.Rds",sep = ""))
  MLD_max_lt <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_array_MLD_max_lt.Rds",sep = ""))
  
  
  ## SHORT-TERM FOR LOOP ----------------
  
  v <- seq(from = start.st, to = start.st + 19, by = 1)
  n = length(v)
  
  #storage container for list of matrices
  list_expc <- list()
  #storage container for second for loop
  output <- matrix(nrow = length(lon.length), ncol = length(lat.length))
  
  #creates list of 20 arrays with POC flux at every lat,lon,depth
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    #pulls out array for one year, 3D with lat,lon,depth
    expc2015 <- ncvar_get(nc_data,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    
    #calculates POC flux at MLD max for every grid cell for one year
    for(i in 1:length(lon.length)) {
      for(j in 1:length(lat.length)) {
        
        #make list and add needed columns
        ret <- list()
        #NOTE - make sure you divide by 100 here if the depth units are in cm (CESM), otherwise the depth units are in m
        ret$depth <-  ncvar_get(nc_data, "lev")
        #subset expc for select lat and lon
        ret$expc <- extract(expc2015, indices = c(i,j), dims = c(1,2))
        #subset MLD max for each lat and lon - just need this for the NA test
        ret$MLD <- extract(MLD_max_st[, , k], indices = c(i,j), dims = c(1,2))
        #the depth to interpolate at
        ret$dh <- depth
        
        #ocean values - if a value exists for MLDmax, then interpolate and store new interpolated POC flux in output matrix
        if (is.na(ret$MLD) == FALSE) {
          
          #find interpolated expc at mld max
          interp <- approx(x = ret$depth, y  = ret$expc, xout = ret$dh)
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
  
  #save matrix for plotting
  setwd("~/senior_thesis/plotting_dataframes/")
  saveRDS(mean_expc_st, file = paste(model.name,"_",depth,"_mean_expc_st.Rds",sep=""), ascii = TRUE)
  
  
  ## LONG-TERM FOR LOOP ----------------
  
  v <- seq(from = start.lt, to = start.lt + 19, by = 1)
  n = length(v)
  
  #storage container for list of matrices
  list_expc <- list()
  #storage container for second for loop
  output <- matrix(nrow = length(lon.length), ncol = length(lat.length))
  
  
  #creates list of 20 arrays with POC flux at every lat,lon,depth
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    #pulls out array for one year, 3D with lat,lon,depth
    expc2077 <- ncvar_get(nc_data,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    
    #calculates POC flux at MLD max for every grid cell for one year
    for(i in 1:length(lon.length)) {
      for(j in 1:length(lat.length)) {
        
        #make list and add needed columns
        ret <- list()
        #NOTE - make sure you divide by 100 here if the depth units are in cm (CESM), otherwise the depth units are in m
        ret$depth <-  ncvar_get(nc_data, "lev")
        #subset expc for select lat and lon
        ret$expc <- extract(expc2077, indices = c(i,j), dims = c(1,2))
        #subset MLD max for each lat and lon - just need this for the NA test
        ret$MLD <- extract(MLD_max_lt[, , k], indices = c(i,j), dims = c(1,2))
        #the depth to interpolate at
        ret$dh <- depth
        
        #ocean values - if a value exists for MLDmax, then interpolate and store new interpolated POC flux in output matrix
        if (is.na(ret$MLD) == FALSE) {
          
          #find interpolated expc at mld max
          interp <- approx(x = ret$depth, y  = ret$expc, xout = ret$dh)
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
  expc_lt <- do.call(cbind, list_expc)
  #combines matrices into a single array
  expc_lt <- array(expc_lt, dim=c(dim(list_expc[[1]]), length(list_expc)))
  
  #20 year mean for the beginning of the 21st century
  mean_expc_lt <- apply(expc_lt, c(1, 2), mean, na.rm = FALSE)
  
  #change in POC flux at the MLD max
  expc_change = mean_expc_lt - mean_expc_st
  
  #save matrix for plotting
  setwd("~/senior_thesis/plotting_dataframes/")
  saveRDS(mean_expc_lt, file = paste(model.name,"_",depth,"_mean_expc_lt.Rds",sep=""), ascii = TRUE)
  saveRDS(expc_change, file = paste(model.name,"_",depth,"_expc_change.Rds",sep=""), ascii = TRUE)
  
}