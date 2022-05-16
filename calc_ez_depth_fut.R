#' @title Calculate euphotic zone depth
#' @description euphotic zone depth is defined as 1% of NPP max
#' @description calculates Ez depth change, historical, and long-term average Ez depth
#' @input NPP nc files
#' @output three matrices of long-term (2079-2099), historical (1850-1900), and change in Ez depth
#' @author Stevie Walker
#' @date 3/28/22

#wd = "~/senior_thesis/combined_CESM_files/"
#nc.fut = "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
#nc.his = "pp_Oyr_CESM2_historical_r10i1p1f1_gn_1850-2014.nc"
#model.name = "CESM"
#start.lt = 66
#start.his = 2
#lon.length = 1:320
#lat.length = 1:384

calc_ez_depth_fut <- function(wd, nc.fut, model.name, start.lt, lon.length, lat.length) {
  
  # Calculate future average (2079-2099) euphotic zone depth -----------
  
  setwd(wd)
  nc_data <- nc_open(nc.fut)
  
  #yearly (uncomment/comment depending on if data is year or month archived)
  v <- seq(from = start.lt, to = start.lt + 19, by = 1)
  
  #monthly
  #v <- seq(from = start.lt, to = start.lt + 228, by = 12)
  
  #create list to store for loop ez depth arrays in
  list_ez <- list()
  
  #storage container for one percent npp data
  output <- matrix(nrow = length(lon.length), ncol = length(lat.length))  
  
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    
    #pulls out array for one year, 3D with lat,lon,depth
    #yearly
    npp <- ncvar_get(nc_data, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    
    #monthly
    #npp <- ncvar_get(nc_data, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
    #npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
    
    #calculates column integrated npp for one year
    for(i in 1:length(lon.length)) {
      for(j in 1:length(lat.length)) {
        
        ret <- list()
        #subset npp for select lat and lon
        ret$npp <- extract(npp, indices = c(i,j), dims = c(1,2))
        #add depth (divide by 100 for CESM)
        ret$depth <- ncvar_get(nc_data, "lev") /100
        #uncomment this for IPSL
        #ret$depth <- ncvar_get(nc_data, "olevel")
        #finds one percent of max npp
        ret$one.percent <- max(ret$npp[1,1, ],na.rm = TRUE)/100
        #true/false test (pulls out first )
        ret$test <- extract(npp[, , 1], indices = c(i,j), dims = c(1,2))
        
        #ocean values - if a value exists for npp, then find ez depth and store in output matrix
        if (is.na(ret$test) == FALSE) {
          
          #find euphotic zone depth
          ez <- approx(x = ret$npp, y  = ret$depth, xout = ret$one.percent)
          
          if(is.na(ez$y) == FALSE) {
            #store interpolated ez depth into the output matrix
            output[i, j] <- ez$y
            
          } else {
            #assign deepest NPP depth available in CESM
            output[i,j] = 150
          }
        } else {
          # land values
          output[i,j] = NA
        }
      }
    }
    #store output into the list
    list_ez[[k]] <- output
  }
  
  #converts from list to matrices
  ez_fut <- do.call(cbind, list_ez)
  #combines matrices into a single array
  ez_fut <- array(ez_fut, dim=c(dim(list_ez[[1]]), length(list_ez)))
  
  #20 year future mean
  ez_depth_fut <- apply(ez_fut, c(1,2), mean, na.rm = FALSE)
  
  #save matrix for plotting
  setwd("~/senior_thesis/plotting_dataframes/ez_depth/")
  saveRDS(ez_depth_fut, file = paste(model.name,"_ez_depth_fut.Rds",sep=""), ascii = TRUE)
  
}

#wd = "~/senior_thesis/combined_CMCC_files/"
#nc.fut = 'pp_Omon_CMCC-ESM2_ssp585_r1i1p1f1_gn_201501-210012.nc'
#nc.his = 'pp_Omon_CMCC-ESM2_historical_r1i1p1f1_gn_185001-201412.nc'
#model.name = "CMCC"
#start.lt = 771
#start.his = 1
#lon.length = 1:362
#lat.length = 1:292


calc_ez_depth_fut2 <- function(wd, nc.fut, model.name, start.lt, lon.length, lat.length) {
  
  # Calculate future average (2079-2099) euphotic zone depth -----------
  
  setwd(wd)
  nc_data <- nc_open(nc.fut)
  
  #yearly (uncomment/comment depending on if data is year or month archived)
  v <- seq(from = start.lt, to = start.lt + 19, by = 1)
  
  #monthly
  #v <- seq(from = start.lt, to = start.lt + 228, by = 12)
  
  #create list to store for loop ez depth arrays in
  list_ez <- list()
  
  #storage container for one percent npp data
  output <- matrix(nrow = length(lon.length), ncol = length(lat.length))  
  
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    
    #pulls out array for one year, 3D with lat,lon,depth
    #yearly
    npp <- ncvar_get(nc_data, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    
    #monthly
    #npp <- ncvar_get(nc_data, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
    #npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
    
    #calculates column integrated npp for one year
    for(i in 1:length(lon.length)) {
      for(j in 1:length(lat.length)) {
        
        ret <- list()
        #subset npp for select lat and lon
        ret$npp <- extract(npp, indices = c(i,j), dims = c(1,2))
        #add depth (divide by 100 for CESM)
        ret$depth <- ncvar_get(nc_data, "lev") /100
        #uncomment this for IPSL
        #ret$depth <- ncvar_get(nc_data, "olevel")
        #finds one percent of max npp
        ret$one.percent <- max(ret$npp[1,1, ],na.rm = TRUE)/100
        #true/false test (pulls out first )
        ret$test <- extract(npp[, , 1], indices = c(i,j), dims = c(1,2))
        
        if (is.na(ret$test) == FALSE) {
          
          #find euphotic zone depth
          ez <- approx(x = ret$npp, y  = ret$depth, xout = ret$one.percent)
          
          if(is.na(ez$y) == FALSE) {
            #store interpolated ez depth into the output matrix
            output[i, j] <- ez$y
            
          } else {
            #true false vector of non-na and na values
            keep <- complete.cases(ret$npp)
            
            #keep rows with values
            new.npp <- ret$npp[keep]
            
            #pull out bottom depth 
            output[i,j] = ret$depth[length(new.npp)]
          }
        } else {
          # land values
          output[i,j] = NA
        }
        
      }
    }
    #store output into the list
    list_ez[[k]] <- output
  }
  
  #converts from list to matrices
  ez_fut <- do.call(cbind, list_ez)
  #combines matrices into a single array
  ez_fut <- array(ez_fut, dim=c(dim(list_ez[[1]]), length(list_ez)))
  
  #20 year future mean
  ez_depth_fut <- apply(ez_fut, c(1,2), mean, na.rm = FALSE)
  
  #save matrix for plotting
  setwd("~/senior_thesis/plotting_dataframes/ez_depth/")
  saveRDS(ez_depth_fut, file = paste(model.name,"_ez_depth_fut.Rds",sep=""), ascii = TRUE)
  
}

## Old version ----------

for(k in 1:length(v)) {
  #read in a year of data
  t <- v[k]
  
  #pulls out array for one year, 3D with lat,lon,depth
  #yearly
  npp <- ncvar_get(nc_data, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
  
  #monthly
  #npp <- ncvar_get(nc_data, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
  #npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
  
  #calculates column integrated npp for one year
  for(i in 1:length(lon.length)) {
    for(j in 1:length(lat.length)) {
      
      ret <- list()
      #subset npp for select lat and lon
      ret$npp <- extract(npp, indices = c(i,j), dims = c(1,2))
      #add depth (divide by 100 for CESM)
      ret$depth <- ncvar_get(nc_data, "lev") #/100
      #uncomment this for IPSL
      #ret$depth <- ncvar_get(nc_data, "olevel")
      #finds one percent of max npp
      ret$one.percent <- max(ret$npp[1,1, ],na.rm = TRUE)/100
      #true/false test (pulls out first )
      ret$test <- extract(npp[, , 1], indices = c(i,j), dims = c(1,2))
      
      #ocean values - if a value exists for npp, then find Ez depth and store in output matrix
      if (is.na(ret$test) == FALSE) {
        
        #find euphotic zone depth
        ez <- approx(x = ret$npp, y  = ret$depth, xout = ret$one.percent)
        
        #store interpolated Ez depth into the output matrix
        output[i, j] <- ez$y
        
        #land values - if a value doesn't exist for Ez depth, then don't interpolate, just put an NA value in output matrix  
      } else {
        output[i,j] <- NA
      }
    }
  }

  
  