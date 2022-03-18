#' @title Calculate long-term and historical average column integrated NPP (mol m-2 d-1)
#' @author Stevie Walker
#' @date 2/20/21
#' @description finds long-term(2079-2099) and historical(1850-1900) average NPP, summed up vertically for each grid cell
#' @input ssp585 and historical pp files
#' @output three matrices: long-term NPP, historical NPP, and change in NPP

## Calculate water column integrated NPP --------------

#wd = "~/senior_thesis/combined_CMCC_files/"
#nc.file = "pp_Omon_CMCC-ESM2_ssp585_r1i1p1f1_gn_201501-210012.nc"
#lon.length = 1:362
#lat.length = 1:292
#start.lt = 747
#model.name = "CMCC"

calc_npp_avg <- function(wd, nc.file, model.name, start.lt, lon.length, lat.length) {

  
  setwd(wd)
  nc_file <- nc_open(nc.file)
  
  #yearly (uncomment/comment depending on if data is year or month archived)
  v <- seq(from = start.lt, to = start.lt + 19, by = 1)
  
  #monthly
  #v <- seq(from = start.lt, to = start.lt + 228, by = 12)
  
  #storage container for list of matrices
  list_npp <- list()
  
  #storage container for summed npp data
  output <- matrix(nrow = length(lon.length), ncol = length(lat.length))  
  
  #creates list of 20 arrays with POC flux at every lat,lon,depth
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    #pulls out array for one year, 3D with lat,lon,depth
    #yearly
    npp <- ncvar_get(nc_file, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    
    #monthly
    #npp <- ncvar_get(nc_file, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
    #npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
    
    #calculates column integrated npp for one year
    for(i in 1:length(lon.length)) {
      for(j in 1:length(lat.length)) {
        
        #make list and add needed columns
        ret <- list()
        #NOTE - make sure you divide by 100 here if the depth units are in cm (CESM), otherwise the depth units are in m
        ret$depth <-  ncvar_get(nc_file, "lev") #/100
        #subset npp for select lat and lon
        ret$npp <- extract(npp, indices = c(i,j), dims = c(1,2))
        #true/false test (pulls out first )
        ret$test <- extract(npp[, , 1], indices = c(i,j), dims = c(1,2))
        
        #ocean values - if a value exists for npp, then find column integrated npp (mol m-2 d-1) and store in output matrix
        if (is.na(ret$test) == FALSE) {
          
          #create data frame
          #NOTE: need to change [1:x] to match the number of depth cells
          profile <- data.frame(ret$depth, ret$npp[1:75]) %>%
            as_tibble() 
          #rename depth column
          profile <- rename(profile, depth = ret.depth)
          #also change this to match number of depth cells
          profile <- rename(profile, npp = ret.npp.1.75.)
          
          #add calculated column height
          profile <- profile %>% 
            mutate(lead = lead(depth))
          profile <- profile %>%
            mutate(bottom_depth = rowMeans(profile[c('lead','depth')]))
          profile <- profile %>%
            mutate(lag = lag(bottom_depth))
          profile <- profile %>%
            mutate(height = bottom_depth - lag)
          
          #replace NA value in height column
          profile[1,6] = profile$bottom_depth[1]
          
          #calculate npp in mol m-2 yr-1
          profile <- profile %>%
            mutate(npp_new = npp*height)
          
          #store interpolated POC flux into the output matrix
          output[i, j] <- sum(profile$npp_new, na.rm = TRUE)
          #land values - if a value doesn't exist for npp test, then don't interpolate, just put an NA value in output matrix  
        } else {
          output[i,j] <- NA
        }
      }
    }
    
    #store each year of output into a list
    list_npp[[k]] <- output
  }
  
  #converts from list to matrices
  npp_lt <- do.call(cbind, list_npp)
  #combines matrices into a single array
  npp_lt <- array(npp_lt, dim=c(dim(list_npp[[1]]), length(list_npp)))
  
  #20 year mean for the end of the 21st century
  mean_npp_lt <- apply(npp_lt, c(1, 2), mean, na.rm = FALSE)
  
  #save matrix for plotting
  setwd("~/senior_thesis/plotting_dataframes/NPP/")
  saveRDS(mean_npp_lt, file = paste(model.name,"_mean_npp_lt.Rds",sep=""), ascii = TRUE)
  
  
}


#wd = "~/senior_thesis/combined_CESM_files/"
#nc.file = "pp_Oyr_CESM2_historical_r10i1p1f1_gn_1850-2014.nc"
#lon.length = 1:320
#lat.length = 1:384
#start.his = 2
#model.name = "CESM"

calc_npp_avg_his <- function(wd, nc.file, model.name, start.his, lon.length, lat.length) {
  
  
  setwd(wd)
  nc_file <- nc_open(nc.file)
  
  #yearly (uncomment/comment depending on if data is year or month archived)
  #v <- seq(from = start.his, to = start.his + 49, by = 1)
  
  #monthly
  v <- seq(from = start.his, to = start.his + 599, by = 12)
  
  #storage container for list of matrices
  list_npp <- list()
  
  #storage container for summed npp data
  output <- matrix(nrow = length(lon.length), ncol = length(lat.length))  
  
  #creates list of 20 arrays with POC flux at every lat,lon,depth
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    #pulls out array for one year, 3D with lat,lon,depth
    #yearly
    #npp <- ncvar_get(nc_file, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    
    #monthly
    npp <- ncvar_get(nc_file, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
    npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
    
    #calculates column integrated npp for one year
    for(i in 1:length(lon.length)) {
      for(j in 1:length(lat.length)) {
        
        #make list and add needed columns
        ret <- list()
        #NOTE - make sure you divide by 100 here if the depth units are in cm (CESM), otherwise the depth units are in m
        ret$depth <-  ncvar_get(nc_file, "lev") /100
        #subset npp for select lat and lon
        ret$npp <- extract(npp, indices = c(i,j), dims = c(1,2))
        #true/false test (pulls out first )
        ret$test <- extract(npp[, , 1], indices = c(i,j), dims = c(1,2))
        
        #ocean values - if a value exists for npp, then find column integrated npp (mol m-2 d-1) and store in output matrix
        if (is.na(ret$test) == FALSE) {
          
          #create data frame
          #NOTE: need to change [1:x] to match the number of depth cells
          profile <- data.frame(ret$depth, ret$npp[1:50]) %>%
            as_tibble() 
          #rename depth column
          profile <- rename(profile, depth = ret.depth)
          #also change this to match number of depth cells
          profile <- rename(profile, npp = ret.npp.1.50.)
          
          #add calculated column height
          profile <- profile %>% 
            mutate(lead = lead(depth))
          profile <- profile %>%
            mutate(bottom_depth = rowMeans(profile[c('lead','depth')]))
          profile <- profile %>%
            mutate(lag = lag(bottom_depth))
          profile <- profile %>%
            mutate(height = bottom_depth - lag)
          
          #replace NA value in height column
          profile[1,6] = profile$bottom_depth[1]
          
          #calculate npp in mol m-2 yr-1
          profile <- profile %>%
            mutate(npp_new = npp*height)
          
          #store interpolated POC flux into the output matrix
          output[i, j] <- sum(profile$npp_new, na.rm = TRUE)
          #land values - if a value doesn't exist for MLDmax, then don't interpolate, just put an NA value in output matrix  
        } else {
          output[i,j] <- NA
        }
      }
    }
    
    #store each year of output into a list
    list_npp[[k]] <- output
  }
  
  #converts from list to matrices
  npp_his <- do.call(cbind, list_npp)
  #combines matrices into a single array
  npp_his <- array(npp_his, dim=c(dim(list_npp[[1]]), length(list_npp)))
  
  #20 year mean for the end of the 21st century
  mean_npp_his <- apply(npp_his, c(1, 2), mean, na.rm = FALSE)
  
  #save matrix for plotting
  setwd("~/senior_thesis/plotting_dataframes/NPP/")
  saveRDS(mean_npp_his, file = paste(model.name,"_mean_npp_his.Rds",sep=""), ascii = TRUE)
  
  mean_npp_lt <- readRDS(paste("~/senior_thesis/plotting_dataframes/NPP/",model.name,"_mean_npp_lt.Rds", sep = ""))
  #calculate change in NPP                       
  npp_change = mean_npp_lt - mean_npp_his
  
  saveRDS(npp_change, file = paste(model.name,"_npp_change.Rds",sep=""), ascii = TRUE)
  
}

