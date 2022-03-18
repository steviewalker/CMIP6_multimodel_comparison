#' @title Calculate time series of globally integrated NPP change (1850-2100)
#' @author Stevie Walker
#' @date 3/7/22
#' @inputs npp nc file, areacello nc file
#' @output csv file of year and global NPP for each year

#fut.name = 'pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc'
#his.name = 'pp_Oyr_CESM2_historical_r10i1p1f1_gn_1850-2014.nc'
#wd = "~/senior_thesis/combined_CESM_files"
#model.name = "CESM"
#area.name = 'areacello_Ofx_CESM2_ssp585_r10i1p1f1_gn.nc'
#lon.length = 1:320
#lat.length = 1:384


time_series_npp <- function(wd, model.name, fut.name, his.name, area.name, lon.length, lat.length) {
  
  #read in NPP and area data
  setwd(wd)
  nc_data_1850 <- nc_open(his.name)
  nc_data_2015 <- nc_open(fut.name)
  nc_data_area <- nc_open(area.name)
  
  area <- ncvar_get(nc_data_area, "areacello")
  
  ## future calculation --------------------------------------------
  
  #length should be about 85 years
  v <- 1:85
  
  #for monthly data
  #start.ts = 2 #CMCC
  #v <- seq(from = start.ts, to = start.ts + 1010, by = 12)
  
  #make first column of the vector (to be combined later)
  #NOTE: double check the start and end years for each model
  year <- seq(from = 2015, to = 2099, by = 1)
  
  #final vector output 
  npp_fut = vector(mode = "numeric", length = length(v))
  
  #storage container for second for loop
  output_fut <- matrix(nrow = length(lon.length), ncol = length(lat.length))
  
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    #pulls out array for one year, 3D with lat,lon,depth
    #yearly
    #npp <- ncvar_get(nc_data_2015, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    
    #monthly
    npp <- ncvar_get(nc_data_2015, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
    npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
    
    #calculates column integrated npp for one year
    for(i in 1:length(lon.length)) {
      for(j in 1:length(lat.length)) {
        
        #make list and add needed columns
        ret <- list()
        #NOTE - make sure you divide by 100 here if the depth units are in cm (CESM), otherwise the depth units are in m
        ret$depth <-  ncvar_get(nc_data_2015, "lev") #/100
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
          profile <- dplyr::rename(profile, depth = ret.depth)
          #also change this to match number of depth cells
          profile <- dplyr::rename(profile, npp = ret.npp.1.50.)
          
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
          output_fut[i, j] <- sum(profile$npp_new, na.rm = TRUE)
          #land values - if a value doesn't exist for MLDmax, then don't interpolate, just put an NA value in output matrix  
        } else {
          output_fut[i,j] <- NA
        }
      }
    }
    #multiply by cell area
    global_flux <- output_fut*area
    
    #sum of all model cells
    sum_flux <- sum(global_flux, na.rm = TRUE)
    
    #Total global NPP in Pt C / yr for one year
    sum_flux <- sum_flux*12.01/1000000000000000
    
    #assign globally integrated POC flux value from t year to the output vector
    npp_fut[k] = sum_flux
  }
  
  df.fut <- qpcR:::cbind.na(year,npp_fut)
  
  ## historical calculation -------------------------------------------
  
  #make first column of the vector (to be combined later)
  #NOTE: double check the start and end years for each model adjust these yearly values accordingly
  year <- seq(from = 1850, to = 2014, by = 1)
  
  #length should be about 164 years 
  v <- 1:165
  
  #for monthly data
  #start.ts2 = 1 #CMCC
  #v <- seq(from = start.ts2, to = start.ts2 + 1958, by = 12)
  
  #final vector output 
  npp_his = vector(mode = "numeric", length = length(v))
  
  #storage container for second for loop
  output_his <- matrix(nrow = length(lon.length), ncol = length(lat.length))
  
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    #pulls out array for one year, 3D with lat,lon,depth
    #yearly
    #npp <- ncvar_get(nc_data_1850, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    
    #monthly
    npp <- ncvar_get(nc_data_1850, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
    npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
    
    #calculates column integrated npp for one year
    for(i in 1:length(lon.length)) {
      for(j in 1:length(lat.length)) {
        
        #make list and add needed columns
        ret <- list()
        #NOTE - make sure you divide by 100 here if the depth units are in cm (CESM), otherwise the depth units are in m
        ret$depth <-  ncvar_get(nc_data_1850, "lev") #/100
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
          profile <- dplyr::rename(profile, depth = ret.depth)
          #also change this to match number of depth cells
          profile <- dplyr::rename(profile, npp = ret.npp.1.50.)
          
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
          output_his[i, j] <- sum(profile$npp_new, na.rm = TRUE)
          #land values - if a value doesn't exist for MLDmax, then don't interpolate, just put an NA value in output matrix  
        } else {
          output_his[i,j] <- NA
        }
      }
    }
    #multiply by cell area
    global_flux <- output_his*area
    
    #sum of all model cells
    sum_flux <- sum(global_flux, na.rm = TRUE)
    
    #Total global NPP in Pt C / yr for one year
    sum_flux <- sum_flux*12.01/1000000000000000
    
    #assign globally integrated POC flux value from t year to the output vector
    npp_his[k] = sum_flux
  }
  
  #binds npp_flux vector to year vector
  df.his <- qpcR:::cbind.na(year,npp_his)
  
  time.series <- rbind(df.his,df.fut)
  df = data.frame(time.series) # this name will get replaced to time.series
  #change column names for merging csv files later
  colnames(df) = c('Year',model.name)
  write.csv(df,paste0("~/senior_thesis/plotting_dataframes/time_series/",model.name,"_time_series_npp.csv"))
  
}