#' @title Calculate time series of globally integrated NPP change (1850-2100) for UKESM
#' @author Stevie Walker
#' @date 3/7/22
#' @inputs npp nc file, areacello nc file
#' @output csv file of year and global NPP for each year
#' @description  broken into sections because of file size


UKESM_npp_time_series_his <- function(file, wd, area.name, start.date, end.date, year.seq, section) {
  
  #read in NPP and area data
  setwd(wd)
  nc_data <- nc_open(file)
  
  #read in ocean cell area data
  nc_data_area <- nc_open(area.name)
  area <- ncvar_get(nc_data_area, "areacello")
  
  ## historical calculation -------------------------------------------
  
  #make first column of the vector (to be combined later)
  #NOTE: double check the start and end years for each model adjust these yearly values accordingly
  year <- year.seq
  
  #for monthly data
  v <- seq(from = start.date, to = end.date, by = 12)
  
  #final vector output 
  npp_his = vector(mode = "numeric", length = length(v))
  
  #storage container for second for loop
  output_his <- matrix(nrow = 360, ncol = 330)
  
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    #pulls out array for one year, 3D with lat,lon,depth
    npp <- ncvar_get(nc_data, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
    npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
    
    #calculates column integrated npp for one year
    for(i in 1:360) {
      for(j in 1:330) {
        
        #make list and add needed columns
        ret <- list()
        #add depth
        ret$depth <-  ncvar_get(nc_data, "lev")
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
          profile <- dplyr::rename(profile, depth = ret.depth)
          #also change this to match number of depth cells
          profile <- dplyr::rename(profile, npp = ret.npp.1.75.)
          
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
  
  df = data.frame(df.his)
  #change column names for merging csv files later
  colnames(df) = c('Year','UKESM')
  write.csv(df,paste0("~/senior_thesis/plotting_dataframes/time_series/UKESM_time_series_npp_",section,".csv"))
  
}
  
