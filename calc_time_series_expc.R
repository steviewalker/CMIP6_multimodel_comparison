


time_series_MLDmax <- function(wd, model.name, his.name, fut.name, area.name, MLD.his.name, MLD.fut.name) {
  
## Calculating one year of MLDmax data ---------------
  
  for(i in 1:n) {
    #read in a year of MLD data
    t <- v[i]
    var <- ncvar_get(nc_data,"mlotst",start= c(1,1,t), count = c(-1,-1,12))
    #calculate the MLD max for each year at each lat and lon
    max <- apply(var, c(1,2),max,na.rm=FALSE)
    #store output into the list
  }
  
  
  
  
## Calculate globally integrated POC flux at MLDmax  
  
  #read in POC flux data, MLD data
  setwd(wd)
  nc_data_1850 <- nc_open(his.name)
  nc_data_2015 <- nc_open(fut.name)
  MLD.his <- nc_open(MLD.his.name)
  MLD.fut <- nc_open(MLD.fut.name)
  
  nc_data_area <- nc_open(area.name)
  area <- ncvar_get(nc_data_area, "areacello")
  
## future calculation --------------
  
    time <- ncvar_get(nc_data_2015, "time")
  
  v <- 1:length(time)
  
  #make first column of the vector (to be combined later)
  year <- seq(from = 2015, to = 2100, by = 1)
  
  #vector output 
  POC_flux = vector(mode = "numeric", length = length(time))
  
  for(k in 1:length(v)) {
    
    t <- v[k]
    
    #get variable at specified time range (1 year)
    variable_fut <- ncvar_get(nc_data_2015,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    MLD.fut <- ncvar_get(nc_data,"mlotst",start= c(1,1,t), count = c(-1,-1,12))
    
    for(i in 1:length(lon.length)) {
      for(j in 1:length(lat.length)) {
        
        #make list and add needed columns
        ret <- list()
        #NOTE - make sure you divide by 100 here if the depth units are in cm (CESM), otherwise the depth units are in m
        ret$depth <-  ncvar_get(nc_data, "lev") #/100
        #subset expc for select lat and lon
        ret$expc <- extract(variable_his, indices = c(i,j), dims = c(1,2))
        #subset MLD max for each lat and lon
        ret$MLD <- extract(MLD_max_his[, , k], indices = c(i,j), dims = c(1,2))
        
        #ocean values - if a value exists for MLDmax, then interpolate and store new interpolated POC flux in output matrix
        if (is.na(ret$MLD) == FALSE) {
          
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
    
    
    #multiply by cell area
    global_flux <- variable_his*area
    
    #sum of all model cells
    sum_flux <- sum(global_flux, na.rm = TRUE)
    
    #Total global POC flux in Pt C / yr for one year
    sum_flux <- sum_flux*12.01/1000000000000000
    
    POC_flux[i] = sum_flux
    
  }
  
  df.his <- qpcR:::cbind.na(year,POC_flux)