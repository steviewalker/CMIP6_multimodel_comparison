#' @title Calculate time series of globally integrated POC flux change at the MLDmax (1850-2100)
#' @author Stevie Walker
#' @date 11/1/21
#' @inputs expc nc file, areacello nc file, mlotst nc file, MLDmax time series arrays (historical and future for all inputs)
#' @output 

#fut.name = 'expc_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc'
#his.name = 'expc_Oyr_CESM2_historical_r10i1p1f1_gn_1850-2014.nc'
#wd = "~/senior_thesis/combined_CESM_files"
#model.name = "CESM"
#area.name = 'areacello_Ofx_CESM2_ssp585_r10i1p1f1_gn.nc'
#lon.length = 1:320
#lat.length = 1:384


time_series_expc <- function(wd, model.name, fut.name, his.name, area.name, lon.length, lat.length) {
  
  
  ## Calculate globally integrated POC flux at MLDmax 
  
  #read in POC flux data, MLD arrays calculated in calc_time_series_MLDmax.R
  setwd(wd)
  nc_data_1850 <- nc_open(his.name)
  nc_data_2015 <- nc_open(fut.name)
  MLD.fut <- readRDS(paste0("~/senior_thesis/plotting_dataframes/time_series/",model.name,"_MLD_fut_time_series.Rds"))
  MLD.his <- readRDS(paste0("~/senior_thesis/plotting_dataframes/time_series/",model.name,"_MLD_his_time_series.Rds"))
  
  #read in ocean cell area data
  nc_data_area <- nc_open(area.name)
  area <- ncvar_get(nc_data_area, "areacello")
  
  ## future calculation --------------------------------------------
  
  #length should be 85 years
  v <- 1:85
  
  #make first column of the vector (to be combined later)
  #NOTE: double check the start and end years for each model, make sure the number of years are the same as MLD.fut, adjust these yearly values accordingly
  year <- seq(from = 2015, to = 2100, by = 1)
  
  #final vector output 
  POC_flux_fut = vector(mode = "numeric", length = length(v))
  
  #storage container for second for loop
  output_fut <- matrix(nrow = length(lon.length), ncol = length(lat.length))
  
  for(k in 1:length(v)) {
    
    #year to read in
    t <- v[k]
    
    #get variable at specified time range (1 year)
    variable_fut <- ncvar_get(nc_data_2015,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    
    
    for(i in 1:length(lon.length)) {
      for(j in 1:length(lat.length)) {
        
        #make list and add needed columns
        ret <- list()
        #NOTE - make sure you divide by 100 here if the depth units are in cm (CESM), otherwise the depth units are in m
        ret$depth <-  ncvar_get(nc_data_2015, "lev") #/100
        #subset expc for select lat and lon
        ret$expc <- extract(variable_fut, indices = c(i,j), dims = c(1,2))
        #subset MLD max for each lat and lon
        ret$MLD <- extract(MLD.fut[, , k], indices = c(i,j), dims = c(1,2))
        
        #ocean values - if a value exists for MLDmax, then interpolate and store new interpolated POC flux in output matrix
        if (is.na(ret$MLD) == FALSE) {
          
          #find interpolated expc at mld max
          interp <- approx(x = ret$depth, y  = ret$expc, xout = ret$MLD)
          #store interpolated POC flux into the output matrix
          output_fut[i, j] <- interp$y[1]
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
    
    #Total global POC flux in Pt C / yr for one year
    sum_flux <- sum_flux*12.01/1000000000000000
    
    #assign globally integrated POC flux value from t year to the output vector
    POC_flux_fut[t] = sum_flux
    
  }
  
  df.fut <- qpcR:::cbind.na(year,POC_flux_fut)
  
  ## historical calculation -------------------------------------------
  
  
  #make first column of the vector (to be combined later)
  #NOTE: double check the start and end years for each model (must be the same as MLD.his), adjust these yearly values accordingly
  year <- seq(from = 1850, to = 2000, by = 1)
  
  #length should be about 164 years 
  # NOTE: MPI expc historical data only runs up to 2000, so make length 150 and change year sequence
  v <- 1:150
  
  #final vector output 
  POC_flux_his = vector(mode = "numeric", length = length(v))
  
  #storage container for second for loop
  output_his <- matrix(nrow = length(lon.length), ncol = length(lat.length))
  
  for(k in 1:length(v)) {
    
    #year to read in
    t <- v[k]
    
    #get variable at specified time range (1 year)
    variable_his <- ncvar_get(nc_data_1850,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    
    #interpolate POC flux at MLDmax for every grid cell and year
    for(i in 1:length(lon.length)) {
      for(j in 1:length(lat.length)) {
        
        #make list and add needed columns
        ret <- list()
        #NOTE - make sure you divide by 100 here if the depth units are in cm (CESM), otherwise the depth units are in m
        ret$depth <-  ncvar_get(nc_data_1850, "lev") #/100
        #subset expc for select lat and lon
        ret$expc <- extract(variable_his, indices = c(i,j), dims = c(1,2))
        #subset MLD max for each lat and lon
        ret$MLD <- extract(MLD.his[, , k], indices = c(i,j), dims = c(1,2))
        
        #ocean values - if a value exists for MLDmax, then interpolate and store new interpolated POC flux in output matrix
        if (is.na(ret$MLD) == FALSE) {
          
          #find interpolated expc at mld max
          interp <- approx(x = ret$depth, y  = ret$expc, xout = ret$MLD)
          #store interpolated POC flux into the output matrix
          output_his[i, j] <- interp$y[1]
          #land values - if a value doesn't exist for MLDmax, then don't interpolate, just put an NA value in output matrix  
        } else {
          output_his[i,j] <- NA
        }
      }
    }
    
    #multiply by cell area
    global_flux_his <- output_his*area
    
    #sum of all model cells
    sum_flux_his <- sum(global_flux_his, na.rm = TRUE)
    
    #Total global POC flux in Pt C / yr for one year
    sum_flux_his <- sum_flux_his*12.01/1000000000000000
    
    #assign globally integrated POC flux value from t year to the output vector
    POC_flux_his[t] = sum_flux_his
    
  }
  
  
  #binds POC_flux vector to year vector
  df.his <- qpcR:::cbind.na(year,POC_flux_his)
  
  time.series <- rbind(df.his,df.fut)
  df = data.frame(time.series) # this name will get replaced to time.series
  #change column names for merging csv files later
  colnames(df) = c('Year',model.name)
  write.csv(df,paste0("~/senior_thesis/plotting_dataframes/time_series/",model.name,"_time_series_expc.csv"))
  
}