#' @title Calculate time series of globally integrated POC flux change at the MLDmax (1850-2100) for UKESM
#' @author Stevie Walker
#' @date 1/25/21
#' @inputs expc nc file, areacello nc file, MLDmax time series arrays (historical and future for all inputs)
#' @output csv file of year and global POC flux for each year
#' @description requires averaging monthly expc data in chunks with uncombined files (monthly data too large to use combine_nc)


#file = 'expc_Omon_UKESM1-0-LL_historical_r1i1p1f2_gn_185001-189912.nc'
#wd = "~/senior_thesis/combined_UKESM_files"
#area.name = 'areacello_Ofx_UKESM1-0-LL_piControl_r1i1p1f2_gn.nc'
#start.date = 1
#end.date = 585
#section = "1850_1900"

UKESM_time_series_his <- function(file, wd, model.name, area.name, start.date, end.date, section) {

#read in POC flux data, MLD arrays calculated in calc_time_series_MLDmax.R
setwd(wd)
nc_data <- nc_open(file)

MLD.his <- readRDS(paste0("~/senior_thesis/plotting_dataframes/time_series/UKESM_MLD_his_time_series.Rds"))

#read in ocean cell area data
nc_data_area <- nc_open(area.name)
area <- ncvar_get(nc_data_area, "areacello")

## HISTORICAL CALCULATION -------------------------

#make first column of the vector (to be combined later)
#NOTE: double check the start and end years for each model (must be the same as MLD.his), adjust these yearly values accordingly
year <- seq(from = 1850, to = 1900, by = 1)

#broken up into chunks since the files aren't combined, get year values from get time
v <- seq(from = start.date, to = end.date, by = 12)

#final vector output 
POC_flux_his = vector(mode = "numeric", length = length(v))

#storage container for second for loop
output_his <- matrix(nrow = 360, ncol = 330)

for(k in 1:length(v)) {
  
  #year to read in
  t <- v[k]
  
  #get variable at specified time range (1 year)
  expc <- ncvar_get(nc_data,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,12))*31536000
  expc <- apply(expc, c(1,2,3),mean,na.rm=FALSE)
  
  #interpolate POC flux at MLDmax for every grid cell and year
  for(i in 1:360) {
    for(j in 1:330) {
      
      #make list and add needed columns
      ret <- list()
      #depth in meters
      ret$depth <-  ncvar_get(nc_data, "lev")
      #subset expc for select lat and lon
      ret$expc <- extract(expc, indices = c(i,j), dims = c(1,2))
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
  POC_flux_his[k] = sum_flux_his
  
}

#binds POC_flux vector to year vector
df.his <- qpcR:::cbind.na(year,POC_flux_his)

df = data.frame(df.his) # this name will get replaced to time.series
#change column names for merging csv files later
colnames(df) = c('Year','UKESM')
write.csv(df,paste0("~/senior_thesis/plotting_dataframes/time_series/UKESM_time_series_",section,".csv"))

}