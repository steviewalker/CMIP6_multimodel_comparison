#' @title Calculate integrated global POC flux time-series (1850-2100)
#' @author Stevie Walker
#' @date 10/6/21
#' @description calculates global POC flux at 100m and MLDmax for every year and saves into a data frame for plotting the time-series figure
#' @input historical POC flux and predicted POC flux (both 100m and depth integrated data)
#' @output data frame with two columns (x = year, y = globally integrated POC flux)

time_series_100 <- function(wd, model.name, his.name, fut.name, area.name, start.his, end.his, start.fut, end.fut) {

setwd(wd)
nc_data_1850 <- nc_open(his.name)
nc_data_2015 <- nc_open(fut.name)

nc_data_area <- nc_open(area.name)
area <- ncvar_get(nc_data_area, "areacello")

#for every 12 time steps, calculate yearly average
v <- seq(from = start.his, to = end.his, by = 12)

#make first column of the vector (to be combined later)
year <- seq(from = 1850, to = 2014, by = 1)

#vector output 
POC_flux = vector(mode = "numeric", length = length(v))

for(i in 1:length(v)) {
  
  t <- v[i]

#get variable at specified time range (12 months)
variable_his <- ncvar_get(nc_data_1850,"epc100",start= c(1,1,t), count = c(-1,-1, 12))

#calculate average POC flux for each grid cell over one year
var_average1 <- apply(variable_his, c(1,2),mean,na.rm=FALSE)*31536000

#multiply by cell area
global_flux <- var_average1*area

#sum of all model cells
sum_flux <- sum(global_flux, na.rm = TRUE)

#Total global POC flux in Pt C / yr for one year
sum_flux <- sum_flux*12.01/1000000000000000

POC_flux[i] = sum_flux

}

df.his <- qpcR:::cbind.na(year,POC_flux)

## Calculate Future Globally Integrated POC Flux -------------

#for every 12 time steps, calculate yearly average
v <- seq(from = start.fut, to = end.fut, by = 12)

year <- seq(from = 2015, to = 2100, by = 1)

#vector output 
POC_flux = vector(mode = "numeric", length = length(v))

for(i in 1:length(v)) {
  
  t <- v[i]
  
  # counter here
  #get variable at specified time range (12 months)
  variable_fut <- ncvar_get(nc_data_2015,"epc100",start= c(1,1,t), count = c(-1,-1, 12))
  
  #calculate average POC flux for each grid cell over one year
  var_average1 <- apply(variable_fut, c(1,2),mean,na.rm=FALSE)*31536000
  
  #multiply by cell area
  global_flux <- var_average1*area
  
  #sum of all model cells
  sum_flux <- sum(global_flux, na.rm = TRUE)
  
  #Total global POC flux in Pt C / yr for one year
  sum_flux <- sum_flux*12.01/1000000000000000
  
  POC_flux[i] = sum_flux
  
}

df.fut <- qpcR:::cbind.na(year,POC_flux)

time.series <- rbind(df.his,df.fut)
df = data.frame(time.series)
#change column names for merging csv files later
colnames(df) = c('Year',model.name)
write.csv(df,paste0("~/senior_thesis/plotting_dataframes/",model.name,"_time_series.csv"))

}

time_series_MLDmax <- function(wd, model.name, his.name, fut.name, area.name, start.his, end.his, start.fut, end.fut) {
  
  setwd(wd)
  nc_data_1850 <- nc_open(his.name)
  nc_data_2015 <- nc_open(fut.name)
  
  nc_data_area <- nc_open(area.name)
  area <- ncvar_get(nc_data_area, "areacello")
  
  v <- seq(from = start.fut, to = end.fut, by = 1)
  
  variable_fut <- ncvar_get(nc_data_1850,"expc",start= c(1,1,t), count = c(-1,-1, 12))
