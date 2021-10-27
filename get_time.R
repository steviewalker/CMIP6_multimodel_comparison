#' @title Get Time
#' @author Stevie Walker
#' @description prints the corrected time values for a netcdf file so you can know where to start the count when reading in the variable
#' @date 7/14/21


get_time <- function (data.path, file.name) {
  
  #open nc file
  setwd(data.path)
  nc_data <- nc_open(file.name)
  
  #get time
  time <- ncvar_get(nc_data, "time")
  
  #listing units of time
  tunits <- ncatt_get(nc_data,"time","units")
  nt <- dim(time)
  
  # convert time -- split the time units string into fields
  tustr <- strsplit(tunits$value, " ")
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth <- as.integer(unlist(tdstr)[2])
  tday <- as.integer(unlist(tdstr)[3])
  tyear <- as.integer(unlist(tdstr)[1])
  time.corrected <- chron(time,origin=c(tmonth, tday, tyear))
  
  #lists the dates for the averages that are being calculated
  print(time.corrected)
}

