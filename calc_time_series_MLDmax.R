#' @title Calculate yearly maximum annual mixed layer depth for historical and future data
#' @author Stevie Walker
#' @date 10/26/21
#' @description finds MLDmax for every grid cell in every year from 1850-2100, to be used in time series calculation
#' @input mlotst model file
#' @output 2 large arrays (historical and future) of yearly mixed layer depth, each matrix is one year
#' @note be sure to run function get_time first so you know what to make seq.start.st and seq.start.lt

#wd = "~/senior_thesis/combined_CESM_files"
#nc.file = 'mlotst_Omon_CESM2_ssp585_r10i1p1f1_gn_201501-210012.nc'
#start.fut = 6
#end.fut = 1014
#model.name = "CESM"


time_series_MLDmax <- function(wd, nc.file, model.name, start.fut, end.fut) {
  
  setwd(wd)
  nc_data <- nc_open(nc.file)
  
  ## Short-term ------------
  
  #short term sequence
  v <- seq(from = start.fut, to = end.fut, by = 12)
  n = length(v)
  
  #create list to store for loop output in
  list_max <- list()
  
  for(i in 1:n) {
    #read in a year of MLD data
    t <- v[i]
    var <- ncvar_get(nc_data,"mlotst",start= c(1,1,t), count = c(-1,-1,12))
    #calculate the MLD max for each year at each lat and lon
    max <- apply(var, c(1,2),max,na.rm=FALSE)
    #store output into the list
    list_max[[i]] <- max
  }
  
  Y1 <- do.call(cbind, list_max)
  fut_array <- array(Y1, dim=c(dim(list_max[[1]]), length(list_max)))
  
  setwd('~/senior_thesis/plotting_dataframes/time_series/')
  #saving non-averaged future array for calc_time_series_expc
  saveRDS(fut_array, file = paste(model.name,"_array_fut_time_series.Rds",sep=""), ascii = TRUE)
   
}
