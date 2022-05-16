#' @title Calculate Interpolated Ocean Temperature and Salinity
#' @author Stevie Walker
#' @date 5/1/22
#' @description Finds the temperature or salinity at any ocean depth
#' @input so or thetao nc file
#' @output a list of matrices with temp or sal for every month
#' @notes takes at least 4 days to run per model, over two weeks for MPI


#includes surface if else statement
calc_interp_tempsal <- function(wd, model.name, nc.so, lon.length, lat.length, depth.out) {
  
  # read in nc files
  
  setwd(wd)
  nc_so <- nc_open(nc.so)
  
  names <- nc.so %>%
    str_split(.,"_") %>%
    unlist %>%
    str_split(.,".nc") %>%
    unlist
  
  #find salinity or temperature at specified depth ------------
  
  time <- ncvar_get(nc_so,"time")
  
  #storage container for list of matrices
  list_interp <- list()
  #storage containers for each month
  output_sal <- matrix(nrow = length(lon.length), ncol = length(lat.length))
  
  for(k in 1:length(time)) {
    
    #pulls out array for one year, 3D with lat,lon,depth
    sal <- ncvar_get(nc_so,names[1],start= c(1,1,1,k), count = c(-1,-1,-1,1))
    
    if (depth.out == "surface") {
      
      list_interp[[k]] <- sal[ , ,1]
      
    } else {
      
      for(i in 1:length(lon.length)) {
        for(j in 1:length(lat.length)) {
          
          #make list and add needed columns
          ret <- list()
          #NOTE - make sure you divide by 100 here if the depth units are in cm (CESM), otherwise the depth units are in m
          #ret$depth <-  ncvar_get(nc_so, "lev") #/100
          #uncomment this line for IPSL
          ret$depth <-  ncvar_get(nc_so, "olevel")
          #subset expc for select lat and lon
          ret$sal <- R.utils::extract(sal, indices = c(i,j), dims = c(1,2))
          #depth to interpolate at
          ret$depth_out <- depth.out
          #NA test
          ret$test <- R.utils::extract(sal[, , 1], indices = c(i,j), dims = c(1,2))
          
          #ocean values - if a value exists for salinity
          if (is.na(ret$test) == FALSE) {
            
            #find interpolated expc at mld max
            interp <- approx(x = ret$depth, y  = ret$sal, xout = ret$depth_out)
            #store interpolated POC flux into the output matrix
            output_sal[i, j] <- interp$y[1]
            #land values - if a value doesn't exist for ocean grid cell, then don't interpolate, just put an NA value in output matrix  
          } else {
            output_sal[i,j] <- NA
          }
        }
      }
      
      #store each year of output into a list
      list_interp[[k]] <- output_sal
      
    }
  }
  setwd("~/senior_thesis/plotting_dataframes/temp_sal/")
  saveRDS(list_interp, file = paste(model.name,"_",names[1],"_",depth.out,"_array_",names[7],".Rds",sep=""), ascii = TRUE)
  
}
