#' @title POC flux transect generator
#' @author Stevie Walker
#' @description formats depth resolved POC flux data for plotting latitudinal transects at
#' at any given longitude for the short-term (2014-2034) and long-term (2079-2099)
#' @details April 2021
#' @details Modified some of the code from https://fishandwhistle.net/post/2019/depth-time-heatmaps/ 


calc_transect <- function(lon, save.name.st, save.name.lt) {
  setwd("~/senior_thesis/combined_CESM_files/")
  nc_data <- nc_open('expc_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc')
  
  #load average POC flux arrays for short-term and long-term
  avg_expc_st <- readRDS("~/senior_thesis/plotting_dataframes/avg_expc_st.Rds")
  avg_expc_lt <- readRDS("~/senior_thesis/plotting_dataframes/avg_expc_lt.Rds")
  
  #extract short term and long term depth profiles for a selected lon
  transect_st <- extract(avg_expc_st, indices = lon, dims = 1)
  transect_lt <- extract(avg_expc_lt, indices = lon, dims = 1)
  
  #preparing short-term melted data --------------
  ret <- list()
  ret$lat <- ncvar_get(nc_data,"nlat")
  ret$depth <-  ncvar_get(nc_data, "lev")/100
  ret$expc <- transect_st*31536000
  
  file_info <- ret
  
  #melt data so I can plot it in ggplot
  melt_depth <- function(L) {
    dimnames(L$expc) <- list(lon = L$nlon, lat = L$nlat, depth = L$depth)
    ret <- melt(L$expc, value.name = "expc")
  }
  
  #formatted for ggplot
  melt_transect_st <- melt_depth(file_info)
  #get rid of lon column so it is now x, y, z format
  melt_transect_st <- subset(melt_transect_st, select = -lon)
  
  #preparing long-term melted data -------------
  ret <- list()
  ret$lat <- ncvar_get(nc_data,"nlat")
  ret$depth <-  ncvar_get(nc_data, "lev")/100
  ret$expc <- transect_lt*31536000
  
  file_info <- ret
  
  #melt data so I can plot it in ggplot
  melt_depth <- function(L) {
    dimnames(L$expc) <- list(lon = L$nlon, lat = L$nlat, depth = L$depth)
    ret <- melt(L$expc, value.name = "expc")
  }
  
  #formatted for ggplot
  melt_transect_lt <- melt_depth(file_info)
  
  #get rid of lon column so it is now x, y, z format
  melt_transect_lt <- subset(melt_transect_lt, select = -lon)
  

##interpolate for the short-term transect -------------------

interp_expc_st <- function(target_lat) {
  
  #filter by target_lat
  data_for_depth <- melt_transect_st %>%
    filter(lat == target_lat) %>%
    arrange(lat)
  #create true/false data frame for every column
  T_or_F <- apply(data_for_depth, 2, is.na) %>%
    data.frame() %>%
    dplyr::rename("lat_tf" = lat, "depth_tf" = depth, "expc_tf" = expc)
  #bind true/false df with data to be interpolated
  data_for_depth <- cbind(data_for_depth,T_or_F)
  
  depth = seq(5, 5375, length.out = 537)
  
  if(data_for_depth$expc_tf == TRUE) { interp <- data.frame(depth)%>%
    add_column(expc = NA)
  } else {interp <- approx(data_for_depth$depth, data_for_depth$expc, xout = seq(5, 5375, length.out = 537)) %>%
    data.frame() %>%
    dplyr::rename("depth" = x, "expc" = y)}
  
  
  #add marker column for sorting later
  lat <- rep(target_lat, 537)
  #bind marker column to interpolated expc data
  interp <- cbind(lat,interp)
} 
  
#interpolate to 10m resolution at every lat
  lats <- c(1:384)
  plot_transect <- lapply(lats, interp_expc_st)
  
  #convert from list to matrix to data frame
  df_st <- rbindlist(plot_transect)
  #save data frame
  setwd("~/senior_thesis/plotting_dataframes/")
  saveRDS(df_st, file = paste(save.name.st,".Rds",sep=""), ascii = TRUE)
  
interp_expc_lt <- function(target_lat) {
    
    #filter by target_lat
    data_for_depth <- melt_transect_lt %>%
      filter(lat == target_lat) %>%
      arrange(lat)
    #create true/false data frame for every column
    T_or_F <- apply(data_for_depth, 2, is.na) %>%
      data.frame() %>%
      dplyr::rename("lat_tf" = lat, "depth_tf" = depth, "expc_tf" = expc)
    #bind true/false df with data to be interpolated
    data_for_depth <- cbind(data_for_depth,T_or_F)
    
    depth = seq(5, 5375, length.out = 537)
    
    if(data_for_depth$expc_tf == TRUE) { interp <- data.frame(depth)%>%
      add_column(expc = NA)
    } else {interp <- approx(data_for_depth$depth, data_for_depth$expc, xout = seq(5, 5375, length.out = 537)) %>%
      data.frame() %>%
      dplyr::rename("depth" = x, "expc" = y)}
    
    
    #add marker column for sorting later
    lat <- rep(target_lat, 537)
    #bind marker column to interpolated expc data
    interp <- cbind(lat,interp)
  }
  
#interpolate to 10m resolution at every lat
lats <- c(1:384)
plot_transect <- lapply(lats, interp_expc_lt)

#convert from list to matrix to data frame
df_lt <- rbindlist(plot_transect)
#save data frame
setwd("~/senior_thesis/plotting_dataframes/")
saveRDS(df_lt, file = paste(save.name.lt,".Rds",sep=""), ascii = TRUE)

}
