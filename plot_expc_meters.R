#' @title Plot average POC flux at any depth
#' @author Stevie Walker
#' @date 10/4/21
#' @description plots and saves 1) a three panel figure of short term POC flux, long term POC flux, and change in POC flux for each model
#' and 2) a one panel figure of change in POC flux that can be later combined in the notebook
#' @description plots on the model's native grid
#' @note check the file metadata for each model to find the lat and lon standard names
#' @note model.name must match the name used in the Rds files saved from calc_epc_100_avg.R
#' @note change depths in the title manually


plot_expc_meters <- function(wd, nc_file, model.name, lat.name, lon.name, depth) {
  
  #open nc file
  setwd(wd)
  nc_data <- nc_open(nc_file)
  
  mean_expc_st <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_",depth,"_mean_expc_st.Rds",sep=""))
  mean_expc_lt <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_",depth,"_mean_expc_lt.Rds",sep=""))
  expc_change <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_",depth,"_expc_change.Rds",sep=""))
  
  
  ## Short-term POC flux --------
  
  ret <- list()
  ret$lat <- ncvar_get(nc_data, lat.name)
  ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
  ret$expc <- mean_expc_st
  
  #melt data so I can plot it in ggplot
  melt_expc <- function(L) {
    dimnames(L$expc) <- list(lon = L$nlon, lat = L$nlat)
    ret <- melt(L$expc, value.name = "expc")
  }
  
  melt_expc <- melt_expc(ret)
  
  
  plot1 <- ggplot(data = melt_expc, aes(x = lon, y = lat, fill = expc)) + 
    geom_raster(interpolate = TRUE) +
    scale_fill_cmocean(limits = c(0,4), oob = squish, name = "deep", direction = 1) +
    scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
    theme_bw() +
    #manually change title to needed depth
    labs(title = expression(paste("Short-term (2015-2035) POC Flux at 1000m (mol ",m^-2," ",y^-1,")", sep = ""))) +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank()
    )
  
  plot1
  
  ## Long-term POC flux --------------
  
  ret <- list()
  ret$lat <- ncvar_get(nc_data, lat.name)
  ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
  ret$time <- ncvar_get(nc_data, "time")
  ret$expc <- mean_expc_lt
  
  #melt data so I can plot it in ggplot
  melt_expc <- function(L) {
    dimnames(L$expc) <- list(lon = L$nlon, lat = L$nlat)
    ret <- melt(L$expc, value.name = "expc")
  }
  
  melt_expc <- melt_expc(ret)
  
  
  plot2 <- ggplot(data = melt_expc, aes(x = lon, y = lat, fill = expc)) + 
    geom_raster(interpolate = TRUE) +
    scale_fill_cmocean(limits = c(0,4), oob = squish, name = "deep", direction = 1) +
    scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
    theme_bw() +
    labs(title = expression(paste("Long-term (2077-2097) POC Flux at 1000m (mol ",m^-2," ",y^-1,")", sep = ""))) +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank()
    )
  
  plot2
  
  ## Change in POC flux ----------
  
  
  ret <- list()
  ret$lat <- ncvar_get(nc_data, lat.name)
  ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
  ret$expc <- expc_change
  
  #melt data so I can plot it in ggplot
  melt_expc <- function(L) {
    dimnames(L$expc) <- list(lon = L$nlon, lat = L$nlat)
    ret <- melt(L$expc, value.name = "expc")
  }
  
  melt_expc <- melt_expc(ret)
  
  
  plot3 <- ggplot(data = melt_expc, aes(x = lon, y = lat, fill = expc)) + 
    geom_raster(interpolate = TRUE) +
    scale_fill_cmocean(limits = c(-1,1), oob = squish, name = "balance", direction = 1)+
    scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
    theme_bw() +
    labs(title = expression(paste("Change in POC Flux at 1000m (mol ",m^-2," ",y^-1,")", sep = ""))) +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank()
    )
  
  plot3
  
  figure <- ggarrange(plot1, plot2, plot3,
                      ncol = 1, nrow = 3)
  figure <- annotate_figure(figure, top = text_grob(model.name, face = "bold", size = 16))
  
  plot3 <- annotate_figure(plot3, top = text_grob(model.name, face = "bold", size = 16))
  
  ggsave(paste(model.name,"_",depth,"_expc_global_map.png",sep=""), plot = figure, path = "~/senior_thesis/figures", width = 22, height = 30, units = "cm", dpi = 400)
  
  #saving panel c
  ggsave(paste(model.name,"_",depth,"_expc_change.png",sep=""), plot = plot3, path = "~/senior_thesis/figures", width = 20, height = 10, units = "cm", dpi = 400)
  
}