#' @title Plot change in POC flux at different depth horizons
#' @author Stevie Walker
#' @date 9/15/21
#' @description plots and saves 1) a three panel figure of change in POC flux at 100m, at MLDmax, and difference between the two for each model
#' and 2) a six panel figure of POC flux difference for all models
#' @description plots on the model's native grid
#' @note check the file metadata for each model to find the lat and lon standard names
#' @note model.name must match the name used in the Rds files
#' @note uncomment scale_y_continuous when plotting MPI


plot_depth_horizon_diff <- function(wd, nc_file, model.name, lat.name, lon.name) {
  
  #open nc file
  setwd(wd)
  nc_data <- nc_open(nc_file)
  
  change_100m <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_epc100_avg_diff.Rds",sep=""))
  change_MLDmax <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_expc_change.Rds",sep=""))
  diff <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_depth_horizon_diff_MLD.Rds",sep=""))
  
  ## Change at 100m --------
  
  ret <- list()
  ret$lat <- ncvar_get(nc_data, lat.name)
  ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
  ret$epc100 <- change_100m
  
  file_info <- ret
  
  #melt data so I can plot it in ggplot
  melt_epc100 <- function(L) {
    dimnames(L$epc100) <- list(lon = L$nlon, lat = L$nlat)
    ret <- melt(L$epc100, value.name = "epc100")
  }
  
  mepc100 <- melt_epc100(file_info)
  
p_epc100 <- ggplot(data = mepc100, aes(x = lon, y = lat, fill = epc100)) + 
    geom_raster(interpolate = TRUE) +
    scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = 1)+
    #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
    theme_bw() +
    labs(title = expression(paste("a) Change in POC Flux (mol ",m^-2," ",y^-1,")", sep = ""))) +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank())
  
  ## Change at MLDmax --------------
  
ret <- list()
ret$lat <- ncvar_get(nc_data, lat.name)
ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
ret$expc <- change_MLDmax

#melt data so I can plot it in ggplot
melt_expc <- function(L) {
  dimnames(L$expc) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$expc, value.name = "expc")
}

melt_expc <- melt_expc(ret)


p_expc <- ggplot(data = melt_expc, aes(x = lon, y = lat, fill = expc)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = 1)+
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = expression(paste("b) Change in POC Flux at MLDmax (mol ",m^-2," ",y^-1,")", sep = ""))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank()
  )
  
  ## Difference between POC flux depth horizons (100-MLDmax) ----------
  
ret <- list()
ret$lat <- ncvar_get(nc_data, lat.name)
ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
ret$expc <- diff

#melt data so I can plot it in ggplot
melt_expc <- function(L) {
  dimnames(L$expc) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$expc, value.name = "expc")
}

melt_expc <- melt_expc(ret)


p_diff <- ggplot(data = melt_expc, aes(x = lon, y = lat, fill = expc)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(-1,1), oob = squish, name = "balance", direction = 1)+
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = expression(paste("c) Difference in POC flux Change (MLDmax - 100m) (mol ",m^-2," ",y^-1,")", sep = ""))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank()
  )

#different label for 6 panel figure
plot <- ggplot(data = melt_expc, aes(x = lon, y = lat, fill = expc)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(-1,1), oob = squish, name = "balance", direction = 1)+
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = expression(paste("Difference in POC flux Change (MLDmax - 100m) (mol ",m^-2," ",y^-1,")", sep = ""))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank()
  )
  
  figure <- ggarrange(p_epc100, p_expc, p_diff,
                      ncol = 1, nrow = 3)
  figure <- annotate_figure(figure, top = text_grob(model.name, face = "bold", size = 16))
  
  plot <- annotate_figure(plot, top = text_grob(model.name, face = "bold", size = 16))
  
  ggsave(paste(model.name,"_depth_horizon_diff.png",sep=""), plot = figure, path = "~/senior_thesis/figures", width = 22, height = 30, units = "cm", dpi = 400)
  
  #saving panel c
  ggsave(paste(model.name,"_expc-epc100.png",sep=""), plot = plot, path = "~/senior_thesis/figures", width = 20, height = 10, units = "cm", dpi = 400)
  
}