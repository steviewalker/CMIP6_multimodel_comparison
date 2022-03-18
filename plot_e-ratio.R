#' @title Plot export efficiency 
#' @author Stevie Walker
#' @date 3/8/22

#model.name = "CESM"
#wd = "~/senior_thesis/combined_CESM_files/"
#nc.file = "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
#lat.name = "nlat"
#lon.name = "nlon"

plot_e_ratio <- function(wd, model.name, nc.file, lat.name, lon.name) {
  
  setwd(wd)
  nc_data <- nc_open(nc.file)
  
  lt_100 <- readRDS(paste0("~/senior_thesis/plotting_dataframes/epc100/",model.name,"_epc100_avg_lt.Rds"))
  lt_MLDmax <- readRDS(paste0("~/senior_thesis/plotting_dataframes/expc/",model.name,"_mean_expc_lt.Rds"))
  lt_npp <- readRDS(paste0("~/senior_thesis/plotting_dataframes/NPP/",model.name,"_mean_npp_lt.Rds"))
  
  e_ratio_100 = lt_100/lt_npp
  e_ratio_MLDmax = lt_MLDmax/lt_npp
  change = e_ratio_100 - e_ratio_MLDmax
  
  
  ret <- list()
  ret$lat <- ncvar_get(nc_data, lat.name)
  ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
  ret$e_ratio <- e_ratio_100
  
  melt_npp <- function(L) {
    dimnames(L$e_ratio) <- list(lon = L$nlon, lat = L$nlat)
    ret <- melt(L$e_ratio, value.name = "e_ratio")
  }
  
  melt_e_ratio <- melt_npp(ret)
  
  e_ratio_100 <- ggplot(data = melt_e_ratio, aes(x = lon, y = lat, fill = e_ratio)) + 
    geom_raster(interpolate = TRUE) +
    scale_fill_cmocean(limits = c(0,0.4), oob = squish, name = "deep", direction = 1) +
    #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
    theme_bw() +
    labs(title = "Long-term (2079-2099) average e-ratio at 100m") +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank())
  
  e_ratio_100
  
  # E-ratio at MLDmax ------------
  
  ret <- list()
  ret$lat <- ncvar_get(nc_data, lat.name)
  ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
  ret$e_ratio <- e_ratio_MLDmax
  
  melt_npp <- function(L) {
    dimnames(L$e_ratio) <- list(lon = L$nlon, lat = L$nlat)
    ret <- melt(L$e_ratio, value.name = "e_ratio")
  }
  
  melt_e_ratio <- melt_npp(ret)
  
  e_ratio_MLDmax <- ggplot(data = melt_e_ratio, aes(x = lon, y = lat, fill = e_ratio)) + 
    geom_raster(interpolate = TRUE) +
    scale_fill_cmocean(limits = c(0,0.4), oob = squish, name = "deep", direction = 1) +
    #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
    theme_bw() +
    labs(title = "Long-term (2079-2099) average e-ratio at MLDmax") +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank())
  
  e_ratio_MLDmax
  
  # change between depth horizons ---------------
  
  ret <- list()
  ret$lat <- ncvar_get(nc_data, lat.name)
  ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
  ret$e_ratio <- change
  
  melt_npp <- function(L) {
    dimnames(L$e_ratio) <- list(lon = L$nlon, lat = L$nlat)
    ret <- melt(L$e_ratio, value.name = "e_ratio")
  }
  
  melt_e_ratio <- melt_npp(ret)
  
  e_ratio_diff <- ggplot(data = melt_e_ratio, aes(x = lon, y = lat, fill = e_ratio)) + 
    geom_raster(interpolate = TRUE) +
    scale_fill_cmocean(limits = c(-0.2,0.2), oob = squish, name = "balance", direction = -1) +
    #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
    theme_bw() +
    labs(title = "e-ratio difference (100m - MLDmax)") +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank())
  
  e_ratio_diff
  
  e_ratio_diff_solo <- ggplot(data = melt_e_ratio, aes(x = lon, y = lat, fill = e_ratio)) + 
    geom_raster(interpolate = TRUE) +
    scale_fill_cmocean(limits = c(-0.2,0.2), oob = squish, name = "balance", direction = -1) +
    #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
    theme_bw() +
    labs(title = "e-ratio difference (100m - MLDmax)") +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank())
  
  e_ratio_diff_solo
  
  figure <- ggarrange(e_ratio_100, e_ratio_MLDmax, e_ratio_diff,
                      ncol = 1, nrow = 3)
  figure <- annotate_figure(figure, top = text_grob(model.name, face = "bold", size = 16))
  
  figure2 <- annotate_figure(e_ratio_diff_solo, top = text_grob(model.name, face = "bold", size = 16))
  
  
  ggsave(paste(model.name,"_e-ratio_diff_map.png",sep=""), plot = figure, path = "~/senior_thesis/figures/e-ratio/", width = 22, height = 30, units = "cm", dpi = 400)
  
  #saving panel c
  ggsave(paste(model.name,"_e-ratio_diff_change.png",sep=""), plot = figure2, path = "~/senior_thesis/figures/e-ratio/", width = 20, height = 10, units = "cm", dpi = 400)
  
}