#' @title Plot transfer efficiency between 1000m and 100m depth horizons
#' @author Stevie Walker
#' @date 3/16/22

#model.name = "CESM"
#wd = "~/senior_thesis/combined_CESM_files/"
#nc.file = "pp_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc"
#lat.name = "nlat"
#lon.name = "nlon"

plot_TE <- function(wd, model.name, nc.file, lat.name, lon.name) {
  
  setwd(wd)
  nc_data <- nc_open(nc.file)
  
  lt_100 <- readRDS(paste0("~/senior_thesis/plotting_dataframes/epc100/",model.name,"_epc100_avg_lt.Rds"))
  lt_1000 <- readRDS(paste0("~/senior_thesis/plotting_dataframes/fixed_DH/",model.name,"_1000_mean_expc_lt.Rds"))
  
  TE = (lt_1000/lt_100)*100
  
  ret <- list()
  ret$lat <- ncvar_get(nc_data, lat.name)
  ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
  ret$TE <- TE
  
  melt_TE <- function(L) {
    dimnames(L$TE) <- list(lon = L$nlon, lat = L$nlat)
    ret <- melt(L$TE, value.name = "TE")
  }
  
  melt_TE <- melt_TE(ret)
  
  TE_plot <- ggplot(data = melt_TE, aes(x = lon, y = lat, fill = TE)) + 
    geom_raster(interpolate = TRUE) +
    scale_fill_cmocean(limits = c(0,80), oob = squish, name = "deep", direction = 1) +
    labs(fill = "% transferred") +
    #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
    theme_bw() +
    labs(title = "Long-term (2079-2099) transfer efficiency between 1000m and 100m") +
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(face = "bold"))
  
  TE_plot
  
  ggsave(paste(model.name,"_TE_lt.png",sep=""), plot = TE_plot, path = "~/senior_thesis/figures/TE/", width = 20, height = 10, units = "cm", dpi = 400)
  
  
}