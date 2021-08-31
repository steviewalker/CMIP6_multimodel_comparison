
setwd("~/senior_thesis/combined_CESM_files")
nc_data <- nc_open('expc_Oyr_CESM2_ssp585_r10i1p1f1_gn_2015-2100.nc')

mean_epc100_st <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_epc100_avg_st.Rds",sep=""))
mean_expc_st <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_mean_expc_st.Rds",sep=""))

POC_diff = mean_epc100_st - mean_expc_st

ret <- list()
ret$lat <- ncvar_get(nc_data, lat.name)
ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
ret$expc <- POC_diff

#melt data so I can plot it in ggplot
melt_expc <- function(L) {
  dimnames(L$expc) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$expc, value.name = "expc")
}

melt_expc <- melt_expc(ret)

change <- ggplot(data = melt_expc, aes(x = lon, y = lat, fill = expc)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = -1)+
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = "POC Flux difference, 100m - MLDmax, 2015-2035") +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank())

change

ggsave(paste(model.name,"_POC_diff.png",sep=""), plot = change, path = "~/senior_thesis/figures", width = 20, height = 10, units = "cm", dpi = 400)
