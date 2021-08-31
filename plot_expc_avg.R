#open nc file
setwd("~/senior_thesis/combined_CMCC_files")
nc_data <- nc_open('expc_Oyr_CMCC-ESM2_ssp585_r1i1p1f1_gn_2015-2100.nc')

mean_expc_st <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_mean_expc_st.Rds",sep=""))
mean_expc_lt <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_mean_expc_lt.Rds",sep=""))
expc_change <- readRDS(paste("~/senior_thesis/plotting_dataframes/",model.name,"_expc_change.Rds",sep=""))


## Short-term POC flux --------

ret <- list()
ret$lat <- ncvar_get(nc_data, lat.name)
ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
ret$time <- ncvar_get(nc_data, "time")
ret$expc <- mean_expc_st

#melt data so I can plot it in ggplot
melt_expc <- function(L) {
  dimnames(L$expc) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$expc, value.name = "expc")
}

melt_expc <- melt_expc(ret)


plot1 <- ggplot(data = melt_expc, aes(x = lon, y = lat, fill = expc)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(0,8), oob = squish, name = "deep", direction = 1) +
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = expression(paste("Short-term (2015-2035) POC Flux at MLDmax (mol ",m^-2," ",y^-1,")", sep = ""))) +
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
  scale_fill_cmocean(limits = c(0,8), oob = squish, name = "deep", direction = 1) +
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = expression(paste("Long-term (2078-2098) POC Flux at MLDmax (mol ",m^-2," ",y^-1,")", sep = ""))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank()
  )

plot2

## Change in POC flux ----------


ret <- list()
ret$lat <- ncvar_get(nc_data, lat.name)
ret$lon <- ncvar_get(nc_data, lon.name) # - 360 # we need them as negative values
ret$time <- ncvar_get(nc_data, "time")
ret$expc <- expc_change

#melt data so I can plot it in ggplot
melt_expc <- function(L) {
  dimnames(L$expc) <- list(lon = L$nlon, lat = L$nlat)
  ret <- melt(L$expc, value.name = "expc")
}

melt_expc <- melt_expc(ret)


plot3 <- ggplot(data = melt_expc, aes(x = lon, y = lat, fill = expc)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = -1)+
  #scale_y_continuous(trans = "reverse") + #comment and uncomment this line for MPI
  theme_bw() +
  labs(title = expression(paste("Change in POC Flux at MLDmax (mol ",m^-2," ",y^-1,")", sep = ""))) +
  theme(axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank()
  )

plot3

figure <- ggarrange(plot1, plot2, plot3,
                    ncol = 1, nrow = 3)
figure <- annotate_figure(figure, top = text_grob(model.name, face = "bold", size = 16))

ggsave(paste(model.name,"_expc_global_map.png",sep=""), plot = figure, path = "~/senior_thesis/figures", width = 22, height = 30, units = "cm", dpi = 400)

#saving panel c
ggsave(paste(model.name,"_expc_change.png",sep=""), plot = plot3, path = "~/senior_thesis/figures", width = 20, height = 10, units = "cm", dpi = 400)

